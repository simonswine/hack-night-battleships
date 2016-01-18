{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
-- simonswine

import           Conduit
import           Data.Conduit
import           Data.Conduit.Network
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit.TMChan
import           Text.Printf              (printf)
import           Control.Concurrent.STM
import qualified Data.Map as Map
import           Data.Word8               (_cr)
import           Control.Monad
import           Control.Concurrent.Async (concurrently)
import           Control.Exception        (finally)

type ClientName = BS.ByteString

data Client = Client
  { clientName     :: ClientName
  , clientChan     :: TMChan Message
  , clientApp      :: AppData
  }

instance Show Client where
    show client =
        BS.unpack (clientName client) ++ "@"
            ++ show (appSockAddr $ clientApp client)

data Server = Server {
    clients :: TVar (Map.Map ClientName Client)
  , tGame :: TVar Game
}

data Message = Notice BS.ByteString
             | Tell ClientName BS.ByteString
             | Broadcast ClientName BS.ByteString
             | Command BS.ByteString
             deriving Show

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  tGame <- newTVarIO newGame
  return Server { clients = c, tGame = tGame }

newClient :: ClientName -> AppData -> STM Client
newClient name app = do
    chan <- newTMChan
    return Client { clientName     = name
                  , clientApp      = app
                  , clientChan     = chan
                  }

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
    clientmap <- readTVar clients
    mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)


sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTMChan clientChan msg

(<++>) = BS.append

handleMessage :: Server -> Client -> Conduit Message IO BS.ByteString
handleMessage server client@Client{..} = awaitForever $ \case
    Notice msg -> output $ "*** " <++> msg
    Tell name msg      -> output $ "*" <++> name <++> "*: " <++> msg
    Broadcast name msg -> do
      liftIO $ atomically $ do
        game' <- readTVar (tGame server)
        let game'' = game' {currentMessage = msg}
        writeTVar (tGame server) game''
      output $ "<" <++> name <++> ">: " <++> msg
    Command msg        -> case BS.words msg of
        ["/current"] -> do
            game <- liftIO $ atomically $ readTVar (tGame server)
            output $ showGame game clientName
        ["/tell", who, what] -> do
            ok <- liftIO $ atomically $
                sendToName server who $ Tell clientName what
            unless ok $ output $ who <++> " is not connected."
        ["/help"] ->
            mapM_ output [ "------ help -----"
                         , "/tell <who> <what> - send a private message"
                         , "/list - list users online"
                         , "/help - show this message"
                         , "/quit - leave"
                         ]
        ["/list"] -> do
            cl <- liftIO $ atomically $ listClients server
            output $ BS.concat $
                "----- online -----\n" : map ((flip BS.snoc) '\n') cl

        ["/quit"] -> do
            error . BS.unpack $ clientName <++> " has quit"

        -- ignore empty strings
        [""] -> return ()
        [] -> return ()

        -- broadcasts
        ws ->
            if BS.head (head ws) == '/' then
                output $ "Unrecognized command: " <++> msg
            else
                liftIO $ atomically $
                    broadcast server $ Broadcast clientName msg
  where
    output s = yield (s <++> "\n")


listClients :: Server -> STM [ClientName]
listClients Server{..} = do
    c <- readTVar clients
    return $ Map.keys c


sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
    clientmap <- readTVar clients
    case Map.lookup name clientmap of
        Nothing -> return False
        Just client -> sendMessage client msg >> return True


checkAddClient :: Server -> ClientName -> AppData -> IO (Maybe Client)
checkAddClient server@Server{..} name app = atomically $ do
    clientmap <- readTVar clients
    if Map.member name clientmap then
        return Nothing
    else if length clientmap == 2 then
        return Nothing
    else do
        client <- newClient name app
        writeTVar clients $ Map.insert name client clientmap
        broadcast server  $ Notice (name <++> " has connected")
        when (length clientmap == 1) $
          broadcast server  $ Notice "let the game begin!"
        game <- readTVar tGame
        let player = newPlayer name
        let game' =
              if null clientmap then
                game {playerOne = player}
              else
                game {playerTwo = player, activePlayer = name}
        writeTVar tGame game'
        return (Just client)


readName :: Server -> AppData -> ConduitM BS.ByteString BS.ByteString IO Client
readName server app = go
  where
  go = do
    yield "What is your name? "
    name <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
    if BS.null name then
        go
    else do
        ok <- liftIO $ checkAddClient server name app
        case ok of
            Nothing -> do
                respond "The name '%s' is in use, please choose another\n" name
                go
            Just client -> do
                respond "Welcome, %s!\nType /help to list commands.\n" name
                return client
  respond msg name = yield $ BS.pack $ printf msg $ BS.unpack name


clientSink :: Client -> Sink BS.ByteString IO ()
clientSink Client{..} = mapC Command =$ sinkTMChan clientChan True

runClient :: ResumableSource IO BS.ByteString -> Server -> Client -> IO ()
runClient clientSource server client@Client{..} =
    void $ concurrently
        (clientSource $$+- linesUnboundedAsciiC =$ clientSink client)
        (sourceTMChan clientChan
            $$ handleMessage server client
            =$ appSink clientApp)

removeClient :: Server -> Client -> IO ()
removeClient server@Server{..} client@Client{..} = atomically $ do
    modifyTVar' clients $ Map.delete clientName
    broadcast server $ Notice (clientName <++> " has disconnected")

main :: IO ()
main = do
    server <- newServer
    runTCPServer (serverSettings 4000 "*") $ \app -> do
        (fromClient, client) <-
            appSource app $$+ readName server app `fuseUpstream` appSink app
        print client
        (runClient fromClient server client)
            `finally` (removeClient server client)

data Game = Game
  { playerOne :: Player
  , playerTwo :: Player
  , activePlayer :: ClientName
  , currentMessage :: BS.ByteString
  }

data Player = Player
  { board :: Board
  , pName :: ClientName
  }

data Board = Board
  { squares :: Map.Map (Int, Int) Square
  }

data Square = Empty | Hit | Miss | Ship

showSquare :: Square -> Bool -> BS.ByteString
showSquare Empty True = "."
showSquare Empty False = "?"
showSquare Hit _ = "x"
showSquare Miss _ = "o"
showSquare Ship True = "+"
showSquare Ship False = "?"

size = 4

newBoard :: Board
newBoard = Board
  { squares = Map.fromList
    [ ((0,0), Ship)
    , ((0,1), Ship)
    , ((0,2), Empty)
    , ((0,3), Ship)
    , ((1,0), Empty)
    , ((1,1), Empty)
    , ((1,2), Empty)
    , ((1,3), Ship)
    , ((2,0), Empty)
    , ((2,1), Empty)
    , ((2,2), Ship)
    , ((2,3), Empty)
    , ((3,0), Empty)
    , ((3,1), Empty)
    , ((3,2), Ship)
    , ((3,3), Empty)
    ]
  }

newPlayer :: ClientName -> Player
newPlayer name = Player
  { board = newBoard
  , pName = name
  }

newGame :: Game
newGame =
  let playerOne = newPlayer "chris"
  in
    Game
      { playerOne = playerOne
      , playerTwo = newPlayer "matt"
      , activePlayer = ""
      , currentMessage = ""
      }

showGame :: Game -> ClientName -> BS.ByteString
showGame game name =
  if name == pName (playerOne game) then
    BS.intercalate "\n\n"
      [ showBoard (board (playerOne game)) True
      , showBoard (board (playerTwo game)) False
      ]
  else
    BS.intercalate "\n\n"
      [ showBoard (board (playerTwo game)) True
      , showBoard (board (playerOne game)) False
      ]

showBoard :: Board -> Bool -> BS.ByteString
showBoard board isOwner =
  let squareLines =
        map (\i -> map (\j -> case Map.lookup (i,j) (squares board) of
                           Just square -> showSquare square isOwner
                           Nothing -> "?"
                       )
                       [0..size-1])
            [0..size-1]
  in
    BS.intercalate "\n" (map (BS.intercalate " ") squareLines)
