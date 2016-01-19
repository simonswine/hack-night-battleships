{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Conduit                  (Conduit, ConduitM, ResumableSource,
                                           Sink, awaitForever, filterCE, foldC,
                                           fuseUpstream, liftIO, lineAsciiC,
                                           linesUnboundedAsciiC, mapC, takeCE,
                                           yield, ($$), ($$+), ($$+-), (=$),
                                           (=$=))
import           Control.Concurrent.Async (concurrently)
import           Control.Concurrent.STM   (STM, TVar, atomically, modifyTVar',
                                           newTVarIO, readTVar, writeTVar)
import           Control.Exception        (finally)
import           Control.Monad            (unless, void, when)
import qualified Data.ByteString.Char8    as BS
import           Data.Conduit.Network     (AppData, appSink, appSockAddr,
                                           appSource, runTCPServer,
                                           serverSettings)
import           Data.Conduit.TMChan      (TMChan, newTMChan, sinkTMChan,
                                           sourceTMChan, writeTMChan)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import           Data.Word8               (_cr)
import           Text.Printf              (printf)

type ClientName = BS.ByteString

data Client = Client
  { clientName :: ClientName
  , clientChan :: TMChan Message
  , clientApp  :: AppData
  }

instance Show Client where
    show client =
        BS.unpack (clientName client) ++ "@"
            ++ show (appSockAddr $ clientApp client)

data Server = Server {
    clients :: TVar (Map.Map ClientName Client)
  , tGame   :: TVar Game
}

data Message = Notice BS.ByteString
             | Tell ClientName BS.ByteString
             | Broadcast ClientName BS.ByteString
             | BroadcastGame
             | Command BS.ByteString
             deriving Show

data Game = Game
  { gamePlayers      :: Map.Map ClientName Player
  , gameActivePlayer :: Maybe ClientName
  } deriving Show

data Player = Player
  { playerBoard :: Board
  } deriving Show

type Board = Map.Map (Int, Int) Square

data Square = Empty | Hit | Miss | Ship deriving Show

data FireResult = FHit | FMiss

data Error = NotYourTurn | BadInput

type Result = Either Error FireResult

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
    mapM_ (`sendMessage` msg) (Map.elems clientmap)

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTMChan clientChan

handleMessage :: Server -> Client -> Conduit Message IO BS.ByteString
handleMessage server Client{..} = awaitForever $ \case
    Notice msg -> output $ "*** " <> msg
    Tell name msg      -> output $ "*" <> name <> "*: " <> msg
    Broadcast name msg ->
      output $ "<" <> name <> ">: " <> msg
    BroadcastGame -> do
      game <- liftIO $ atomically $ readTVar (tGame server)
      case gameActivePlayer game of
        Just player -> do
          output $ "To play: " <> player
          output $ showGame game clientName
        Nothing ->
          output "Game not started yet."
    Command msg        -> case BS.words msg of
        ["/fire", x, y] ->
          handleFireCommand server clientName x y
        ["/debug"] -> do
            game <- liftIO $ atomically $ readTVar (tGame server)
            output $ BS.pack (show game)
        ["/tell", who, what] -> do
            ok <- liftIO $ atomically $
                sendToName server who $ Tell clientName what
            unless ok $ output $ who <> " is not connected."
        ["/help"] ->
            mapM_ output [ "------ help -----"
                         , "/fire <x> <y> - fire a missile"
                         , "/tell <who> <what> - send a private message"
                         , "/list - list users online"
                         , "/help - show this message"
                         , "/quit - leave"
                         ]
        ["/list"] -> do
            cl <- liftIO $ atomically $ listClients server
            output $ BS.concat $
                "----- online -----\n" : map (`BS.snoc` '\n') cl

        ["/quit"] ->
            error . BS.unpack $ clientName <> " has quit"

        -- ignore empty strings
        [""] -> return ()
        [] -> return ()

        -- broadcasts
        ws ->
            if BS.head (head ws) == '/' then
                output $ "Unrecognized command: " <> msg
            else
                liftIO $ atomically $
                    broadcast server $ Broadcast clientName msg
  where
    output s = yield (s <> "\n")

handleFireCommand
  :: Server
     -> BS.ByteString
     -> BS.ByteString
     -> BS.ByteString
     -> Conduit Message IO BS.ByteString
handleFireCommand server clientName x y = do
  result <- liftIO $ atomically $ do
    game <- readTVar (tGame server)
    let (res, game') = fire game clientName x y
    writeTVar (tGame server) game'
    return res
  case result of
    Left err ->
      case err of
        NotYourTurn -> output "Wait your turn!"
        BadInput -> output $ "Can't fire on " <> x <> " " <> y
    Right fResult ->
        let
          msg =
            case fResult of
              FHit -> "It's a hit!"
              FMiss -> "Missed!"
        in
          liftIO $ atomically $ do
            broadcast server $ Notice $
              clientName <> " fired at (" <> x <> ", " <> y <> "). " <> msg
            broadcast server BroadcastGame
  where
    output s = yield (s <> "\n")

listClients :: Server -> STM [ClientName]
listClients Server{..} = do
    c <- readTVar clients
    return $ Map.keys c

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName Server{..} name msg = do
    clientmap <- readTVar clients
    case Map.lookup name clientmap of
        Nothing -> return False
        Just client -> sendMessage client msg >> return True

checkAddClient :: Server -> ClientName -> AppData -> IO (Maybe Client)
checkAddClient server@Server{..} name app = atomically $ do
    clientmap <- readTVar clients
    if Map.member name clientmap || length clientmap == 2 then
        return Nothing
    else do
        client <- newClient name app
        writeTVar clients $ Map.insert name client clientmap
        broadcast server  $ Notice (name <> " has connected")
        when (length clientmap == 1) $ do
          broadcast server $ Notice "let the game begin!"
          broadcast server BroadcastGame
        game <- readTVar tGame
        let game' = game { gamePlayers = Map.insert name newPlayer (gamePlayers game)
                         , gameActivePlayer = if length clientmap == 1 then Just name else Nothing}
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
removeClient server@Server{..} Client{..} = atomically $ do
    modifyTVar' clients $ Map.delete clientName
    broadcast server $ Notice (clientName <> " has disconnected")

main :: IO ()
main = do
    server <- newServer
    runTCPServer (serverSettings 4000 "*") $ \app -> do
        (fromClient, client) <-
            appSource app $$+ readName server app `fuseUpstream` appSink app
        print client
        runClient fromClient server client
            `finally` removeClient server client

showSquare :: Square -> Bool -> BS.ByteString
showSquare Empty True = "."
showSquare Empty False = "?"
showSquare Hit _ = "x"
showSquare Miss _ = "o"
showSquare Ship True = "+"
showSquare Ship False = "?"

boardSize :: Int
boardSize = 4

newBoard :: Board
newBoard =
  Map.fromList
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

newPlayer :: Player
newPlayer = Player
  { playerBoard = newBoard
  }

newGame :: Game
newGame =
  Game
    { gamePlayers = Map.empty
    , gameActivePlayer = Nothing
    }

getOtherPlayerName :: ClientName -> Game -> ClientName
getOtherPlayerName myName game =
  head [n | n <- Map.keys (gamePlayers game), n /= myName ]

getPlayerBoard :: ClientName -> Game -> Board
getPlayerBoard name game =
  playerBoard (gamePlayers game Map.! name)

setPlayerBoard :: ClientName -> Game -> Board -> Game
setPlayerBoard name game board =
  let
    players = gamePlayers game
    player = players Map.! name
  in
    game { gamePlayers = Map.insert name (player {playerBoard = board}) players}

showGame :: Game -> ClientName -> BS.ByteString
showGame game myName =
  let
    myBoard = playerBoard (gamePlayers game Map.! myName)
    otherName = getOtherPlayerName myName game
    otherBoard = getPlayerBoard otherName game
  in
    BS.intercalate "\n\n"
      [ showBoard myBoard True
      , showBoard otherBoard False
      ]

showBoard :: Board -> Bool -> BS.ByteString
showBoard board isOwner =
  let squareLines =
        map (\i -> map (\j -> case Map.lookup (i,j) board of
                           Just square -> showSquare square isOwner
                           Nothing -> "???"
                       )
                       [0..boardSize-1])
            [0..boardSize-1]
  in
    BS.intercalate "\n" (map (BS.intercalate " ") squareLines)

switchPlayer :: Game -> Game
switchPlayer game =
  let
    getInactivePlayer activePlayer =
      head [name | name <- Map.keys (gamePlayers game), name /= activePlayer]
    newActivePlayer = fmap getInactivePlayer (gameActivePlayer game)
  in
    game { gameActivePlayer = newActivePlayer }

fire :: Game -> ClientName -> BS.ByteString -> BS.ByteString -> (Result, Game)
fire game playerName x y =
    case gameActivePlayer game of
      Nothing ->
        (Left NotYourTurn, game)
      Just activePlayer ->
        if playerName /= activePlayer then
          (Left NotYourTurn, game)
        else
          let
            parsed = parseCoords x y
          in
            case parsed of
              Nothing ->
                (Left BadInput, game)
              Just coords ->
                let
                  otherName = getOtherPlayerName playerName game
                  (result, board) = fireOnBoard (getPlayerBoard otherName game) coords
                  game' = setPlayerBoard otherName game board
                  game'' = switchPlayer game'
                in
                  (Right result, game'')

parseCoords :: BS.ByteString -> BS.ByteString -> Maybe (Int, Int)
parseCoords xStr yStr = do
  (x, _) <- BS.readInt xStr
  (y, _) <- BS.readInt yStr
  if x > 0 && x <= boardSize && y > 0 && y <= boardSize then
    Just (x-1, y-1)
  else
    Nothing


fireOnBoard :: Board -> (Int, Int) -> (FireResult, Board)
fireOnBoard board coords =
  let
    square = board Map.! coords
  in
    case square of
      Ship -> (FHit, Map.insert coords Hit board)
      Empty -> (FMiss, Map.insert coords Miss board)
      Hit -> (FHit, board)
      Miss -> (FMiss, board)
