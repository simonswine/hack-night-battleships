module Battleship.Battleship
  ( newBattleship
  , newRandomBattleship
  , positionsBattleship
  , Battleship(..)
  ) where

import System.Random
import qualified Battleship.Board as Board

type Size = Int
type Position = (Int,Int)

data Battleship = Battleship
  { size       :: Size
  , position   :: Position
  , horizontal :: Bool
  }

positionsBattleship :: Battleship -> [Position]
positionsBattleship Battleship
  { size=s
  , position=(x,y)
  , horizontal=h
  } = case h of
          True -> map (\y' -> (x,y')) [y..y+s-1]
          False -> map (\x' -> (x',y)) [x..x+s-1]

newBattleship :: Size -> Position -> Bool -> Battleship
newBattleship s p h = Battleship
  { size = s
  , position = p
  , horizontal = h
  }

newRandomBattleship :: Board.Size -> Size -> StdGen -> (Battleship, StdGen)
newRandomBattleship boardSize shipSize g = (newBattleship shipSize (x, y) horizontal, g''')
    where (x, g')            = randomR (0, boardSize - shipSize -1) g
          (y, g'')           = randomR (0, boardSize - shipSize -1) g'
          (horizontal, g''') = randomR (True, False) g''
