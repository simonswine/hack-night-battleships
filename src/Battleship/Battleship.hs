module Battleship.Battleship 
  ( battleshipLength
  , newBattleship
  , newRandomBattleship
  ) where

import System.Random

type ShipSize = Int
type BoardSize = Int
type Position = (Int,Int)

data Battleship = Battleship
  { shipSize      :: ShipSize
  , positionA :: Position
  , positionB :: Position
  }

battleshipLength :: Position -> Position -> Int
battleshipLength (x1, y1) (x2, y2) | x1 == x2 && y1 == y2 = 1
                                   | x1 == x2  = abs(y1 - y2) + 1
                                   | y1 == y2  = abs(x1 - x2) + 1

newBattleship :: Position -> Position -> Battleship
newBattleship posA posB = Battleship
  { shipSize = battleshipLength posA posB
  , positionA = posA
  , positionB = posB
  }

newRandomBattleship :: BoardSize -> ShipSize -> StdGen -> Battleship
newRandomBattleship boardSize shipSize g = newBattleship (x, y) (x, y + shipSize)
    where (x, g') = randomR (0, boardSize - shipSize -1) g
          (y, g'') = randomR (0, boardSize - shipSize -1) g'

