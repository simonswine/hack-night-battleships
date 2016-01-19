module Battleship.Battleship 
  ( battleshipLength
  , newBattleship
  ) where

type ShipSize = Int
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
  { shipSize = 0
  , positionA = posA
  , positionB = posB
  }
