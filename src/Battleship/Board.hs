module Battleship.Board
  (Size
  ) where

import qualified Data.Map as Map

type Size = Int

type Board = Map.Map (Int, Int) Square

data Square = Empty | Hit | Miss | Ship deriving Show

newBoard :: Size -> Board
newBoard n = Map.fromList[]
