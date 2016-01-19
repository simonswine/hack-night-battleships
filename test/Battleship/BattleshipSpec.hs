module Battleship.BattleshipSpec (main, spec) where

import Test.Hspec
import Control.Exception
import Battleship.Battleship

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "battleshipLength" $ do
    it "returns the correct length for size 1 ships" $ do
      battleshipLength (0, 0) (0, 0) `shouldBe` (1 :: Int)
    it "returns the correct length for size 2 ships" $ do
      battleshipLength (0, 1) (0, 0) `shouldBe` (2 :: Int)
    it "returns the correct length for size 4 ships" $ do
      battleshipLength (1, 1) (1, 4) `shouldBe` (4 :: Int)
    it "returns exception for diagonal ships" $ do
      evaluate(battleshipLength (1, 1) (2, 2)) `shouldThrow` anyException

