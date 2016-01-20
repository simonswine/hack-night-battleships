module Battleship.BattleshipSpec (main, spec) where

import Test.Hspec
import Control.Exception
import Battleship.Battleship
import System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "newRandomBattleship" $ do
    it "returns random numbers" $ do
      let ship = fst $ newRandomBattleship 10 5 $ mkStdGen 1328
      (size ship) `shouldBe` 5
      (position ship) `shouldBe` (3,4)
      (horizontal ship) `shouldBe` False
  describe "positionsBattleship" $ do
    it "returns single position for one size ship" $ do
      let ship = newBattleship 1 (0,0) True
      positionsBattleship ship `shouldBe` [(0,0)]
    it "returns single position for 4-size ships horiz" $ do
      let ship = newBattleship 3 (0,0) True
      positionsBattleship ship `shouldBe` [(0,0),(0,1),(0,2)]
    it "returns single position for 4-size ships vert" $ do
      let ship = newBattleship 3 (0,0) False
      positionsBattleship ship `shouldBe` [(0,0),(1,0),(2,0)]
