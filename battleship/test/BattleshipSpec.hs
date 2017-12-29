{-# LANGUAGE OverloadedStrings #-}

module BattleshipSpec
  ( spec
  ) where

import Battleship
import Data.Matrix
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Battleship game" $ do
    it "the length of shipCoordinate is equal to length of the ship " $ do
      length (shipCoordinate (6, 7) TailLeft Carrier) `shouldBe` 5
    it "in bound check of ship that is out of boundary is qual to False" $ do
      inBound (-1, 1) (emptyPrimaryGrid 10 10) `shouldBe` False
    it "a Destroyer at (3,3) to the left has [(3,2), (3,3)] coordinate" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (3, 3) TailLeft Destroyer scene
      allShipPositions (myPrimaryGrid scene') `shouldBe` [(3, 2), (3, 3)]
    it "a Submarine at (3,3) to the right has [(3,3), (3,4), (3,5)] coordinate" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (3, 3) TailRight Submarine scene
      allShipPositions (myPrimaryGrid scene') `shouldBe` [(3, 3), (3, 4), (3, 5)]
    it "a Carrier at (3,3) downward has [(3,3), (4,3), (5,3), (6,3), (7,3)] coordinate" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (3, 3) TailDown Carrier scene
      allShipPositions (myPrimaryGrid scene') `shouldBe` [(3, 3), (4, 3), (5, 3), (6, 3), (7, 3)]
    it "a Battleship at (6,3) upward has [(3, 3), (4, 3), (5, 3), (6, 3)] coordinate" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
      allShipPositions (myPrimaryGrid scene') `shouldBe` [(3, 3), (4, 3), (5, 3), (6, 3)]
    it "a Cruiser placed at (3,3) to the left is in the myShips registery" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (3, 3) TailLeft Cruiser scene
      Cruiser `elem` (myShips scene') `shouldBe` True
    it "the neighbouring cells of (4,2) is [(4,1),(4,3),(5,2),(3,2)]" $ do
      neighbourCells [(4, 2)] `shouldBe` [(4, 1), (4, 3), (5, 2), (3, 2)]
    it "ship requires extra one unit space, isNotOccupied check on (5,6) for shit at (5,5) is False" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (5, 5) TailLeft Destroyer scene
          grid = myPrimaryGrid scene'
      isNotOccupied [(5, 6)] grid `shouldBe` False
    it "isNotOccupied check on (5,7) for shit at (5,5) is True" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (5, 5) TailLeft Destroyer scene
          grid = myPrimaryGrid scene'
      isNotOccupied [(5, 7)] grid `shouldBe` True
    it "updatePrimaryGrid for Destroyer at (5,5) has (5,5) (5,4) set to True" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          grid = myPrimaryGrid scene
          proposedShip = shipCoordinate (5, 5) TailLeft Destroyer
          grid' = updatePrimaryGrid proposedShip grid
          b1 = getElem 5 5 grid'
          b2 = getElem 5 4 grid'
      b1 && b2 `shouldBe` True
    it "when a ship already added, the ship registery should not change" $ do
      let shipRegistery = [Destroyer, Cruiser]
          newShipRegistery = updateMyShipRegistery Cruiser shipRegistery
      shipRegistery `shouldBe` newShipRegistery
    it "adding Battleship to registery [Destroyer, Cruiser] equals [Battleship, Destroyer, Cruiser]" $ do
      let shipRegistery = [Destroyer, Cruiser]
          newShipRegistery = updateMyShipRegistery Battleship shipRegistery
      [Battleship, Destroyer, Cruiser] `shouldBe` newShipRegistery
    it "place a Carrier at (2,2) tailing right and Destroyer at (2,8) tailing down is ok" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (2, 2) TailRight Carrier scene
          scene'' = placeShip (2, 8) TailDown Destroyer scene'
          shipRegistery = myShips scene''
      shipRegistery `shouldBe` [Destroyer, Carrier]
    it "place a Carrier at (2,2) tailing right and Destroyer at (2,7) tailing down is not ok" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (2, 2) TailRight Carrier scene
          scene'' = placeShip (2, 7) TailDown Destroyer scene'
          shipRegistery = myShips scene''
      shipRegistery `shouldBe` [Carrier]
    it "placing Carrier, Battleship, Cruiser, Submarine, and Destroyer at avaliable space is ok" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (2, 2) TailRight Carrier scene
          scene'' = placeShip (2, 8) TailDown Destroyer scene'
          scene''' = placeShip (5, 5) TailLeft Submarine scene''
          scene'''' = placeShip (7, 9) TailUp Cruiser scene'''
          scene''''' = placeShip (7, 4) TailDown Battleship scene''''
          shipRegistery = myShips scene'''''
      shipRegistery `shouldBe` [Battleship, Cruiser, Submarine, Destroyer, Carrier]
    it "place a Carrier at (2,4) tailing left in out of bound position is not ok" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (2, 4) TailLeft Carrier scene
          shipRegistery = myShips scene'
      shipRegistery `shouldBe` []
    it "a new empty primary grid has all cells equal to False" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          grid = myPrimaryGrid scene
          listCell = toList grid
      foldr (||) False listCell `shouldBe` False
    it "a new empty targeting grid has all cells equal to Unchecked" $ do
      let grid = blankTrackingGrid 10 10
          listCell =
            fmap
              (\x ->
                 if x == Unchecked
                   then False
                   else True) $
            toList grid
      foldr (||) False listCell `shouldBe` False
