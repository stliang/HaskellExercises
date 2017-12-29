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
    it "updatePrimaryGrid for Destroyer at (5,5) has (5,5) (5,4) set to Destroyer" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          grid = myPrimaryGrid scene
          proposedShip = shipCoordinate (5, 5) TailLeft Destroyer
          grid' = updatePrimaryGrid Destroyer proposedShip grid
          b1 = getElem 5 5 grid' == Destroyer
          b2 = getElem 5 4 grid' == Destroyer
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
          listCell =
            fmap
              (\x ->
                 if x == Water
                   then False
                   else True) $
            toList grid
      foldr (||) False listCell `shouldBe` False
    it "a new empty targeting grid has all cells equal to Unchecked" $ do
      let grid = blankTargetingGrid 10 10
          listCell =
            fmap
              (\x ->
                 if x == Unchecked
                   then False
                   else True) $
            toList grid
      foldr (||) False listCell `shouldBe` False
    it "check an existing Battleship placed at (6,3) equals True" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
      isShipAt (6, 3) (myPrimaryGrid scene') `shouldBe` True
    it "get positions of one Battleship placed at (6,3) equals [(3, 3), (4, 3), (5, 3), (6, 3)]" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
      oneShipPositions Battleship (myPrimaryGrid scene') `shouldBe` [(3, 3), (4, 3), (5, 3), (6, 3)]
    it "check if sections of ship got hit on an unharmed Battleship equa False" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          targetingGrid = blankTargetingGrid 10 10
          scene' = placeShip (6, 3) TailUp Battleship scene
      isOneShipAllHit Battleship (myPrimaryGrid scene') targetingGrid `shouldBe` False
    it "check if sections of ship got hit on sunk Battleship equals True" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
          targetingGrid = blankTargetingGrid 10 10
          targetingGrid' = setElem CellHit (6, 3) targetingGrid
          targetingGrid'' = setElem CellHit (4, 3) targetingGrid'
          targetingGrid''' = setElem CellHit (5, 3) targetingGrid''
          targetingGrid'''' = setElem CellHit (3, 3) targetingGrid'''
      isOneShipAllHit Battleship (myPrimaryGrid scene') targetingGrid'''' `shouldBe` True
    it "get the shiptype of a Battleship place at (6,3) equals to Battleship" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
      getPrimaryCellType (6, 3) (myPrimaryGrid scene') `shouldBe` Battleship
    it "when last ship is sunk, check if won equals True" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
          targetingGrid = blankTargetingGrid 10 10
          targetingGrid' = setElem CellHit (6, 3) targetingGrid
          targetingGrid'' = setElem CellHit (4, 3) targetingGrid'
          targetingGrid''' = setElem CellHit (5, 3) targetingGrid''
          targetingGrid'''' = setElem CellHit (3, 3) targetingGrid'''
      won (myPrimaryGrid scene') targetingGrid'''' `shouldBe` True
    it "when last ship is sunk, check if won equals True" $ do
      let scene = Scene (emptyPrimaryGrid 10 10) []
          targetingGrid = blankTargetingGrid 10 10
          scene' = placeShip (6, 3) TailUp Battleship scene
          primaryGrid = myPrimaryGrid scene'
          state = State targetingGrid primaryGrid Start
          targetList = [(3, 3), (4, 3), (5, 3), (6, 3)]
          state' = attack targetList state
          cond = condition state'
      cond `shouldBe` Win
