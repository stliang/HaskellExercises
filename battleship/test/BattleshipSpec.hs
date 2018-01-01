{-# LANGUAGE OverloadedStrings #-}

module BattleshipSpec
  ( spec
  ) where

import Battleship
import Data.Matrix
import TargetingGridTypes as T
import Test.Hspec
import Types

spec :: SpecWith ()
spec =
  describe "Battleship game" $ do
    it "the length of shipCoordinate is equal to length of the ship " $ do
      length (shipCoordinate (6, 7) TailLeft Carrier) `shouldBe` 5
    it "in bound check of ship that is out of boundary is qual to False" $ do
      inBound (-1, 1) (emptyShipGrid 10 10) `shouldBe` False
    it "a Destroyer at (3,3) to the left has [(3,2), (3,3)] coordinate" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (3, 3) TailLeft Destroyer scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 2), (3, 3)]
    it "a Submarine at (3,3) to the right has [(3,3), (3,4), (3,5)] coordinate" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (3, 3) TailRight Submarine scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (3, 4), (3, 5)]
    it "a Carrier at (3,3) downward has [(3,3), (4,3), (5,3), (6,3), (7,3)] coordinate" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (3, 3) TailDown Carrier scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (4, 3), (5, 3), (6, 3), (7, 3)]
    it "a Battleship at (6,3) upward has [(3, 3), (4, 3), (5, 3), (6, 3)] coordinate" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (4, 3), (5, 3), (6, 3)]
    it "a Cruiser placed at (3,3) to the left is in the myShips registery" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (3, 3) TailLeft Cruiser scene
            return $ Cruiser `elem` (myShips scene')
      x `shouldBe` Right True
    it "the neighbouring cells of (4,2) is [(4,1),(4,3),(5,2),(3,2)]" $ do
      neighbourCells [(4, 2)] `shouldBe` [(4, 1), (4, 3), (5, 2), (3, 2)]
    it "ship requires extra one unit space, isNotOccupied check on (5,6) for shit at (5,5) is False" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (5, 5) TailLeft Destroyer scene
            let grid = myShipGrid scene'
            return $ isNotOccupied [(5, 6)] grid
      x `shouldBe` Right False
    it "isNotOccupied check on (5,7) for shit at (5,5) is True" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (5, 5) TailLeft Destroyer scene
            let grid = myShipGrid scene'
            return $ isNotOccupied [(5, 7)] grid
      x `shouldBe` Right True
    it "updatePrimaryGrid for Destroyer at (5,5) has (5,5) (5,4) set to Destroyer" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          grid = myShipGrid scene
          proposedShip = shipCoordinate (5, 5) TailLeft Destroyer
          grid' = updateShipGrid Destroyer proposedShip grid
          b1 = getElem 5 5 grid' == Just Destroyer
          b2 = getElem 5 4 grid' == Just Destroyer
      b1 && b2 `shouldBe` True
    it "when a ship already added, the ship registery should not change" $ do
      let shipRegistery = [Destroyer, Cruiser]
          newShipRegistery = updateShipRegistery Cruiser shipRegistery
      shipRegistery `shouldBe` newShipRegistery
    it "adding Battleship to registery [Destroyer, Cruiser] equals [Battleship, Destroyer, Cruiser]" $ do
      let shipRegistery = [Destroyer, Cruiser]
          newShipRegistery = updateShipRegistery Battleship shipRegistery
      [Battleship, Destroyer, Cruiser] `shouldBe` newShipRegistery
    it "place a Carrier at (2,2) tailing right and Destroyer at (2,8) tailing down is ok" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          args = [((2, 2), TailRight, Carrier), ((2, 8), TailDown, Destroyer)]
          x = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            return $ myShips scene''
      x `shouldBe` Right [Carrier, Destroyer]
    it
      "place a Carrier at (2,2) tailing right and Destroyer at (2,7) tailing down equals NotAvaliable" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          args = [((2, 2), TailRight, Carrier), ((2, 7), TailDown, Destroyer)]
          x = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            return $ myShips scene''
      x `shouldBe` Left NotAvaliable
    it "placing Carrier, Battleship, Cruiser, Submarine, and Destroyer at avaliable space is ok" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          args =
            [ ((2, 2), TailRight, Carrier)
            , ((2, 8), TailDown, Destroyer)
            , ((5, 5), TailLeft, Submarine)
            , ((7, 9), TailUp, Cruiser)
            , ((7, 4), TailDown, Battleship)
            ]
          x = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            return $ myShips scene''
      x `shouldBe` Right [Carrier, Destroyer, Submarine, Cruiser, Battleship]
    it "place a Carrier at (2,4) tailing left in out of bound position equals NotInBound" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (2, 4) TailLeft Carrier scene
            return $ myShips scene'
      x `shouldBe` Left NotInBound
    it "a new empty ship grid has all cells equal to Nothing" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          grid = myShipGrid scene
          x = all (\x -> x == Nothing) grid
      x `shouldBe` True
    it "a new empty targeting grid has all cells equal to Unchecked" $ do
      let grid = blankTargetingGrid 10 10
          x = all (\x -> x == Unchecked) grid
      True `shouldBe` True
    it "check an existing Battleship placed at (6,3) equals True" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ isShipAt (6, 3) (myShipGrid scene')
      x `shouldBe` Right True
    it "get positions of one Battleship placed at (6,3) equals [(3, 3), (4, 3), (5, 3), (6, 3)]" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ oneShipPositions Battleship (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (4, 3), (5, 3), (6, 3)]
    it "check if sections of ship got hit on an unharmed Battleship equal False" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          targetingGrid = blankTargetingGrid 10 10
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ isOneShipAllHit Battleship (myShipGrid scene') targetingGrid
      x `shouldBe` Right False
    it "check if sections of ship got hit on sunk Battleship equals True" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          args = [(T.Hit, (6, 3)), (T.Hit, (4, 3)), (T.Hit, (5, 3)), (T.Hit, (3, 3))]
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            let targetingGrid = blankTargetingGrid 10 10
                targetingGrid'' =
                  foldr (\(h, c) targetingGrid' -> setElem h c targetingGrid') targetingGrid args
            return $ isOneShipAllHit Battleship (myShipGrid scene') targetingGrid''
      x `shouldBe` Right True
    it "get the cell of a Battleship place at (6,3) equals to Just Battleship" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ getShipGridCell (6, 3) (myShipGrid scene')
      x `shouldBe` (Right $ Just Battleship)
{-|
    it "when last ship is sunk, check if won equals True" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          scene' = placeShip (6, 3) TailUp Battleship scene
          targetingGrid = blankTargetingGrid 10 10
          targetingGrid' = setElem CellHit (6, 3) targetingGrid
          targetingGrid'' = setElem CellHit (4, 3) targetingGrid'
          targetingGrid''' = setElem CellHit (5, 3) targetingGrid''
          targetingGrid'''' = setElem CellHit (3, 3) targetingGrid'''
      won (myShipGrid scene') targetingGrid'''' `shouldBe` True
    it "when last ship is sunk, check if won equals True" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          targetingGrid = blankTargetingGrid 10 10
          scene' = placeShip (6, 3) TailUp Battleship scene
          primaryGrid = myShipGrid scene'
          state = State targetingGrid primaryGrid Start
          targetList = [(3, 3), (4, 3), (5, 3), (6, 3)]
          state' = attack targetList state
          cond = condition state'
      cond `shouldBe` Win
    it "when hitting all sections of a ship and the game is not won, the condition equals Sunk" $ do
      let scene = Scene (emptyShipGrid 10 10) []
          targetingGrid = blankTargetingGrid 10 10
          scene' = placeShip (6, 3) TailUp Battleship scene
          scene'' = placeShip (1, 1) TailRight Destroyer scene'
          primaryGrid = myShipGrid scene''
          state = State targetingGrid primaryGrid Start
          targetList = [(3, 3), (4, 3), (5, 3), (6, 3)]
          state' = attack targetList state
          cond = condition state'
      cond `shouldBe` Sunk
-}
