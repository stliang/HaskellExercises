module BattleshipSpec
  ( spec
  ) where

import Battleship
import Data.Matrix
import TargetingGridTypes as Target
import Test.Hspec
import Types

spec :: SpecWith ()
spec =
  describe "Battleship game" $ do
    it "length of a ship type produces a list of ShipCoordinates of the same length" $ do
      length (shipCoordinate (6, 7) TailLeft Carrier) `shouldBe` 5
    it "in bound check of ship that is out of boundary is quals to False" $ do
      inBound (-1, 1) mkShipGrid `shouldBe` False
    it "ship type Destroyer at (3,3) to the left has [(3,2), (3,3)] coordinate" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (3, 3) TailLeft Destroyer scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 2), (3, 3)]
    it "ship type Submarine at (3,3) to the right has [(3,3), (3,4), (3,5)] coordinate" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (3, 3) TailRight Submarine scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (3, 4), (3, 5)]
    it "ship type Carrier at (3,3) downward has [(3,3), (4,3), (5,3), (6,3), (7,3)] coordinate" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (3, 3) TailDown Carrier scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (4, 3), (5, 3), (6, 3), (7, 3)]
    it "ship type Battleship at (6,3) upward has [(3, 3), (4, 3), (5, 3), (6, 3)] coordinate" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ allShipPositions (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (4, 3), (5, 3), (6, 3)]
    it "ship type Cruiser placed at (3,3) to the left is in the myShips registery" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (3, 3) TailLeft Cruiser scene
            return $ Cruiser `elem` (myShips scene')
      x `shouldBe` Right True
    it "neighbouring cells of (4,2) is [(4,1),(4,3),(5,2),(3,2)]" $ do
      neighbourCells [(4, 2)] `shouldBe` [(4, 1), (4, 3), (5, 2), (3, 2)]
    it "position at (5,6) for a ship placed at (5,5) isNotOccupied is False" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (5, 5) TailLeft Destroyer scene
            let grid = myShipGrid scene'
            return $ isNotOccupied [(5, 6)] grid
      x `shouldBe` Right False
    it "isNotOccupied check on (5,7) for ship placed at (5,5) is True" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (5, 5) TailLeft Destroyer scene
            let grid = myShipGrid scene'
            return $ isNotOccupied [(5, 7)] grid
      x `shouldBe` Right True
    it "updatePrimaryGrid for Destroyer at (5,5) has (5,5) (5,4) set to Destroyer" $ do
      let scene = Scene mkShipGrid []
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
    it
      "place a Carrier at (2,2) tailing right and Destroyer at (2,8) tailing down registery equals [Carrier, Destroyer]" $ do
      let scene = Scene mkShipGrid []
          args = [((2, 2), TailRight, Carrier), ((2, 8), TailDown, Destroyer)]
          x = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            return $ myShips scene''
      x `shouldBe` Right [Carrier, Destroyer]
    it
      "place a Carrier at (2,2) tailing right and Destroyer at (2,7) tailing down equals NotAvaliable" $ do
      let scene = Scene mkShipGrid []
          args = [((2, 2), TailRight, Carrier), ((2, 7), TailDown, Destroyer)]
          x = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            return $ myShips scene''
      x `shouldBe` Left NotAvaliable
    it
      "placing Carrier, Battleship, Cruiser, Submarine, and Destroyer at avaliable space resulting registry of all ships" $ do
      let scene = Scene mkShipGrid []
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
    it "place a Carrier at (2,4) tailing left at out of bound position equals NotInBound" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (2, 4) TailLeft Carrier scene
            return $ myShips scene'
      x `shouldBe` Left NotInBound
    it "new empty ship grid has all cells equal to Nothing" $ do
      let scene = Scene mkShipGrid []
          grid = myShipGrid scene
          x = all (\x -> x == Nothing) grid
      x `shouldBe` True
    it "new empty targeting grid has all cells equal to Unchecked" $ do
      let grid = mkTargetingGrid
          x = all (\x -> x == Unchecked) grid
      True `shouldBe` True
    it "check an existing Battleship placed at (6,3) equals True" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ isShipAt (6, 3) (myShipGrid scene')
      x `shouldBe` Right True
    it "get positions of one Battleship placed at (6,3) equals [(3, 3), (4, 3), (5, 3), (6, 3)]" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ shipPositions Battleship (myShipGrid scene')
      x `shouldBe` Right [(3, 3), (4, 3), (5, 3), (6, 3)]
    it "check if sections of ship got hit on an unharmed Battleship equal False" $ do
      let scene = Scene mkShipGrid []
          targetingGrid = mkTargetingGrid
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ isSunk Battleship (myShipGrid scene') targetingGrid
      x `shouldBe` Right False
    it "check if sections of ship got hit on sunk Battleship equals True" $ do
      let scene = Scene mkShipGrid []
          args =
            [(Target.Hit, (6, 3)), (Target.Hit, (4, 3)), (Target.Hit, (5, 3)), (Target.Hit, (3, 3))]
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            let targetingGrid = mkTargetingGrid
                targetingGrid'' =
                  foldr (\(h, c) targetingGrid' -> setElem h c targetingGrid') targetingGrid args
            return $ isSunk Battleship (myShipGrid scene') targetingGrid''
      x `shouldBe` Right True
    it "get the cell of a Battleship place at (6,3) equals to Just Battleship" $ do
      let scene = Scene mkShipGrid []
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            return $ getShipGridCell (6, 3) (myShipGrid scene')
      x `shouldBe` (Right $ Just Battleship)
    it "isWon function returns True if shipGrid and targetingGrid agree all ships are Hit" $ do
      let scene = Scene mkShipGrid []
          args =
            [(Target.Hit, (6, 3)), (Target.Hit, (4, 3)), (Target.Hit, (5, 3)), (Target.Hit, (3, 3))]
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            let targetingGrid = mkTargetingGrid
                targetingGrid'' =
                  foldr (\(h, c) targetingGrid' -> setElem h c targetingGrid') targetingGrid args
            return $ isWon (myShipGrid scene') targetingGrid''
      x `shouldBe` Right True
    it "attack all positions of a single ship yields its coordinates in targeting grid equal Hit" $ do
      let scene = Scene mkShipGrid []
          tGrid = mkTargetingGrid
          x = do
            scene' <- placeShip (6, 3) TailUp Battleship scene
            let sGrid = myShipGrid scene'
                state = State tGrid sGrid Playing
                targetList = fmap mkTargetCoordinate [(3, 3), (4, 3), (5, 3), (6, 3)]
                state' = foldr (\x acc -> attack x acc) state targetList
                tGrid' = targetingGrid state'
            return $ all (\(TargetCoordinate (i, j)) -> getElem i j tGrid' == Target.Hit) targetList
      x `shouldBe` Right True
    it "when hitting all sections of a ship and the game is not won, the condition equals Sunk" $ do
      let scene = Scene mkShipGrid []
          tGrid = mkTargetingGrid
          args = [((6, 3), TailUp, Battleship), ((1, 1), TailRight, Destroyer)]
          cond = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            let sGrid = myShipGrid scene''
                state = State tGrid sGrid Playing
                targetList = fmap mkTargetCoordinate [(3, 3), (4, 3), (5, 3), (6, 3)]
                state' = foldr (\x acc -> run x acc) state targetList
            return $ condition state'
      cond `shouldBe` Right Sunk
    it "when ships are sunk by attacks, the condition equals Win" $ do
      let scene = Scene mkShipGrid []
          tGrid = mkTargetingGrid
          args = [((6, 3), TailUp, Battleship), ((1, 1), TailRight, Destroyer)]
          cond = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            let sGrid = myShipGrid scene''
                state = State tGrid sGrid Playing
                targetList =
                  fmap mkTargetCoordinate [(3, 3), (4, 3), (5, 3), (6, 3), (1, 1), (1, 2)]
                state' = foldr (\x acc -> run x acc) state targetList
            return $ condition state'
      cond `shouldBe` Right Win
    it "attack the same position produces condition equals AlreadyTaken" $ do
      let scene = Scene mkShipGrid []
          tGrid = mkTargetingGrid
          args = [((6, 3), TailUp, Battleship), ((1, 1), TailRight, Destroyer)]
          cond = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            let sGrid = myShipGrid scene''
                state = State tGrid sGrid Playing
                targetList = fmap mkTargetCoordinate [(6, 3), (6, 3), (4, 3), (5, 3)] -- first two dups because fold right 
                state' = foldr (\x acc -> run x acc) state targetList
            return $ condition state'
      cond `shouldBe` Right AlreadyTaken
    it "attack a position where a ship is placed produces condition equals Hit" $ do
      let scene = Scene mkShipGrid []
          tGrid = mkTargetingGrid
          args = [((6, 3), TailUp, Battleship), ((1, 1), TailRight, Destroyer)]
          cond = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            let sGrid = myShipGrid scene''
                state = State tGrid sGrid Playing
                targetList = fmap mkTargetCoordinate [(4, 3)]
                state' = foldr (\x acc -> run x acc) state targetList
            return $ condition state'
      cond `shouldBe` Right Types.Hit
    it "attack a position where no ship is placed produces condition equals Miss" $ do
      let scene = Scene mkShipGrid []
          tGrid = mkTargetingGrid
          args = [((6, 3), TailUp, Battleship), ((1, 1), TailRight, Destroyer)]
          cond = do
            scene'' <-
              foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
            let sGrid = myShipGrid scene''
                state = State tGrid sGrid Playing
                targetList = fmap mkTargetCoordinate [(10, 3), (7, 9)]
                state' = foldr (\x acc -> run x acc) state targetList
            return $ condition state'
      cond `shouldBe` Right Types.Miss
