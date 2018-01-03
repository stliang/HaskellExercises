module Main where

import Battleship
import Types

main :: IO ()
main = do
  let scene = Scene mkShipGrid []
      -- Declare ship placements
      args =
        [ ((2, 2), TailRight, Carrier)
        , ((2, 8), TailDown, Destroyer)
        , ((5, 5), TailLeft, Submarine)
        , ((7, 9), TailUp, Cruiser)
        , ((7, 4), TailDown, Battleship)
        ]
      -- Declare ship targets
      carrirerTargetList = fmap mkTargetCoordinate [(2, 2), (2, 3), (2, 4), (2, 5), (2, 6)]
      destroyerTargetList = fmap mkTargetCoordinate [(2, 8), (3, 8)]
      submarineTargetList = fmap mkTargetCoordinate [(5, 3), (5, 4), (5, 5)]
      battleshipTargetList = fmap mkTargetCoordinate [(7, 4), (8, 4), (9, 4), (10, 4)]
      cruiserTargetList = fmap mkTargetCoordinate [(5, 9), (6, 9), (7, 9)]
      allShipTargets =
        carrirerTargetList ++
        destroyerTargetList ++ submarineTargetList ++ battleshipTargetList ++ cruiserTargetList
      -- Place all ships in a scene
      eScene = foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
      -- Attack all ships
      eCond = do
        scene'' <- eScene
        let sGrid = myShipGrid scene''
            tGrid = mkTargetingGrid
            state = State tGrid sGrid Playing
            state' = foldr (\x acc -> run x acc) state allShipTargets
        return $ condition state'
  -- Print out ship grid
  case eScene of
    (Right s) -> showGrid $ myShipGrid s
    (Left e) -> putStrLn $ "error: " ++ (show e)
  -- Print out condition of the game
  case eCond of
    (Right c) -> putStrLn (show c)
    (Left e) -> putStrLn $ "error: " ++ (show e)
