module Main where

import Battleship
import Types

main :: IO ()
main = do
  let scene = Scene (emptyShipGrid 10 10) []
      args =
        [ ((2, 2), TailRight, Carrier)
        , ((2, 8), TailDown, Destroyer)
        , ((5, 5), TailLeft, Submarine)
        , ((7, 9), TailUp, Cruiser)
        , ((7, 4), TailDown, Battleship)
        ]
      eitherScene = foldr (\(c, d, s) (Right scene') -> placeShip c d s scene') (Right scene) args
  case eitherScene of
    (Right scene'') -> showGrid $ myShipGrid scene''
    (Left e) -> putStrLn $ show e
