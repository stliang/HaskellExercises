module Main where

import Battleship

main :: IO ()
main = do
  let scene = Scene (emptyPrimaryGrid 10 10) []
      scene' = placeShip (2, 2) TailRight Carrier scene
      scene'' = placeShip (2, 8) TailDown Destroyer scene'
      scene''' = placeShip (5, 5) TailLeft Submarine scene''
      scene'''' = placeShip (7, 9) TailUp Cruiser scene'''
      scene''''' = placeShip (7, 4) TailDown Battleship scene''''
  showGrid $ myPrimaryGrid scene'''''
