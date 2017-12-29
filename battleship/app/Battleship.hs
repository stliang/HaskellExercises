{-|
Description : Battleship game - https://en.wikipedia.org/wiki/Battleship_(game)

This Battleship game has a flixable grid size. An example of a grid size is 10 x 10 
where the top left corner position is (1,1), left bottom is (1,10) and right
bottom is (10,10).  The coordinate is represented by a tuple of Int not Int and Char.
Fist Int is "i" or row and the other Int is "j" which is column number. 

The primary grid is your side while the target grid represents
your opponent's primary grid.  You may place a number of ships into the primary
grid only.
-}
module Battleship where

import Data.List
import Data.Matrix -- Vector based matrix index starts from 1

data Cell
  = Unchecked -- Position not been attacked before
  | Hit -- A ship occupying the position, subsequent attacks signal Taken
  | Miss -- No ship occupies the position, subsequent attacks signal Taken

-- Tracking grid: your target scene
type TrackingGrid = Matrix Cell

-- Primary grid: your ship layout
type PrimaryGrid = Matrix Bool -- Ship presence is True otherwise False

-- (i,j) coordinate representing i-th row and j-th column numbers
type Coordinate = (Int, Int)

data ShipType
  = Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer
  deriving (Eq, Show)

type ShipeWidth = Int

type ShipLength = Int

data ShipDim = ShipDim
  { shipWidth :: Int
  , shipLength :: Int
  , shipHeight :: Int
  } deriving (Show)

class ShipSize a where
  dim :: a -> ShipDim

instance ShipSize ShipType where
  dim Carrier = ShipDim 1 5 1
  dim Battleship = ShipDim 1 4 1
  dim Cruiser = ShipDim 1 3 1
  dim Submarine = ShipDim 1 3 1
  dim Destroyer = ShipDim 1 2 1

-- Valid ship placement directions
data Direction
  = TailUp
  | TailDown
  | TailLeft
  | TailRight

-- Game play states
data Condition
  = Won
  | Lost
  | Playing

-- Game state at trasition
data State = State
  { trackingGrid :: TrackingGrid
  , primaryGrid :: PrimaryGrid
  , condition :: Condition
  }

-- A Scene is the primary grid and the ships placed in it.
-- It is a Data struture to hold a built out battle formation before game play
data Scene = Scene
  { myPrimaryGrid :: PrimaryGrid
  , myShips :: [ShipType]
  } deriving (Show)

-- Get the Coordinates of a proposed ship position
shipCoordinate
  :: ShipSize shipType
  => Coordinate -> Direction -> shipType -> [Coordinate]
shipCoordinate (i, j) dir ship =
  case dir of
    TailUp -> goUD negate sl
    TailDown -> goUD id sl
    TailLeft -> goLR negate sl
    TailRight -> goLR id sl
  where
    sl = shipLength (dim ship) - 1 -- Minus one because counting down from shipLength
    goUD :: (Int -> Int) -> Int -> [Coordinate]
    goUD _ 0 = [(i, j)]
    goUD f x = (i + f x, j) : goUD f (x - 1)
    goLR :: (Int -> Int) -> Int -> [Coordinate]
    goLR _ 0 = [(i, j)]
    goLR f x = (i, j + f x) : goLR f (x - 1)

-- Check if a Coordinate is within the bound of a given grid
inBound :: Coordinate -> PrimaryGrid -> Bool
inBound (i, j) grid = i >= 1 && i <= r && j >= 1 && j <= c
  where
    r = nrows grid
    c = ncols grid

-- 
-- O(r * c) complixity as accessing cell is O(1)
allShipPositions :: PrimaryGrid -> [Coordinate]
allShipPositions grid = [(i, j) | i <- [1 .. r], j <- [1 .. c], getElem i j grid]
  where
    r = nrows grid
    c = ncols grid

-- Get the surrounding cells of a given list of coordinates
neighbourCells :: [Coordinate] -> [Coordinate]
neighbourCells = foldr (\x acc -> lr x ++ tb x ++ acc) []
  where
    lr (i, j) = [(i, j - 1), (i, j + 1)]
    tb (i, j) = [(i + 1, j), (i - 1, j)]

-- Check if the proposed site is already occupied
isOccupied :: [Coordinate] -> PrimaryGrid -> Bool
isOccupied coords grid = all (\(i, j) -> not $ getElem i j grid) proposed
  where
    proposed = filter (`inBound` grid) $ nub $ neighbourCells coords ++ coords

-- Update primary grid with ship placement
updatePrimaryGrid :: [Coordinate] -> PrimaryGrid -> PrimaryGrid
-- updatePrimaryGrid [] grid = grid
-- updatePrimaryGrid (x:xs) grid = updatePrimaryGrid xs (setElem True x grid)
--updatePrimaryGrid xs grid = foldl (flip (setElem True)) grid xs
updatePrimaryGrid xs grid = foldr (setElem True) grid xs

-- Update existing ship on primary grid
updateExistingShip :: ShipType -> [ShipType] -> [ShipType]
updateExistingShip ship ships =
  case elem ship ships of
    True -> ships
    _ -> ship : ships

-- Place a new type of ship onto primary grid only if space avaliable
placeShip :: Coordinate -> Direction -> ShipType -> Scene -> Scene
placeShip coord dir ship scene =
  case invalidPlacement of
    True -> scene
    False ->
      Scene
        (updatePrimaryGrid proposedShip $ myPrimaryGrid scene)
        (updateExistingShip ship $ myShips scene)
  where
    proposedShip = shipCoordinate coord dir ship
    grid = myPrimaryGrid scene
    isAlreadyPlaced = elem ship (myShips scene)
    isNotInBound = not $ all (`inBound` grid) proposedShip
    isNotAvaliable = isOccupied proposedShip grid
    invalidPlacement = isAlreadyPlaced || isNotInBound || isNotAvaliable -- should short circuit

-- Test data:
carrier = Carrier

battelship = Battleship

cruiser = Cruiser

submarine = Submarine

destroyer = Destroyer

testTargetingGrid = (matrix 10 10 $ \(i, j) -> Unchecked) :: TrackingGrid

testPrimaryGrid = (matrix 10 10 $ \(i, j) -> False) :: PrimaryGrid

grid1 = setElem True (3, 3) testPrimaryGrid

grid2 = setElem True (3, 4) grid1

grid3 = setElem True (3, 5) grid2

scene = Scene testPrimaryGrid [carrier, cruiser]

scene' = placeShip (1, 1) TailLeft destroyer scene

--placeShip :: Coordinate -> Direction -> ShipType -> Scene -> Scene
scene'' = placeShip (6, 7) TailUp cruiser scene'
