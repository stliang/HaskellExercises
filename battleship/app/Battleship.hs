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

class Size a where
  size :: a -> (Int, Int)

data ShipType
  = Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer
  deriving (Eq, Show)

instance Size ShipType where
  size Carrier = (1, 5)
  size Battleship = (1, 4)
  size Cruiser = (1, 3)
  size Submarine = (1, 3)
  size Destroyer = (1, 2)

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
  :: Size a
  => a -> Coordinate -> Direction -> [Coordinate]
shipCoordinate ship (i, j) dir =
  case dir of
    TailUp -> go negate shipSize
    TailDown -> go id shipSize
    TailLeft -> go' negate shipSize
    TailRight -> go' id shipSize
  where
    shipSize = snd (size ship) - 1
    go :: (Int -> Int) -> Int -> [Coordinate]
    go _ 0 = (i, j) : []
    go f x = (i + (f x), j) : go f (x - 1)
    go' :: (Int -> Int) -> Int -> [Coordinate]
    go' _ 0 = (i, j) : []
    go' f x = (1, (j + (f x))) : go f (x - 1)

-- Check if a Coordinate is within the bound of a given grid
inBound :: PrimaryGrid -> Coordinate -> Bool
inBound = undefined

-- Get the surounding cells of a given list of coordinates
neighbourCells :: [Coordinate] -> [Coordinate]
neighbourCells = undefined

-- Check if there is a ship located at the given position
isShipAtCoordinate :: PrimaryGrid -> Coordinate -> Bool
isShipAtCoordinate = undefined

placeShip :: ShipType -> Scene -> Coordinate -> Direction -> Scene
placeShip ship scene coord dir =
  case validPlacement of
    True -> scene
    False -> scene
  where
    existingShip = not (elem ship (myShips scene))
    validPlacement :: Bool
    validPlacement = existingShip || False

-- Test data:
carrier = Carrier

battelship = Battleship

cruiser = Cruiser

submarine = Submarine

destroyer = Destroyer

testTargetingGrid = (matrix 10 10 $ \(i, j) -> Unchecked) :: TrackingGrid

testPrimaryGrid = (matrix 10 10 $ \(i, j) -> False) :: PrimaryGrid

scene = Scene testPrimaryGrid [carrier, cruiser]

scene' = placeShip destroyer scene (1, 1) TailLeft
