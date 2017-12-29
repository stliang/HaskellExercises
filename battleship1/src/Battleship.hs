{-|
Description : Battleship game - https://en.wikipedia.org/wiki/Battleship_(game)

-}
module Battleship where

import Data.List
import Data.Matrix -- Vector based matrix index starts from 1

data Cell
  = Unchecked
  | CellHit
  | CellMiss
  deriving (Eq, Show)

-- Tracking grid: your target scene
type TargetingGrid = Matrix Cell

-- Primary grid: your ship layout
type PrimaryGrid = Matrix Bool -- TODO: consider data Pcell = ShipType | Island | Water

-- (i,j) coordinate representing i-th row and j-th column numbers
type Coordinate = (Int, Int)

data ShipType
  = Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer
  deriving (Enum, Eq, Show)

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
  = Win
  | Sunk
  | AlreadyTaken
  | Hit
  | Miss

-- Game state at trasition
data State = State
  { targetingGrid :: TargetingGrid
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

-- Gather all the ships positions on a primary grid
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
isNotOccupied :: [Coordinate] -> PrimaryGrid -> Bool
isNotOccupied coords grid = all (\(i, j) -> not $ getElem i j grid) proposed
  where
    proposed = filter (`inBound` grid) $ nub $ neighbourCells coords ++ coords

-- Update primary grid with ship placement
updatePrimaryGrid :: [Coordinate] -> PrimaryGrid -> PrimaryGrid
updatePrimaryGrid xs grid = foldr (setElem True) grid xs

-- Update existing ship on primary grid
updateMyShipRegistery :: ShipType -> [ShipType] -> [ShipType]
updateMyShipRegistery ship ships =
  if ship `elem` ships
    then ships
    else ship : ships

-- Place a new type of ship onto primary grid only if space avaliable
placeShip :: Coordinate -> Direction -> ShipType -> Scene -> Scene
placeShip coord dir ship scene =
  if invalidPlacement
    then scene
    else Scene
           (updatePrimaryGrid proposedShip $ myPrimaryGrid scene)
           (updateMyShipRegistery ship $ myShips scene)
  where
    proposedShip = shipCoordinate coord dir ship
    grid = myPrimaryGrid scene
    isAlreadyPlaced = ship `elem` myShips scene
    isNotInBound = not $ all (`inBound` grid) proposedShip
    isNotAvaliable = not $ isNotOccupied proposedShip grid
    invalidPlacement = isAlreadyPlaced || isNotInBound || isNotAvaliable -- should short circuit

emptyPrimaryGrid :: Int -> Int -> PrimaryGrid
emptyPrimaryGrid r c = matrix r c $ \(i, j) -> False

blankTargetingGrid :: Int -> Int -> TargetingGrid
blankTargetingGrid r c = matrix r c $ \(i, j) -> Unchecked

showGrid
  :: Show a
  => Matrix a -> IO ()
showGrid m = putStrLn $ prettyMatrix m

-- Check if a ship is in a given position
isShipAt :: Coordinate -> PrimaryGrid -> Bool
isShipAt (i, j) grid = getElem i j grid -- NOTE: Might need to case of what a cell means

{-|
consecutiveShipPostion :: Coordinate -> PrimaryGrid -> [Coordinate]
consecutiveShipPostion (i, j) grid = (i, j) `elem` allShips
  where
    allShips = --allShipPositions grid
-}
-- With your opponent's PrimaryGrid, making a move updates your TargetingGrid and the game Condition
attack :: [Coordinate] -> State -> State
attack [] state = state
attack (x:xs) state = attack xs (f x)
  where
    t = targetingGrid state
    o = primaryGrid state
    c = condition state
    updateTargetingGrid (i, j) x = setElem x (i, j) t
    f (i, j) =
      let targetingCell = getElem i j t
          opponentCell = getElem i j o
      in case targetingCell of
           Unchecked ->
             case opponentCell of
               True -> State (updateTargetingGrid (i, j) CellHit) o Hit
               _ -> State (updateTargetingGrid (i, j) CellMiss) o Miss
           CellHit ->
             case opponentCell of
               True -> State t o AlreadyTaken
               _ -> State (updateTargetingGrid (i, j) CellMiss) o Miss
           CellMiss ->
             case opponentCell of
               True -> State (updateTargetingGrid (i, j) CellHit) o Hit
               _ -> State t o Miss
