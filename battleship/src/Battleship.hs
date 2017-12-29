{-|
Description : Battleship game - https://en.wikipedia.org/wiki/Battleship_(game)

-}
module Battleship where

import Data.List
import Data.Matrix -- Vector based matrix index starts from 1

data TargetingCell
  = Unchecked
  | CellHit
  | CellMiss
  deriving (Eq, Show)

-- Tracking grid: your target scene
type TargetingGrid = Matrix TargetingCell

-- Primary grid: your ship layout
type PrimaryGrid = Matrix PrimaryCell

-- (i,j) coordinate representing i-th row and j-th column numbers
type Coordinate = (Int, Int)

data PrimaryCell
  = Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer
  | Island
  | Water
  deriving (Enum, Eq, Show)

listOfShipType = [Carrier, Battleship, Cruiser, Submarine, Destroyer]

type ShipeWidth = Int

type ShipLength = Int

data Dim = Dim
  { getWidth :: Int
  , getLength :: Int
  , getHeight :: Int
  } deriving (Show)

class Size a where
  dim :: a -> Dim

instance Size PrimaryCell where
  dim Carrier = Dim 1 5 1
  dim Battleship = Dim 1 4 1
  dim Cruiser = Dim 1 3 1
  dim Submarine = Dim 1 3 1
  dim Destroyer = Dim 1 2 1
  dim Island = Dim 1 1 1
  dim Water = Dim 10 10 1

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
  | Start
  deriving (Eq, Show)

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
  , myShips :: [PrimaryCell]
  } deriving (Show)

-- Get the Coordinates of a proposed ship position
-- TODO shipType should be object with size
shipCoordinate
  :: Size shipType
  => Coordinate -> Direction -> shipType -> [Coordinate]
shipCoordinate (i, j) dir ship =
  case dir of
    TailUp -> goUD negate sl
    TailDown -> goUD id sl
    TailLeft -> goLR negate sl
    TailRight -> goLR id sl
  where
    sl = getLength (dim ship) - 1 -- Minus one because counting down from shipLength
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
-- O(r * c * listOfShipType) complixity as accessing cell is O(1)
allShipPositions :: PrimaryGrid -> [Coordinate]
allShipPositions grid =
  [(i, j) | i <- [1 .. r], j <- [1 .. c], (getElem i j grid) `elem` listOfShipType]
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
isNotOccupied coords grid = all (\(i, j) -> not $ (getElem i j grid) `elem` listOfShipType) proposed
  where
    proposed = filter (`inBound` grid) $ nub $ neighbourCells coords ++ coords

-- Update primary grid with ship placement
updatePrimaryGrid :: PrimaryCell -> [Coordinate] -> PrimaryGrid -> PrimaryGrid
updatePrimaryGrid primaryCell xs grid = foldr (setElem primaryCell) grid xs

-- Update existing ship on primary grid
updateMyShipRegistery :: PrimaryCell -> [PrimaryCell] -> [PrimaryCell]
updateMyShipRegistery ship ships =
  if ship `elem` ships
    then ships
    else ship : ships

-- Place a new type of ship onto primary grid only if space avaliable
-- TODO PrimaryCell ambiguous rethink data structure
placeShip :: Coordinate -> Direction -> PrimaryCell -> Scene -> Scene
placeShip coord dir ship scene =
  if invalidPlacement
    then scene
    else Scene
           (updatePrimaryGrid ship proposedShip $ myPrimaryGrid scene)
           (updateMyShipRegistery ship $ myShips scene)
  where
    proposedShip = shipCoordinate coord dir ship
    grid = myPrimaryGrid scene
    isAlreadyPlaced = ship `elem` myShips scene
    isNotInBound = not $ all (`inBound` grid) proposedShip
    isNotAvaliable = not $ isNotOccupied proposedShip grid
    invalidPlacement = isAlreadyPlaced || isNotInBound || isNotAvaliable -- should short circuit

emptyPrimaryGrid :: Int -> Int -> PrimaryGrid
emptyPrimaryGrid r c = matrix r c $ \(i, j) -> Water

blankTargetingGrid :: Int -> Int -> TargetingGrid
blankTargetingGrid r c = matrix r c $ \(i, j) -> Unchecked

showGrid
  :: Show a
  => Matrix a -> IO ()
showGrid m = putStrLn $ prettyMatrix m

-- Check if a ship is in a given position
isShipAt :: Coordinate -> PrimaryGrid -> Bool
isShipAt (i, j) grid = getElem i j grid `elem` listOfShipType

-- Locate consecutive position of one type of ship
oneShipPositions :: PrimaryCell -> PrimaryGrid -> [Coordinate]
oneShipPositions primaryCell grid =
  [(i, j) | i <- [1 .. r], j <- [1 .. c], (getElem i j grid) == primaryCell]
  where
    r = nrows grid
    c = ncols grid

-- Findout if a type of ship is all hit
isOneShipAllHit :: PrimaryCell -> PrimaryGrid -> TargetingGrid -> Bool
isOneShipAllHit ship primaryGrid targetingGrid =
  all (\(i, j) -> (getElem i j targetingGrid == CellHit)) coords
  where
    coords = oneShipPositions ship primaryGrid

-- get primary cell type
getPrimaryCellType :: Coordinate -> PrimaryGrid -> PrimaryCell
getPrimaryCellType (i, j) grid = getElem i j grid

-- Findout if all ships are Sunk and the game is won
won :: PrimaryGrid -> TargetingGrid -> Bool
won primaryGrid targetingGrid =
  all (\x -> isOneShipAllHit x primaryGrid targetingGrid) listOfShipType

-- With your opponent's PrimaryGrid, making a move updates your TargetingGrid and the game Condition
attack :: [Coordinate] -> State -> State
attack [] state =
  if (won o t)
    then State t o Win
    else state
  where
    t = targetingGrid state
    o = primaryGrid state
attack (x:xs) state = attack xs (f x)
  where
    t = targetingGrid state
    o = primaryGrid state
    c = condition state
    updateTargetingGrid (i, j) x = setElem x (i, j) t
    f (i, j) =
      let targetingCell = getElem i j t
          opponentCell = getElem i j o `elem` listOfShipType
      in case targetingCell of
           Unchecked ->
             case opponentCell of
               True ->
                 if (isOneShipAllHit (getElem i j o) o t)
                   then State (updateTargetingGrid (i, j) CellHit) o Sunk
                   else State (updateTargetingGrid (i, j) CellHit) o Hit
               _ -> State (updateTargetingGrid (i, j) CellMiss) o Miss
           CellHit ->
             case opponentCell of
               True -> State t o AlreadyTaken
               _ -> State (updateTargetingGrid (i, j) CellMiss) o Miss
           CellMiss ->
             case opponentCell of
               True ->
                 if (isOneShipAllHit (getElem i j o) o t)
                   then State (updateTargetingGrid (i, j) CellHit) o Sunk
                   else State (updateTargetingGrid (i, j) CellHit) o Hit
               _ -> State t o Miss
