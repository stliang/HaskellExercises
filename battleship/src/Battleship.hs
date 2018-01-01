{-|
Description : Battleship game - https://en.wikipedia.org/wiki/Battleship_(game)

This version of Battleship can only have one ship per ShipType be placed
on ShipGrid.

TODO Guading coordinate out of bound
use smart constructor or promote value to type level
-}
module Battleship where

import Data.List

-- | Matrix index starts from 1 and has access complexity of O(1)
import Data.Matrix
import TargetingGridTypes as T
import Types

-- | A targeting grid that tracks the result of attacks
type TargetingGrid = Matrix T.TargetingCell

-- | A grid where ships can be placed
type ShipGrid = Matrix (Maybe ShipType)

-- | A grid describs the locations of land masses
type LandGrid = Matrix (Maybe LandType)

-- | (i,j) coordinate representing i-th row and j-th column numbers
-- When used in ship placement, it represents the bow
type Coordinate = (Int, Int)

type ShipeWidth = Int

type ShipLength = Int

-- | Dimension describs objects in a game
data Dim = Dim
  { getWidth :: Int
  , getLength :: Int
  , getHeight :: Int
  } deriving (Show)

-- | Type class enforces correctness of function processing sizeness
class Size a where
  dim :: a -> Dim

-- | Valid sizes for each ship type
instance Size ShipType where
  dim Carrier = Dim 1 5 1
  dim Battleship = Dim 1 4 1
  dim Cruiser = Dim 1 3 1
  dim Submarine = Dim 1 3 1
  dim Destroyer = Dim 1 2 1

-- | Game state at trasition
data State = State
  { targetingGrid :: TargetingGrid
  , shipGrid :: ShipGrid
  , condition :: Condition
  }

-- | A Scene reflects the states of a game after each move is made
data Scene = Scene
  { myShipGrid :: ShipGrid
  , myShips :: [ShipType]
  } deriving (Show)

-- | Type of ship placement errors
data PlacementError
  = AlreadyPlaced
  | NotInBound
  | NotAvaliable
  deriving (Enum, Eq, Show)

-- | Type of attack errors
data AttackError
  = OutOfBound
  | InvalidState (Enum, Eq, Show)

-- | Get the Coordinates of a proposed ship position
shipCoordinate
  :: Size shipType
  => Coordinate -- ^ grid position of the ship bow
  -> Direction -- ^ the stern direction
  -> shipType -- ^ ShipType is an instance of Size class
  -> [Coordinate] -- ^ coordinates of a ship regardless of grid dimension
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

-- | Check if a Coordinate is within the bound of a given grid
inBound :: Coordinate -> ShipGrid -> Bool
inBound (i, j) grid = i >= 1 && i <= r && j >= 1 && j <= c
  where
    r = nrows grid
    c = ncols grid

-- | Gather all cell positions of a ship type
shipPositions :: ShipType -> ShipGrid -> [Coordinate]
shipPositions ship grid = [(i, j) | i <- [1 .. r], j <- [1 .. c], (getElem i j grid) == Just ship]
  where
    r = nrows grid
    c = ncols grid

-- | A list of valid ship types
listOfShipType :: [ShipType]
listOfShipType = enumFrom (toEnum 0 :: ShipType)

-- | All positions of all ship types
allShipPositions :: ShipGrid -> [Coordinate]
allShipPositions grid =
  foldr (\shipType acc -> shipPositions shipType grid ++ acc) [] listOfShipType

-- | Get the surrounding cells of a given list of coordinates
neighbourCells :: [Coordinate] -> [Coordinate]
neighbourCells = foldr (\x acc -> lr x ++ tb x ++ acc) []
  where
    lr (i, j) = [(i, j - 1), (i, j + 1)]
    tb (i, j) = [(i + 1, j), (i - 1, j)]

-- | Check if the proposed site is not already occupied
isNotOccupied :: [Coordinate] -> ShipGrid -> Bool
isNotOccupied coords grid = all (\(i, j) -> (getElem i j grid) == Nothing) proposed
  where
    proposed = filter (`inBound` grid) $ nub $ neighbourCells coords ++ coords

-- | Update ship grid with ship placement
updateShipGrid :: ShipType -> [Coordinate] -> ShipGrid -> ShipGrid
updateShipGrid ship coord grid = foldr (setElem $ Just ship) grid coord

-- | Update ship registry
updateShipRegistery :: ShipType -> [ShipType] -> [ShipType]
updateShipRegistery ship ships =
  if ship `elem` ships
    then ships
    else ship : ships

-- | Place a type of ship onto ship grid only if space is avaliable
placeShip
  :: Coordinate -- ^ Bow position
  -> Direction -- ^ Direction of stern
  -> ShipType -- ^ An instance of a ship type
  -> Scene -- ^ Given a scene to place ship in
  -> MaybeEither PlacementError Scene -- ^ Either return a Right of valid scene or Left of PlacementError
placeShip coord dir ship scene
  | isAlreadyPlaced = Left AlreadyPlaced
  | isNotInBound = Left NotInBound
  | isNotAvaliable = Left NotAvaliable
  | otherwise =
    Right $
    Scene
      (updateShipGrid ship proposedShip $ myShipGrid scene)
      (updateShipRegistery ship $ myShips scene)
  where
    proposedShip = shipCoordinate coord dir ship
    grid = myShipGrid scene
    isAlreadyPlaced = ship `elem` myShips scene -- limits one ship per type in this version
    isNotInBound = not $ all (`inBound` grid) proposedShip
    isNotAvaliable = not $ isNotOccupied proposedShip grid

-- | Generate an ship grid with no ships
emptyShipGrid :: Int -> Int -> ShipGrid
emptyShipGrid r c = matrix r c $ \(i, j) -> Nothing

-- | Generate a targeting grid with all unchecked cells
blankTargetingGrid :: Int -> Int -> TargetingGrid
blankTargetingGrid r c = matrix r c $ \(i, j) -> Unchecked

-- | Show the conthen of a matrix
showGrid
  :: Show a
  => Matrix a -> IO ()
showGrid m = putStrLn $ prettyMatrix m

-- | Check if a ship is in a given position
isShipAt :: Coordinate -> ShipGrid -> Bool
isShipAt (i, j) grid = getElem i j grid /= Nothing

-- | Locate consecutive position of one type of ship
shipPositions :: ShipType -> ShipGrid -> [Coordinate]
shipPositions ship grid = [(i, j) | i <- [1 .. r], j <- [1 .. c], (getElem i j grid) == Just ship]
  where
    r = nrows grid
    c = ncols grid

-- | Findout if targeted position is already taken
isTaken :: Coordinate -> TargetingGrid -> Bool
isTaken coord targetingGrid = targetCell == T.Hit || targetCell == T.Miss
  where
    targetCell = getElem i j targetingGrid

-- | Findout if a targeted ship has sunk
isSunk :: Coordinate -> ShipGrid -> TargetingGrid -> Bool
isSunk coord shipGrid targetingGrid =
  case coords of
    Nothing -> False -- No ship at targeted position
    Just xs -> all (\(i, j) -> (getElem i j targetingGrid == T.Hit)) xs -- all positions hit then sunk
  where
    coords = do
      ship <- getElem i j shipGrid -- find the type of ship being attcked
      return $ shipPositions ship shipGrid -- return position of the whole ship

-- | Findout if all ships are sunk and the game is won
isWon :: ShipGrid -> TargetingGrid -> Bool
isWon shipGrid targetingGrid = all (\(i, j) -> (targetingCell i j) == T.Hit) ships -- all func should short circuit
  where
    ships = allShipPositions shipGrid
    targetingCell i j = getElem i j targetingGrid

-- | Perform an attack and generate a new state of the game
attack :: Coordinate -> State -> Either AttackError State
attack (i, j) state
  | not inBound = Left OutOfBound
  | otherwise =
    if isHit
      then (Right $ updateTarget T.Hit)
      else (Right $ updateTarget T.Miss)
  where
    tGrid = targetingGrid state
    sGrid = shipGrid state
    cond = condition state
    isHit =
      case getElem i j sGrid of
        Just _ -> True -- Just expresses a ship is in the cell
        _ -> False
    updateTarget :: TargetingCell -> State
    updateTarget c = State (setElem c i j tGrid) sGrid cond

-- | Eval a recorded attack
evalAttack :: Coordinate -> State -> Either AttackError State
evalAttack (i, j) state
  | not inBound = Left OutOfBound
  | isWon sGrid tGrid = Right $ State tGrid sGrid Win
  | isSunk (i, j) sGrid tGrid = Right $ State tGrid sGrid Sunk
  | isTaken (i, j) tGrid = Right $ State tGrid sGrid AlreadyTaken
  | isHit = Right $ State tGrid sGrid Hit
  | otherwise = Right $ State tGrid sGrid Miss
  where
    isHit = getElem i j tGrid == T.Hit
    tGrid = targetingGrid state
    sGrid = shipGrid state

-- | Run the game by one move
run :: Coordinate -> State -> Either AttackError State
run c s = undefined
