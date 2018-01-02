module Types where

import Data.Matrix
import TargetingGridTypes as T

-- | Valid number of row per grid
gridRowCount = 10 :: Int

-- | Valid number of column per grid
gridColumnCount = 10 :: Int

-- | Alias for grid row
type Row = Int

-- | Alias for grid column
type Column = Int

-- | TargetCoordinate of grid
newtype TargetCoordinate =
  TargetCoordinate (Row, Column)

-- | ShipCoordinate of grid
-- Smart constructor is not need as placement of ship is checked by the Battleship module
-- Also, out of bound ShipCoordinate is allow for finding out the surrounding space of a ship
type ShipCoordinate = (Row, Column)

-- | Valid ship types
data ShipType
  = Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer
  deriving (Enum, Eq, Show)

-- | Valid ship placedment directions
data Direction
  = TailUp
  | TailDown
  | TailLeft
  | TailRight

-- | Valid land types
data LandType =
  Island
  deriving (Eq, Show)

-- | Valid conditions that describ the state of a game play 
data Condition
  = Win
  | Sunk
  | AlreadyTaken
  | Hit
  | Miss
  | Playing
  deriving (Eq, Show)

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
