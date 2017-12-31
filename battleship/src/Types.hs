module Types where

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
