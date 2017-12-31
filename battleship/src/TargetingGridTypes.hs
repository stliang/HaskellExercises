module TargetingGridTypes where

-- | Valid states of each cell in a targeting grid
data TargetingCell
  = Unchecked
  | Hit
  | Miss
  deriving (Eq, Show)
