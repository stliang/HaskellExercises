{-|
Description : Battleship game - https://en.wikipedia.org/wiki/Battleship_(game)
This version of Battleship can only have one ship per ShipType be placed on ShipGrid.
-}
module Battleship where

import Control.Exception.Base
import Data.List

-- | Matrix index starts from 1 and has access complexity of O(1)
import Data.Matrix
import TargetingGridTypes as Target
import Types

-- | Get the Coordinates of a proposed ship position
shipCoordinate
  :: Size shipType
  => ShipCoordinate -- ^ grid position of the ship bow
  -> Direction -- ^ the stern direction
  -> shipType -- ^ ShipType is an instance of Size class
  -> [ShipCoordinate] -- ^ coordinates of a ship regardless of grid dimension
shipCoordinate (i, j) dir ship =
  case dir of
    TailUp -> goUD negate sl
    TailDown -> goUD id sl
    TailLeft -> goLR negate sl
    TailRight -> goLR id sl
  where
    sl = getLength (dim ship) - 1 -- Minus one because counting down from shipLength
    goUD :: (Int -> Int) -> Int -> [ShipCoordinate]
    goUD _ 0 = [(i, j)]
    goUD f x = ((i + f x), j) : goUD f (x - 1)
    goLR :: (Int -> Int) -> Int -> [ShipCoordinate]
    goLR _ 0 = [(i, j)]
    goLR f x = (i, (j + f x)) : goLR f (x - 1)

-- | Check if a Coordinate is within the bound of a given grid
inBound :: ShipCoordinate -> ShipGrid -> Bool
inBound (i, j) grid = i >= 1 && i <= r && j >= 1 && j <= c
  where
    r = nrows grid
    c = ncols grid

-- | Gather all cell positions of a ship type
shipPositions :: ShipType -> ShipGrid -> [ShipCoordinate]
shipPositions ship grid = [(i, j) | i <- [1 .. r], j <- [1 .. c], (getElem i j grid) == Just ship]
  where
    r = nrows grid
    c = ncols grid

-- | A list of valid ship types
listOfShipType :: [ShipType]
listOfShipType = enumFrom (toEnum 0 :: ShipType)

-- | All positions of all ship types
allShipPositions :: ShipGrid -> [ShipCoordinate]
allShipPositions grid =
  foldr (\shipType acc -> shipPositions shipType grid ++ acc) [] listOfShipType

-- | Get the surrounding cells of a given list of coordinates
neighbourCells :: [ShipCoordinate] -> [ShipCoordinate]
neighbourCells = foldr (\x acc -> lr x ++ tb x ++ acc) []
  where
    lr (i, j) = [(i, j - 1), (i, j + 1)]
    tb (i, j) = [(i + 1, j), (i - 1, j)]

-- | Check if the proposed site is not already occupied
isNotOccupied :: [ShipCoordinate] -> ShipGrid -> Bool
isNotOccupied coords grid = all (\(i, j) -> (getElem i j grid) == Nothing) proposed
  where
    proposed = filter (`inBound` grid) $ nub $ neighbourCells coords ++ coords

-- | Update ship grid with ship placement
updateShipGrid :: ShipType -> [ShipCoordinate] -> ShipGrid -> ShipGrid
updateShipGrid ship coord grid = foldr (setElem $ Just ship) grid coord

-- | Update ship registry
updateShipRegistery :: ShipType -> [ShipType] -> [ShipType]
updateShipRegistery ship ships =
  if ship `elem` ships
    then ships
    else ship : ships

-- | Place a type of ship onto ship grid only if space is avaliable
placeShip
  :: ShipCoordinate -- ^ Bow position
  -> Direction -- ^ Direction of stern
  -> ShipType -- ^ An instance of a ship type
  -> Scene -- ^ Given a scene to place ship in
  -> Either PlacementError Scene -- ^ Either return a Right of valid scene or Left of PlacementError
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

-- | Show the conthen of a matrix
showGrid
  :: Show a
  => Matrix a -> IO ()
showGrid m = putStrLn $ prettyMatrix m

-- | Get a snigle cell of a ShipGrid
getShipGridCell :: ShipCoordinate -> ShipGrid -> Maybe ShipType
getShipGridCell (i, j) grid = getElem i j grid

-- | Check if a ship is in a given position
isShipAt :: ShipCoordinate -> ShipGrid -> Bool
isShipAt (i, j) grid = getElem i j grid /= Nothing

-- | Findout if targeted position is already taken
isTaken :: ShipCoordinate -> TargetingGrid -> Bool
isTaken (i, j) targetingGrid = targetCell == Target.Hit || targetCell == Target.Miss
  where
    targetCell = getElem i j targetingGrid

-- | Findout if a type of ship is sunk
isSunk :: ShipType -> ShipGrid -> TargetingGrid -> Bool
isSunk shipType shipGrid targetingGrid =
  all (\(i, j) -> (getElem i j targetingGrid == Target.Hit)) coords -- all positions hit then sunk
  where
    coords = shipPositions shipType shipGrid -- return position of the whole ship

-- | Findout if a targeted ship has sunk
isSunk' :: ShipCoordinate -> ShipGrid -> TargetingGrid -> Bool
isSunk' (i, j) shipGrid targetingGrid =
  case (getElem i j shipGrid) of
    Nothing -> False -- No ship at targeted position
    Just shipType -> isSunk shipType shipGrid targetingGrid

-- | Findout if all ships are sunk and the game is won
isWon :: ShipGrid -> TargetingGrid -> Bool
isWon shipGrid targetingGrid = all (\(i, j) -> (targetingCell i j) == Target.Hit) ships -- all func should short circuit
  where
    ships = allShipPositions shipGrid
    targetingCell i j = getElem i j targetingGrid

-- | Perform an attack and generate a new state of the game
-- Assertion is made with TargetCoordinate is in bound
attack :: TargetCoordinate -> State -> State
attack (TargetCoordinate (i, j)) state
  | isTaken (i, j) tGrid = State tGrid sGrid Types.AlreadyTaken
  | otherwise =
    if isHit
      then updateTarget Target.Hit
      else updateTarget Target.Miss
  where
    tGrid = targetingGrid state
    sGrid = shipGrid state
    cond = condition state
    isHit =
      case getElem i j sGrid of
        Just _ -> True -- Just expresses a ship is in the cell
        _ -> False
    updateTarget :: TargetingCell -> State
    updateTarget c =
      case c of
        Target.Hit -> State (setElem c (i, j) tGrid) sGrid Types.Hit
        _ -> State (setElem c (i, j) tGrid) sGrid Types.Miss

-- | Eval a recorded attack
evalAttack :: TargetCoordinate -> State -> State
evalAttack (TargetCoordinate (i, j)) state
  | isWon sGrid tGrid = State tGrid sGrid Win
  | isSunk' (i, j) sGrid tGrid = State tGrid sGrid Sunk
  | otherwise = state
  where
    isHit = getElem i j tGrid == Target.Hit
    tGrid = targetingGrid state
    sGrid = shipGrid state

-- | Run the game by one move
run :: TargetCoordinate -> State -> State
run target s = evalAttack target $ attack target s

-- | Smart constructor of TargetCoordinate
mkTargetCoordinate :: ShipCoordinate -> TargetCoordinate
mkTargetCoordinate (r, c) =
  assert (r > 0 && r <= gridRowCount && c > 0 && c <= gridColumnCount) TargetCoordinate (r, c)

-- | Smart constructor of ShipGrid
mkShipGrid :: ShipGrid
mkShipGrid = matrix gridRowCount gridColumnCount $ \(i, j) -> Nothing

-- | Smart constructor of TargetingGrid
mkTargetingGrid :: TargetingGrid
mkTargetingGrid = matrix gridRowCount gridColumnCount $ \(i, j) -> Unchecked
