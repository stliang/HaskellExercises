# When coding, keep these in mind:

Testability

Performace

Error handling

Correctness

Extensibility

Readability

Wholesomeness (use fold)

# Test result

Battleship
  Battleship game
    length of a ship type produces a list of ShipCoordinates of the same length
    in bound check of ship that is out of boundary is quals to False
    ship type Destroyer at (3,3) to the left has [(3,2), (3,3)] coordinate
    ship type Submarine at (3,3) to the right has [(3,3), (3,4), (3,5)] coordinate
    ship type Carrier at (3,3) downward has [(3,3), (4,3), (5,3), (6,3), (7,3)] coordinate
    ship type Battleship at (6,3) upward has [(3, 3), (4, 3), (5, 3), (6, 3)] coordinate
    ship type Cruiser placed at (3,3) to the left is in the myShips registery
    neighbouring cells of (4,2) is [(4,1),(4,3),(5,2),(3,2)]
    position at (5,6) for a ship placed at (5,5) isNotOccupied is False
    isNotOccupied check on (5,7) for ship placed at (5,5) is True
    updatePrimaryGrid for Destroyer at (5,5) has (5,5) (5,4) set to Destroyer
    when a ship already added, the ship registery should not change
    adding Battleship to registery [Destroyer, Cruiser] equals [Battleship, Destroyer, Cruiser]
    place a Carrier at (2,2) tailing right and Destroyer at (2,8) tailing down registery equals [Carrier, Destroyer]
    place a Carrier at (2,2) tailing right and Destroyer at (2,7) tailing down equals NotAvaliable
    placing Carrier, Battleship, Cruiser, Submarine, and Destroyer at avaliable space resulting registry of all ships
    place a Carrier at (2,4) tailing left at out of bound position equals NotInBound
    new empty ship grid has all cells equal to Nothing
    new empty targeting grid has all cells equal to Unchecked
    check an existing Battleship placed at (6,3) equals True
    get positions of one Battleship placed at (6,3) equals [(3, 3), (4, 3), (5, 3), (6, 3)]
    check if sections of ship got hit on an unharmed Battleship equal False
    check if sections of ship got hit on sunk Battleship equals True
    get the cell of a Battleship place at (6,3) equals to Just Battleship
    isWon function returns True if shipGrid and targetingGrid agree all ships are Hit
    attack all positions of a single ship yields its coordinates in targeting grid equal Hit
    when hitting all sections of a ship and the game is not won, the condition equals Sunk
    when ships are sunk by attacks, the condition equals Win
    attack the same position produces condition equals AlreadyTaken
    attack a position where a ship is placed produces condition equals Hit
    attack a position where no ship is placed produces condition equals Miss

Finished in 0.0053 seconds
31 examples, 0 failures
             
battleship-0.1.0.0: Test suite battleship-test passed
