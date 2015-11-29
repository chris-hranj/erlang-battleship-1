# Documentation for `single_game_server.erl`

## Data Structures
### Records
#### `game` Record
- The `game` record is used to store the state of a game of battleship
- For each instance of a `single_game_server`, there will be one `game` record instance stored by the server
##### Attributes
- `player1Info` and `player2Info` - will contain the process ids of the player 1 and 2 client processes
    - Not yet utilized in implementation
- `player1Board` and `player2Board` - represents the coordinates of each ship on the board for each player and which of those coordinates have been hit
    - Stored as a list of `ship` records
- `player1Console` and `player2Console` - a list of `coord_recs` that stores all coordinates that each player has targeted and whether the attack at each coordinate was a hit or a miss
- `winner` - stores an atom that indicates whether someone has won the game - updated each time a move is made
    - Possible values: `player1`, `player2`, and `no_one`
- `turn` - atom that indicates whose turn it is
    - By default, player 1 should take the first turn
    - Possible values: `player1` and `player2`

#### `ship` Record
- The ship record stores the name of a ship, the list of coordinates it occupies, and whether each of those coordinates has been hit or not
##### Attributes
- `name` - the name of a ship stored as an atom
- `coord_list` - a list of `coord_rec` records that indicate which coordinates it occupies and whether each has been hit or not

#### `coord_rec` Record
- This type of record stores the position of a coordinate and whether that coordinate has been hit
##### Attributes
- `hit_status` - whether the coordinate has been hit, was missed, or has not been targeted
    - Possible values - `hit`, `miss`, `none`
- `coord` - a `coord` record that indicates the position of the coordinate

#### `coord` Record
- Stores the position of a coordinate
##### Attributes
- `row` - the row value of the coordinate (stored as a lower case alphabetical character, which is represented as an integer internally in Erlang)
    - `row` values start at `$a` and end at `$j`
- `column` - the column number of the coordinate (stored as an integer)
    - `column` values start at `1` and end at `10`

## Client API
### `place(Pid, ShipName, ShipCoords, Placer)`
- Places a ship on a player's board at the specified coordinates (only if those coordinates are valid)
#### Attributes
- `Pid` - the process id of the single_game_server to interact with
- `ShipName` - the name of the ship (stored as an atom) to be placed
- `ShipCoords` - a list of two-tuples that represent the coordinates to place the ship on
    - To place the ship on the first two columns of row A, use this list of tuples: `[{$a,1}, {$a,2}]`
- `Placer` - an atom that indicates which player is placing the ship
    - Possible values: `player1` and `player2`

### `attack(Pid, Target, Attacker)`
- Executes a move by attacking the coordinate represented by the `Target` parameter
#### Parameters
- `Pid` - the process id of the single_game_server to interact with
- `Target` - a two-tuple that specifies the coordinate to attack
    - Ex: to attack the coordinate at row B, column 3, pass in the tuple `{$b,3}`
- `Attacker` - an atom that indicates which player is attacking
    - Possible values: `player1` and `player2`

### `get_game(Pid)`
- Returns the `game` record that represents the current state of the game on the single_game_server
- Only use this function for debugging - don't send this data to any player clients, since it will give them top secret info on the position of opponents' ships
#### Parameters
- `Pid` - the process id of the single_game_server to interact with