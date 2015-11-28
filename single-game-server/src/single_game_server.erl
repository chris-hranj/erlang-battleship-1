-module(single_game_server).
%%-behavior(gen_server).
-compile(export_all).

-record(game, {player1Info,         % Player 1 client's process id and reference
               player2Info,         % Player 2 client's process and reference
               player1Board=[],     % Player 1's board state (a list of ships)
               player2Board=[],     % Player 2's board state
               player1Console=[],   % Player 1's list of hits and misses (list of coord_recs)
               player2Console=[]}). % Player 2's list of hits and misses

-record(ship, {name,                % Name of the ship as an atom
               coord_list=[]}).     % List of coordinate records where the ship is placed - indicates current state of the ship

-record(coord, {row,        % Character to indicate row
                     column}).    % Column number

-record(coord_rec, {hit_status=none,    % Whether the coordinate was a hit, miss, or not yet targeted
                      coord}).                  % The actual coordinate

%% Server functions

%% Calls init/1 with an empty list as the parameter
%%start_link() -> gen_server(?MODULE, [], []).

%% Create a game
%%init([]) -> {ok, #game{}}. 

%% Client API Calls

%% Creates a ship record that has not been hit yet
place_ship(ShipName, CoordList, PlayerNum, GameState=#game{}) -> %% Each coordinate in list param is a 2-tuple {$a, 1}
    NewShip = #ship{name=ShipName,
          %% List comprehension to create new list of coordinate
          %% records with hit_status of none (the default)
          coord_list=[#coord_rec{coord=#coord{row=Row,column=Col}} 
           || {Row, Col} <- CoordList,    %% Map each coordinate from passed-in list to a two-tuple
              Row >= $a, Row =< $j,       %% Ensure row is valid
              Col >= 1, Col =< 10 ]},      %% Ensure column is valid
    %% Add the new ship to the appropriate player's board in the game
    %% TODO: Before updating board, check to make sure ships don't overlap
    %% TODO: Also before updating, check to make sure ship is in a valid position (not diagonal)
    if
        PlayerNum =:= 1 ->
            GameState#game{player1Board=[NewShip|GameState#game.player1Board]};
        PlayerNum =:= 2 ->
            GameState#game{player2Board=[NewShip|GameState#game.player2Board]}
    end.

%% PlayerNum is the number of the player making the move
make_move({Row,Col}, PlayerNum, GameState=#game{}) ->
    TargetCoord = #coord{row=Row,column=Col},
    if
        PlayerNum =:= 1 ->
            {HitStatus, NewBoard} = hit_board(TargetCoord,
                                              GameState#game.player2Board, []),
            NewConsole = [#coord_rec{hit_status=HitStatus, coord=TargetCoord} |
                          GameState#game.player1Console],
            GameState#game{player2Board=NewBoard,
                           player1Console=NewConsole};
        PlayerNum =:= 2 ->
            {HitStatus, NewBoard} = hit_board(TargetCoord,
                                              GameState#game.player1Board, []),
            NewConsole = [#coord_rec{hit_status=HitStatus, coord=TargetCoord} |
                          GameState#game.player2Console],
            GameState#game{player1Board=NewBoard,
                           player2Console=NewConsole}
    end.

%% Takes in a target coordinate, a board (list of ships to check against), and a final board accumulator
hit_board(_,[],FinalBoard) -> {miss, FinalBoard}; %% The target was not found - a miss
hit_board(Target=#coord{}, [CurrShip=#ship{}|RestBoard], FinalBoard) ->
    %% Check if target coord is a coord the ship is placed on
    TgtCoordRec = #coord_rec{hit_status=none,coord=Target},
    io:format("Trying to hit target: ~p~n", [Target]),
    case lists:member(TgtCoordRec, CurrShip#ship.coord_list) of 
        true -> %% Update the ship's coordinate to be hit - target was found
            io:format("Hit the target!~n"),
            {hit, 
             [CurrShip#ship{coord_list=[#coord_rec{hit_status=hit,coord=Target} |
              lists:delete(TgtCoordRec, CurrShip#ship.coord_list)]}]
              ++ RestBoard ++ FinalBoard};
        false -> %% Check the next ship
            io:format("Didn't hit the target, try next ship~n"),
            hit_board(Target, RestBoard, [CurrShip | FinalBoard])
    end. 

get_winner(GameState=#game{}) ->
    %% Assumes that get_winner is called after every move - only 1 winner at a time
    case are_ships_left(GameState#game.player1Board) of
        false -> player2;
        true ->
            case are_ships_left(GameState#game.player2Board) of
                false -> player1;
                true -> no_one
            end
    end.

are_ships_left([]) -> false;
are_ships_left([Ship|Rest]) ->
    case lists:filter(fun(X) -> X#coord_rec.hit_status =/= hit end, Ship#ship.coord_list) of
        [] -> are_ships_left(Rest);
        _ -> true
    end.
