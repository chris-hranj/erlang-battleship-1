-module(single_game_server).
-compile(export_all).

-record(game, {player1Board=[],     % Player 1's board state (a list of ships)
               player2Board=[],     % Player 2's board state
               player1Console=[],   % Player 1's list of hits and misses (list of coord_recs)
               player2Console=[],   % Player 2's list of hits and misses
               winner=no_one,       % Indicates whether someone has won yet
               turn=player1}).      % Indicates whose turn it is

-record(ship, {name,                % Name of the ship as an atom
               coord_list=[]}).     % List of coordinate records where the ship is placed
                                    % indicates current state of the ship

-record(coord, {row,        % Character to indicate row
                column}).    % Column number

-record(coord_rec, {hit_status=none,    % Whether the coordinate was a hit, miss, or not yet targeted
                    coord}).            % The actual coordinate

%% Client API
attack_target(Target, Attacker, Game=#game{}) ->
    call_attack(Target, Attacker, Game).

place(ShipName, ShipCoords, Placer, Game=#game{}) ->
    call_place(ShipName, ShipCoords, Placer, Game).

call_attack(Target, Attacker, Game=#game{}) ->
    if Attacker =:= Game#game.turn ->
           NewGame = make_move(Target, Attacker, Game),
           {did_attack, NewGame};
       Attacker =/= Game#game.turn ->
           {did_not_attack, Game}
    end.

call_place(ShipName, CoordList, Placer, Game=#game{}) ->
    {Status, NewGame} = place_ship(ShipName, CoordList, Placer, Game),
    {Status, NewGame}.

%% Creates a ship record that has not been hit yet
place_ship(ShipName, CoordList, Placer, GameState=#game{}) -> %% Each coordinate in list param is a 2-tuple {$a, 1}
    OrigLen = length(CoordList),
    NewShip = #ship{name=ShipName,
          %% List comprehension to create new list of coordinate
          %% records with hit_status of none (the default)
          coord_list=[#coord_rec{coord=#coord{row=Row,column=Col}} 
           || {Row, Col} <- CoordList,    %% Map each coordinate from passed-in list to a two-tuple
              Row >= $a, Row =< $j,       %% Ensure row is valid
              Col >= 1, Col =< 10 ]},      %% Ensure column is valid
    case length(NewShip#ship.coord_list) of 
        OrigLen -> %% No original coordinates were filtered out
            if
                Placer =:= player1 ->
                    case valid_coord_list(ShipName, CoordList, GameState#game.player1Board) of
                        true ->
                            {placed, GameState#game{player1Board=[NewShip|GameState#game.player1Board]}};
                        false -> 
                            {not_placed, GameState}
                    end;
                Placer =:= player2 ->
                    case valid_coord_list(ShipName, CoordList, GameState#game.player1Board) of
                        true ->
                            {placed, GameState#game{player2Board=[NewShip|GameState#game.player2Board]}};
                        false ->
                            {not_placed, GameState}
                    end
            end;
        _ -> GameState
    end.

valid_coord_list(ShipName, CoordList, Board) ->
    Continue = case length(CoordList) =:= ship_len(ShipName) andalso same_row(CoordList) of
        true -> adjacent(row,CoordList);
        false ->
            case same_column(CoordList) of
                true -> adjacent(col,CoordList);
                false -> false
            end
    end,
    case Continue of
        true -> overlap(CoordList,Board);
        false -> false
    end.

%% Check if all coordinates are on the same row
same_row([{Row,_}|Rest]) -> same_row(Rest, Row). %% Get row of first coord to pass into recursive call
same_row([],_) -> true;
same_row([{Row,_}|Rest], Row) -> same_row(Rest, Row); %% Pattern matches when row of coord and passed-in row are the same
same_row(_,_) -> false. %% If above guard fails, at least one row is not the same

%% Check if all coordinates are on the same column - works the same exact was as same_row
same_column([{_,Col}|Rest]) -> same_column(Rest, Col).
same_column([],_) -> true;
same_column([{_,Col}|Rest],Col) -> same_column(Rest, Col);
same_column(_,_) -> false.

%% Check if all coordinates are adjacent
adjacent(col,CoordList) ->
    ColList = lists:sort([X || {X,_} <- CoordList]),
    L1 = lists:reverse(tl(lists:reverse(ColList))),
    [_|L2] = ColList,
    check_for_ones(lists:zipwith(fun(X,Y) -> Y-X end, L1, L2));
adjacent(row,CoordList) ->
    RowList = lists:sort([X || {_,X} <- CoordList]),
    L1 = lists:reverse(tl(lists:reverse(RowList))),
    [_|L2] = RowList,
    check_for_ones(lists:zipwith(fun(X,Y) -> Y-X end, L1, L2)).

%% Check if all elements of a list are equal to 1
check_for_ones([]) -> true;
check_for_ones([1|Rest]) -> check_for_ones(Rest);
check_for_ones([_|_]) -> false.

%% Check if coordinates overlap with the existing ships
overlap(_,[]) -> true;
overlap(CoordList, [Ship=#ship{}|Rest]) ->
    ExistCoords = [coord_rec_to_tuple(CRec) || CRec <- Ship#ship.coord_list],
    AllCoords = CoordList ++ ExistCoords,
    case length(AllCoords) =:= sets:size(sets:from_list(AllCoords)) of
        true -> overlap(CoordList, Rest);
        false -> false
    end.

coord_rec_to_tuple(#coord_rec{coord=#coord{row=Row,column=Col}}) -> {Row,Col}.

%% PlayerNum is the number of the player making the move
make_move({Row,Col}, Attacker, GameState=#game{}) ->
    TargetCoord = #coord{row=Row,column=Col},
    if
        Attacker =:= player1 ->
            {HitStatus, NewBoard} = hit_board(TargetCoord,
                                              GameState#game.player2Board, []),
            NewConsole = [#coord_rec{hit_status=HitStatus, coord=TargetCoord} |
                          GameState#game.player1Console],
            NewGame = GameState#game{player2Board=NewBoard,
                           player1Console=NewConsole,
                           turn=player2},
            %% Check if anyone has won after this move has been made
            Winner = get_winner(NewGame),
            NewGame#game{winner=Winner};
        Attacker =:= player2 ->
            {HitStatus, NewBoard} = hit_board(TargetCoord,
                                              GameState#game.player1Board, []),
            NewConsole = [#coord_rec{hit_status=HitStatus, coord=TargetCoord} |
                          GameState#game.player2Console],
            NewGame = GameState#game{player1Board=NewBoard,
                           player2Console=NewConsole,
                           turn=player1},
            %% Check if anyone has won after this move has been made
            Winner = get_winner(NewGame),
            NewGame#game{winner=Winner}
    end.

%% Takes in a target coordinate, a board (list of ships to check against), and a final board accumulator
hit_board(_,[],FinalBoard) -> {miss, FinalBoard}; %% The target was not found - a miss
hit_board(Target=#coord{}, [CurrShip=#ship{}|RestBoard], FinalBoard) ->
    %% Check if target coord is a coord the ship is placed on
    TgtCoordRec = #coord_rec{hit_status=none,coord=Target},
    case lists:member(TgtCoordRec, CurrShip#ship.coord_list) of 
        true -> %% Update the ship's coordinate to be hit - target was found
            {hit, 
             [CurrShip#ship{coord_list=[#coord_rec{hit_status=hit,coord=Target} |
              lists:delete(TgtCoordRec, CurrShip#ship.coord_list)]}]
              ++ RestBoard ++ FinalBoard};
        false -> %% Check the next ship
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

ship_len(patrol_boat) -> 2;
ship_len(submarine) -> 3;
ship_len(destroyer) -> 3;
ship_len(battleship) -> 4;
ship_len(carrier) -> 5.
