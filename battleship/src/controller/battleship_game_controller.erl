-module(battleship_game_controller, [Req]).
-compile(export_all).

-record(game, {player1Board=[],     
               player2Board=[],     
               player1Console=[],   
               player2Console=[],   
               winner=no_one,       
               turn=player1}).      

-record(ship, {name,                
               coord_list=[]}).     
                                    
-record(coord, {row,        
                column}).  

-record(coord_rec, {hit_status=none,
                    coord}).            

-import(single_game_server,[place/4,attack_target/3]).

list('GET', []) ->
    Games = boss_db:find(game, []),
    {ok, [{games, Games}]}.



attack('POST', [GameId,PlayerStr,Coord]) ->
    Curr = boss_db:find_first(game, [{id, 'equals', GameId}]),
    [AttackCoord|_] = Curr:parse(Coord),
    Player = list_to_atom(PlayerStr),
    GameRec = #game{player1Board=Curr:player1_board(),
                    player2Board=Curr:player2_board(),
                    player1Console=Curr:player1_console(),
                    player2Console=Curr:player2_console(),
                    winner=Curr:winner(),
                    turn=Curr:turn()},
    {Status, NewRec} = single_game_server:attack_target(AttackCoord, Player, GameRec),
    case Status of
      did_not_attack ->
        {ok, [{error, "It's not your turn, your attack didn't go through"}]};
      _ ->
        NewGame = Curr:set([{player1_board, NewRec#game.player1Board},
                            {player2_board, NewRec#game.player2Board},
                            {player1_console, NewRec#game.player1Console},
                            {player2_console, NewRec#game.player2Console},
                            {winner, NewRec#game.winner},
                            {turn, NewRec#game.turn}]),
        boss_db:save_record(NewGame),
        {ok, []}
    end.

create('GET', []) ->
  ok;
create('POST', []) ->
  NewGame = game:new(id, [], [], [], [], no_one, player1),
  case NewGame:save() of
    {ok, SavedGame} ->
    {redirect, [{action, "setup"}, {game_id,SavedGame:id()}, {player, "player1"}]};
    {error, ErrorList} ->
    {ok, [{errors, ErrorList}, {new_msg, NewGame}]}
  end.

join('POST', []) ->
  GameId = Req:post_param("id"),
  {redirect, [{action, "setup"}, {game_id, GameId}, {player, "player2"}]}.

setup('GET', [GameId, Player]) ->
  Game = boss_db:find(GameId),
  {ok, [{gameid, GameId}, {player, Player}]};
setup('POST', [GameId, PlayerStr]) ->
  %%GameId = Req:post_param("game_id"),
  PlayerStrr = Req:post_param("player"),
  Player = list_to_atom(PlayerStrr),

  AircraftPlacement = Req:post_param("carrier"),
  BattleshipPlacement = Req:post_param("battleship"),
  DestroyerPlacement = Req:post_param("destroyer"),
  SubmarinePlacement = Req:post_param("submarine"),
  PatrolPlacement = Req:post_param("patrol_boat"),
  

  Curr = boss_db:find_first(game, [{id, 'equals', GameId}]),
  OrigRec = #game{player1Board=Curr:player1_board(),
                    player2Board=Curr:player2_board(),
                    player1Console=Curr:player1_console(),
                    player2Console=Curr:player2_console(),
                    winner=Curr:winner(),
                    turn=Curr:turn()},

  {AircraftStatus, AircraftRec} = single_game_server:place(carrier, Curr:parse(AircraftPlacement), Player, OrigRec),
  {BattleshipStatus, BattleshipRec} = single_game_server:place(battleship, Curr:parse(BattleshipPlacement), Player, AircraftRec),
  {DestroyerStatus, DestroyerRec} = single_game_server:place(destroyer, Curr:parse(DestroyerPlacement), Player, BattleshipRec),
  {SubmarineStatus, SubmarineRec} = single_game_server:place(submarine, Curr:parse(SubmarinePlacement), Player, DestroyerRec),
  {PatrolStatus, PatrolRec} = single_game_server:place(patrol_boat, Curr:parse(PatrolPlacement), Player, SubmarineRec),

  StatusList = [AircraftStatus, BattleshipStatus, DestroyerStatus, SubmarineStatus, PatrolStatus],
  case lists:all(fun(Status) -> Status =:= placed end, StatusList) of
      true ->
        NewGame = Curr:set([{player1_board, PatrolRec#game.player1Board},
                        {player2_board, PatrolRec#game.player2Board},
                        {player1_console, PatrolRec#game.player1Console},
                        {player2_console, PatrolRec#game.player2Console},
                        {winner, PatrolRec#game.winner},
                        {turn, PatrolRec#game.turn}]),
        boss_db:save_record(NewGame),
        {redirect, [{action, "play/" ++ GameId ++ "/" ++ PlayerStrr}]};
      false ->
        {ok, [{error, "Invalid placement of ships, please try again"}]}
  end.

play('GET', [GameId,Player]) ->
  Game = boss_db:find_first(game, [{id, 'equals', GameId}]),
  Turn = Game:turn(),
  TurnString = atom_to_list(Turn),
  if 
    TurnString == Player ->
      PlayerTurn = "your";
    true ->
      PlayerTurn = "your opponent's"
  end,
  case Game:winner() of
    no_one ->
      {ok, [{game_id, GameId}, {player, Player}, {turn, PlayerTurn}]};
    _ ->
      {redirect, [{action, "winner/" ++ GameId ++ "/" ++ Game:winner()}]}
  end.

winner('GET', [GameId, Winner]) ->
  ok.

get_data('GET', [GameId,Player]) ->
    Game = boss_db:find_first(game, [{id, 'equals', GameId}]),
    PlayerAtom = list_to_atom(Player),
    PropList = [{turn, Game:turn()},{winner, Game:winner()}],
    case PlayerAtom of
      player1 ->
        {json, PropList ++ [{board, board_to_proplist(Game:player1_board())}, {console, coord_recs_to_proplist(Game:player1_console())}]};
      player2 ->
        {json, PropList ++ [{board, board_to_proplist(Game:player2_board())}, {console, coord_recs_to_proplist(Game:player1_console())}]};
      _ ->
        {json, [{error, "error"}]}
    end.

board_to_proplist([]) -> [];
board_to_proplist([Curr=#ship{}|Rest]) ->
  [[{ship_name,Curr#ship.name},
   {coord_list,coord_recs_to_proplist(Curr#ship.coord_list)}] | board_to_proplist(Rest)].

coord_recs_to_proplist([]) -> [];
coord_recs_to_proplist([Curr=#coord_rec{}|Rest]) ->
  [[{hit_status, Curr#coord_rec.hit_status},
   {coord, coord_to_proplist(Curr#coord_rec.coord)}] | coord_recs_to_proplist(Rest)].

coord_to_proplist(Coord=#coord{}) ->
  [{row, Coord#coord.row}, {column, Coord#coord.column}].
