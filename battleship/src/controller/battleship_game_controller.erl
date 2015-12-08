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

place_ship('GET', []) -> ok;
place_ship('POST', []) ->
    Curr = boss_db:find_first(game, [{id, 'equals', "game-1"}]),
    GameRec = #game{player1Board=Curr:player1_board(),
                    player2Board=Curr:player2_board(),
                    player1Console=Curr:player1_console(),
                    player2Console=Curr:player2_console(),
                    winner=Curr:winner(),
                    turn=Curr:turn()},
    {_, NewRec} = single_game_server:place(patrol_boat, [{$a,1},{$a,2}], player2, GameRec),
    NewGame = Curr:set([{player1_board, NewRec#game.player1Board},
                        {player2_board, NewRec#game.player2Board},
                        {player1_console, NewRec#game.player1Console},
                        {player2_console, NewRec#game.player2Console},
                        {winner, NewRec#game.winner},
                        {turn, NewRec#game.turn}]),
    boss_db:save_record(NewGame),
    {ok, NewGame}.

attack('GET', []) -> ok;
attack('POST', []) ->
    Curr = boss_db:find_first(game, [{id, 'equals', "game-1"}]),
    GameRec = #game{player1Board=Curr:player1_board(),
                    player2Board=Curr:player2_board(),
                    player1Console=Curr:player1_console(),
                    player2Console=Curr:player2_console(),
                    winner=Curr:winner(),
                    turn=Curr:turn()},
    {_, NewRec} = single_game_server:attack_target({$a,1}, player1, GameRec),
    NewGame = Curr:set([{player1_board, NewRec#game.player1Board},
                        {player2_board, NewRec#game.player2Board},
                        {player1_console, NewRec#game.player1Console},
                        {player2_console, NewRec#game.player2Console},
                        {winner, NewRec#game.winner},
                        {turn, NewRec#game.turn}]),
    boss_db:save_record(NewGame),
    {ok, NewGame}.

create('GET', []) ->
  ok;
create('POST', []) ->
  NewGame = game:new(id, [], [], [], [], no_one, player1),
  case NewGame:save() of
    {ok, SavedGame} ->
    {redirect, [{action, "setup"}, {game_id,SavedGame:id()}]};
    {error, ErrorList} ->
    {ok, [{errors, ErrorList}, {new_msg, NewGame}]}
  end.

join('GET', []) ->
  ok;
join('POST', []) ->
  GameId = Req:post_param("game_id"),
  ExistingGame = boss_db:find(game, [{game_id,GameId}]).

setup('GET', [GameId]) ->
  Game = boss_db:find(GameId),
  {ok, [{gameid, Game}]}.

test('GET', []) ->
  ok;
test('POST', []) ->
  GameId = Req:post_param("game_id"),
  AircraftPlacement = Req:post_param("carrier"),
  BattleshipPlacement = Req:post_param("battleship"),
  DestroyerPlacement = Req:post_param("destroyer"),
  SubmarinePlacement = Req:post_param("submarine"),
  PatrolPlacement = Req:post_param("patrol_boat"),
  {ok, [{carrier, AircraftPlacement}, {battleship, BattleshipPlacement}, {destroyer, DestroyerPlacement}, {submarine, SubmarinePlacement}, {patrol, PatrolPlacement}]}.

%% Don't know what to do with this yet
%attack('POST', []) ->
  %coords = Req:post_param("attack_coords").
