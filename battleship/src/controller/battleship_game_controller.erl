-module(battleship_game_controller, [Req]).
-compile(export_all).

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

attack('POST', []) ->
  coords = Req:post_param("attack_coords").



