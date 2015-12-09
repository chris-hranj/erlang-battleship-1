-module(battleship_01_news).

-export([init/0, stop/1]).

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
    boss_news:start(),
    boss_mq:start(),
	{ok, WatchId} = boss_news:watch("games", 
	fun(created, NewGame) -> 
			boss_mq:push("new-games", NewGame);
		(deleted, OldGame) ->
			boss_mq:push("old-games", OldGame)
	end),
	{ok, [WatchId]}.

stop(ListOfWatchIDs) ->
    lists:map(fun boss_news:cancel_watch/1, ListOfWatchIDs).
