-module(battleship_game_controller, [Req]).
-compile(export_all).

hello('GET', []) ->
    {ok, [{greeting, "Hello, world!"}]}.

list('GET', []) ->
    Games = boss_db:find(game, []),
    {ok, [{games, Games}]}.

create('GET', []) ->
    ok;
create('POST', []) ->
    NewGame = game:new(id,[],[],[],[],no_one,player1),
    case NewGame:save() of
    	{ok, SavedGame} ->
    	{redirect, [{action, "list"}]};
    	{error, ErrorList} ->
    	{ok, [{errors, ErrorList}, {new_msg, NewGame}]}
    end.

goodbye('POST', []) ->
    boss_db:delete(Req:post_param("id")),
    {redirect, [{action, "list"}]}.
%%% -------The stuff under here has not been tested yet-----
send_test_message('GET', []) ->
    TestMessage = "Free at last!",
    boss_mq:push("test-channel", TestMessage),
    {output, TestMessage}.

pull('GET', [LastTimestamp]) ->
    {ok, Timestamp, Greetings} = boss_mq:pull("new-greetings", 
    list_to_integer(LastTimestamp)),
    {json, [{timestamp, Timestamp}, {greetings, Greetings}]}.

live('GET', []) ->
    Greetings = boss_db:find(greeting, []),
    Timestamp = boss_mq:now("new-greetings"),
    {ok, [{greetings, Greetings}, {timestamp, Timestamp}]}.
