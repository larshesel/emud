-module(mymap).

-export([init/0]).


init() ->
    {ok, StartRoom} = emud_room:create_empty_room(),
    ok = emud_room:set_description(StartRoom, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around."),
    
    {ok, WestRoom} = emud_room:create_empty_room(),
    ok = emud_room:set_description(WestRoom, "westRoom"),

    emud_room:link_rooms(StartRoom, WestRoom, west),
    emud_room:link_rooms(WestRoom, StartRoom, east),

    {ok, Player1} = emud_player:create_player(),
    emud_player:enter(StartRoom, Player1),

    {ok, Player2} = emud_player:create_player(),
    emud_player:enter(StartRoom, Player2),

    %% [StartRoom, WestRoom, Player1, Player2],
    emud_console:start(Player1).

