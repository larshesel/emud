-module(mymap).

-export([init/0]).


init() ->
    {ok, StartRoom} = emud_room:create_empty_room(startroom),
    ok = emud_room:set_description(StartRoom, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around."),
    
    {ok, WestRoom} = emud_room:create_empty_room(westroom),
    ok = emud_room:set_description(WestRoom, "You're in a huge lobby with beatiful marble columns and floors."),


    emud_room:link_rooms(StartRoom, WestRoom, w),
    emud_room:link_rooms(WestRoom, StartRoom, e),


    {ok, Toilet} = emud_room:create_empty_room(restroom),
    ok = emud_room:set_description(Toilet, "You're in the restroom. There's a toilet in the corner and a sink on the wall. It smells of poo."),

    {ok, Poo} = emud_item:create_item(poo),
    emud_item:set_description(Poo, "You pick up the dark, round object, it feels soft and slightly sticky. You are disgusted to realize it's a poo and someone has been eating peanuts."),
    emud_item:set_short_description(Poo, "A dark oblong object."),
    ok = emud_room:add_item(Toilet, Poo),

    emud_room:link_rooms(StartRoom, Toilet, n),
    emud_room:link_rooms(Toilet, StartRoom, s),


    {ok, Player1} = emud_player:create_player(player1),
    emud_player:enter(Player1, StartRoom),

    {ok, Player2} = emud_player:create_player(player2),
    emud_player:enter(Player2, StartRoom),

    %% [StartRoom, WestRoom, Player1, Player2],
    emud_console:start(Player1).

