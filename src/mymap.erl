-module(mymap).

-export([init/0]).


init() ->
    {ok, StartRoom} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(startroom)),
    ok = emud_room:set_description(StartRoom, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around."),
    
    {ok, WestRoom} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(westroom)),
    ok = emud_room:set_description(WestRoom, "You're in a huge lobby with beatiful marble columns and floors."),

    emud_room:link_rooms(StartRoom, WestRoom, w),
    emud_room:link_rooms(WestRoom, StartRoom, e),

    {ok, RestRoom} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(restroom)),
    ok = emud_room:set_description(RestRoom, "You're in the restroom. There's a toilet in the corner and a sink on the wall. It smells of poo."),

    {ok, Poo} = supervisor:start_child(emud_item_sup, emud_specs:childspec_item(poo)),
    emud_item:set_description(Poo, "You pick up the dark, round object, it feels soft and slightly sticky. You are disgusted to realize it's a poo and someone has been eating peanuts."),
    emud_item:set_short_description(Poo, "A dark oblong object."),
    ok = emud_room:add_item(RestRoom, Poo),

    emud_room:link_rooms(StartRoom, RestRoom, n),
    emud_room:link_rooms(RestRoom, StartRoom, s),

    {ok, P1} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player1)),
    emud_player:enter(P1, StartRoom),

    {ok, P2} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player2)),
    emud_player:enter(P2, StartRoom),

    emud_console:start(P1).

