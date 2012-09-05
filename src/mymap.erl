-module(mymap).

-export([init/0]).

childspec_player(Name) ->
    {Name, {emud_player, start_link, [Name]},permanent, 2000, worker, [emud_player]}.
childspec_room(Name) ->
    {Name, {emud_room, start_link, [Name]},permanent, 2000, worker, [emud_room]}.

init() ->
    {ok, _StartRoom} = supervisor:start_child(emud_room_sup, childspec_room(startroom)),
    ok = emud_room:set_description(startroom, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around."),
    
    {ok, _WestRoom} = supervisor:start_child(emud_room_sup, childspec_room(westroom)),
    ok = emud_room:set_description(westroom, "You're in a huge lobby with beatiful marble columns and floors."),


    emud_room:link_rooms(startroom, westroom, w),
    emud_room:link_rooms(westroom, startroom, e),


    {ok, _RestRoom} = supervisor:start_child(emud_room_sup, childspec_room(restroom)),
    ok = emud_room:set_description(restroom, "You're in the restroom. There's a toilet in the corner and a sink on the wall. It smells of poo."),

    {ok, Poo} = emud_item:create_item(poo),
    emud_item:set_description(Poo, "You pick up the dark, round object, it feels soft and slightly sticky. You are disgusted to realize it's a poo and someone has been eating peanuts."),
    emud_item:set_short_description(Poo, "A dark oblong object."),
    ok = emud_room:add_item(restroom, Poo),

    emud_room:link_rooms(startroom, restroom, n),
    emud_room:link_rooms(restroom, startroom, s),

    supervisor:start_child(emud_player_sup, childspec_player(player1)),
    emud_player:enter(player1, startroom),

    supervisor:start_child(emud_player_sup, childspec_player(player2)),
    emud_player:enter(player2, startroom),

    emud_console:start(player1).

