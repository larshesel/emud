-module(misc_tests).
-include_lib("eunit/include/eunit.hrl").

create_player_test() ->
    {ok, _Player} = emud_player:create_player(player1),
    gen_server:call(player1, stop).

create_room_test() ->
    {ok, StartRoom} = emud_room:create_empty_room(startroom),
    ok = emud_room:set_description(StartRoom, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around."),
        gen_server:call(startroom, stop).

create_linked_rooms_test() ->
    {ok, EastRoom} = emud_room:create_empty_room(eastroom),
    {ok, WestRoom} = emud_room:create_empty_room(westroom),

    emud_room:link_rooms(EastRoom, WestRoom, west),
    emud_room:link_rooms(WestRoom, EastRoom, east),
    
    gen_server:call(eastroom, stop),
    gen_server:call(westroom, stop).
    

place_two_players_in_a_room_test() ->
    {ok, Room} = emud_room:create_empty_room(room),


    {ok, P1} = emud_player:create_player(p1),
    {ok, P2} = emud_player:create_player(p2),

    emud_player:enter(Room, P1),
    emud_player:enter(Room, P2),

    gen_server:call(room, stop),
    gen_server:call(p1, stop),
    gen_server:call(p2, stop).

get_room_items_test() ->
    {ok, Room} = emud_room:create_empty_room(room),
    {ok, _Items} = emud_room:get_items(Room),
    gen_server:call(room, stop).

go_to_next_room_test() ->
    {ok, EastRoom} = emud_room:create_empty_room(eastroom),
    {ok, WestRoom} = emud_room:create_empty_room(westroom),

    emud_room:link_rooms(EastRoom, WestRoom, west),
    emud_room:link_rooms(WestRoom, EastRoom, east),

    {ok, P1} = emud_player:create_player(p1),

    emud_player:enter(EastRoom, P1),
    emud_player:go(P1, west),
    
    gen_server:call(eastroom, stop),
    gen_server:call(westroom, stop),
    gen_server:call(p1, stop).

add_item_to_room_test() ->
    {ok, Room} = emud_room:create_empty_room(room),
    {ok, Item} = emud_item:create_item(item),
    ok = emud_room:add_item(Room, Item),
    {ok, _Items} = emud_room:get_items(Room),
    
    gen_server:call(room, stop),
    gen_server:call(item, stop).




    
    


    

