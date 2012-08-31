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

    emud_player:enter(P1, Room),
    emud_player:enter(P2, Room),

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

    emud_player:enter(P1, EastRoom),
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

pickup_item_test() ->
    {ok, Room} = emud_room:create_empty_room(room),
    {ok, Item} = emud_item:create_item(item),
    ok = emud_room:add_item(Room, Item),
    {ok, _Items} = emud_room:get_items(Room),
    
    {ok, P1} = emud_player:create_player(p1),

    emud_player:enter(P1, Room),
    emud_player:pickup(P1, "item"),

    gen_server:call(room, stop),
    gen_server:call(item, stop),
    gen_server:call(p1, stop).

send_message_to_output_server_test() ->
    {ok, _Pid} = emud_console_output:start_link(output_server),
    emud_console_output:write_string(output_server, "test_string"),
    gen_server:call(output_server, stop).


send_message_to_output_server_via_player_test() ->
    {ok, _P1} = emud_player:create_player(p1),
    {ok, _Pid} = emud_console_output:start_link(output_server),
    ok = emud_player:register_output_server(p1, output_server),
    emud_player:send_msg(p1, "test_string"),

    gen_server:call(p1, stop),
    gen_server:call(output_server, stop).

    

    
