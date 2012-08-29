-module(misc_tests).
-include_lib("eunit/include/eunit.hrl").

create_player_test() ->
    {ok, _Player} = emud_player:create_player().


create_room_test() ->
    {ok, StartRoom} = emud_room:create_empty_room(),
    ok = emud_room:set_description(StartRoom, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around.").

create_linked_rooms_test() ->
    {ok, EastRoom} = emud_room:create_empty_room(),
    {ok, WestRoom} = emud_room:create_empty_room(),

    emud_room:link_rooms(EastRoom, WestRoom, west),
    emud_room:link_rooms(WestRoom, EastRoom, east).

place_two_players_in_a_room_test() ->
    {ok, Room} = emud_room:create_empty_room(),


    {ok, P1} = emud_player:create_player(),
    {ok, P2} = emud_player:create_player(),

    emud_player:enter(Room, P1),
    emud_player:enter(Room, P2).

get_room_items_test() ->
    {ok, Room} = emud_room:create_empty_room(),
    {ok, _Items} = emud_room:get_items(Room).

go_to_next_room_test() ->
    {ok, EastRoom} = emud_room:create_empty_room(),
    {ok, WestRoom} = emud_room:create_empty_room(),

    emud_room:link_rooms(EastRoom, WestRoom, west),
    emud_room:link_rooms(WestRoom, EastRoom, east),

    {ok, P1} = emud_player:create_player(),

    emud_player:enter(EastRoom, P1),
    emud_player:go(P1, west).




    
    


    

