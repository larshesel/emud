-module(misc_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    application:start(emud).

stop(_) ->
    application:stop(emud).

create_player_test_() ->
   {"A player can be created",
      ?setup(fun create_player/1)}.

create_player(_) ->
    {ok, PlayerPid} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player1)),
    [?_assert(erlang:is_process_alive(PlayerPid)),
     ?_assert(erlang:is_process_alive(whereis(emud_player_sup)))].

create_room_test_() ->
   {"A room can be created",
      ?setup(fun create_room/1)}.

create_room(_) ->
    {ok, RoomPid} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(room1)),
    [?_assert(erlang:is_process_alive(RoomPid)),
     ?_assert(erlang:is_process_alive(whereis(emud_room_sup)))].

%% create_linked_rooms_test() ->
%%     {ok, EastRoom} = emud_room:create_empty_room(eastroom),
%%     {ok, WestRoom} = emud_room:create_empty_room(westroom),

%%     emud_room:link_rooms(EastRoom, WestRoom, west),
%%     emud_room:link_rooms(WestRoom, EastRoom, east),
    
%%     gen_server:call(eastroom, stop),
%%     gen_server:call(westroom, stop).

place_two_players_in_a_room_test_() ->
   {"Place two players in a room",
      ?setup(fun place_two_players_in_a_room/1)}.

place_two_players_in_a_room(_) ->
    {ok, RoomPid} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(room1)),

    {ok, P1} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(p1)),
    {ok, P2} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(p2)),

    [?_assertMatch(ok, emud_player:enter(P1, RoomPid)),
     ?_assertMatch(ok, emud_player:enter(P2, RoomPid)),
     ?_assert(fun() ->
		      {ok, [TP1, TP2]} = emud_room:get_players(RoomPid),
		      ((TP1 == P1) and (TP2 == P2)) or ((TP1 == P2) and (TP2 == P1)) 
	      end()
       )].

%% get_room_items_test() ->
%%     {ok, Room} = emud_room:create_empty_room(room),
%%     {ok, _Items} = emud_room:get_items(Room),
%%     gen_server:call(room, stop).

%% go_to_next_room_test() ->
%%     {ok, EastRoom} = emud_room:create_empty_room(eastroom),
%%     {ok, WestRoom} = emud_room:create_empty_room(westroom),

%%     emud_room:link_rooms(EastRoom, WestRoom, west),
%%     emud_room:link_rooms(WestRoom, EastRoom, east),

%%     {ok, P1} = emud_player:create_player(p1),

%%     emud_player:enter(P1, EastRoom),
%%     emud_player:go(P1, west),
    
%%     gen_server:call(eastroom, stop),
%%     gen_server:call(westroom, stop),
%%     gen_server:call(p1, stop).

%% add_item_to_room_test() ->
%%     {ok, Room} = emud_room:create_empty_room(room),
%%     {ok, Item} = emud_item:create_item(item),
%%     ok = emud_room:add_item(Room, Item),
%%     {ok, _Items} = emud_room:get_items(Room),
    
%%     gen_server:call(room, stop),
%%     gen_server:call(item, stop).

%% pickup_item_test() ->
%%     {ok, Room} = emud_room:create_empty_room(room),
%%     {ok, Item} = emud_item:create_item(item),
%%     ok = emud_room:add_item(Room, Item),
%%     {ok, _Items} = emud_room:get_items(Room),
    
%%     {ok, P1} = emud_player:create_player(p1),

%%     emud_player:enter(P1, Room),
%%     emud_player:pickup(P1, "item"),

%%     gen_server:call(room, stop),
%%     gen_server:call(item, stop),
%%     gen_server:call(p1, stop).

%% send_message_to_output_server_test() ->
%%     {ok, _Pid} = emud_console_output:start_link(output_server),
%%     emud_console_output:write_string(output_server, "test_string"),
%%     gen_server:call(output_server, stop).


%% send_message_to_output_server_via_player_test() ->
%%     {ok, _P1} = emud_player:create_player(p1),
%%     {ok, _Pid} = emud_console_output:start_link(output_server),
%%     ok = emud_player:register_output_server(p1, output_server),
%%     emud_player:send_msg(p1, "test_string"),

%%     gen_server:call(p1, stop),
%%     gen_server:call(output_server, stop).
