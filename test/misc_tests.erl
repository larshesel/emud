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
    {ok, RoomPid} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(startroom)),
    [?_assert(erlang:is_process_alive(RoomPid)),
     ?_assert(erlang:is_process_alive(whereis(emud_room_sup)))].

create_item_test_() ->
   {"An item can be created",
      ?setup(fun create_item/1)}.

create_item(_) ->
    {ok, ItemPid} = supervisor:start_child(emud_item_sup, emud_specs:childspec_item(poo)),
    [?_assert(erlang:is_process_alive(ItemPid)),
     ?_assert(erlang:is_process_alive(whereis(emud_item_sup)))].

place_two_players_in_a_room_test_() ->
   {"Place two players in a room",
      ?setup(fun place_two_players_in_a_room/1)}.

place_two_players_in_a_room(_) ->
    {ok, RoomPid} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(startroom)),

    {ok, P1} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player1)),
    {ok, P2} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player2)),

    [?_assertMatch(ok, emud_player:enter(P1, RoomPid)),
     ?_assertMatch(ok, emud_player:enter(P2, RoomPid)),
     ?_assert(fun() ->
		      {ok, [TP1, TP2]} = emud_room:get_players(RoomPid),
		      ((TP1 == P1) and (TP2 == P2)) or ((TP1 == P2) and (TP2 == P1)) 
	      end()
       )].
