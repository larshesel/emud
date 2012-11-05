-module(dets_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    application:start(emud).

stop(_) ->
    application:stop(emud).

emud_player_db_is_up_test_() ->
   {"emud_player_db gen server is up",
      ?setup(fun emud_player_db_is_up/1)}.

emud_player_db_is_up(_) ->
    [?_assert(erlang:is_process_alive(whereis(emud_player_db)))].

insert_delete_player_test_() ->
   {"insert_delete_player_test_",
      ?setup(fun put_get_delete_data_to_player_table/1)}.

put_get_delete_data_to_player_table(_) ->
    [?_assert(erlang:is_process_alive(whereis(emud_player_db))),
     ?_assertEqual(ok, emud_player_db:put_player(reserved_key, no_state)),
     ?_assertEqual([{reserved_key, no_state}], emud_player_db:get_player(reserved_key)),
     ?_assertEqual(true, emud_player_db:player_exists(reserved_key)),
     ?_assertEqual(ok, emud_player_db:delete_player(reserved_key)),
     ?_assertEqual([], emud_player_db:get_player(reserved_key))].
