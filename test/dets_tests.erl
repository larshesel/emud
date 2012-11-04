-module(dets_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    application:start(emud).

stop(_) ->
    application:stop(emud).

emud_player_dets_is_up_test_() ->
   {"emud_player_dets gen server is up",
      ?setup(fun emud_player_dets_is_up/1)}.

emud_player_dets_is_up(_) ->
    [?_assert(erlang:is_process_alive(whereis(emud_player_dets)))].

