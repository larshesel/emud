-module(emud_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    emud_player_sup:start_link(),
    emud_room_sup:start_link(),
    emud_item_sup:start_link(),
    emud_player_db:start_link().

stop(_State) ->
    ok.
