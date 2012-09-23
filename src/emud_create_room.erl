-module(emud_create_room).
-export([create_state/0, set_description/2, add_ai/2]).
-include("emud_room.hrl").

create_state() ->
    #room_state{}.

set_description(State, Description) ->
    State#room_state{description = Description}.

add_ai(State, AI) ->
    State#room_state{ais = [AI | State#room_state.ais]}.
