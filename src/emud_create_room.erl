-module(emud_create_room).
-export([create_state/0, set_description/2]).
-include("emud_room.hrl").

create_state() ->
    #room_state{}.

set_description(State, Description) ->
    State#room_state{description = Description}.
