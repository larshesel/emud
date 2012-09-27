-module(emud_create_player).
-include("emud_player.hrl").

-compile(export_all). 

create_state() ->
    #state{}.

set_name(State, Name) ->
    State#state{name = Name}.

set_description(State, Description) ->
    State#state{description = Description}.

set_short_description(State, Description) ->
    State#state{short_description = Description}.
