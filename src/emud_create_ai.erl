-module(emud_create_ai).
-include("emud_item.hrl").

-export([create_state/0, set_description/2, 
	 set_short_description/2, set_interaction_names/2]).

create_state() ->
    #item_state{}.

set_description(State, Description) ->
    State#item_state{description = Description}.

set_short_description(State, Description) ->
    State#item_state{short_description = Description}.

set_interaction_names(State, Names) ->
    State#item_state{interaction_names = Names}.
