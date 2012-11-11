-module(heavy_stone).

-export([short_description/0, description/0, 
	 interaction_names/0, pickup_requirements/0]).

short_description() ->
    "A heavy stone.\n".

description() ->
    "A nice looking stone. You feel like picking it up. Looks heavy though.\n".

interaction_names() ->
    [[<<"heavy">>,<<"stone">>]].

pickup_requirements() ->
    [{strength, 60}].
