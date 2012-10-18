-module(stone).

-export([short_description/0, description/0, interaction_names/0, pickup_requirements/0]).

short_description() ->
    "A stone.\n".

description() ->
    "A nice looking stone. You feel like picking it up.\n".

interaction_names() ->
    ["stone"].


pickup_requirements() ->
    [{strength, 40}].
