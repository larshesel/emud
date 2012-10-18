-module(chair).

-export([short_description/0, description/0, interaction_names/0, pickup_requirements/0]).


short_description() ->
    "A chair.\n".

description() ->
    "A dingy looking chair, made of driftwood.\n".

interaction_names() ->
    ["chair"].

pickup_requirements() ->
    [].
