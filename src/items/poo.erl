-module(poo).

-export([short_description/0, description/0, interaction_names/0, pickup_requirements/0]).

short_description() ->
    "A dark oblong object.\n".

description() ->
    "It is a dark, round object.\nYou are disgusted to realize it's a poo\nand someone has been eating peanuts.\n".

interaction_names() ->
    ["object", "poo"].

pickup_requirements() ->
    [].
