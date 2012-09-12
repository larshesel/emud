-module(emud_specs).
-export([childspec_player/1, childspec_room/1, childspec_item/1]).

childspec_player(Name) ->
    {Name, {emud_player, start_link, [Name]},permanent, 2000, worker, [emud_player]}.
childspec_room(Name) ->
    {Name, {emud_room, start_link, [Name]},permanent, 2000, worker, [emud_room]}.
childspec_item(Name) ->
    {Name, {emud_item, start_link, [create_item_state(Name)]},permanent, 2000, worker, [emud_item]}.

create_item_state(poo) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A dark oblong object."),
    S3 = emud_create_item:set_description(S2, "You pick up the dark, round object, it feels soft and slightly sticky. You are disgusted to realize it's a poo and someone has been eating peanuts."),
    S3.

