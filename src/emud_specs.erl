-module(emud_specs).
-export([childspec_player/1, childspec_room/1, childspec_item/1]).

childspec_player(Name) ->
    {Name, {emud_player, start_link, [Name]},permanent, 2000, worker, [emud_player]}.
childspec_room(Name) ->
    {Name, {emud_room, start_link, [Name]},permanent, 2000, worker, [emud_room]}.
childspec_item(Name) ->
    {Name, {emud_item, start_link, [Name]},permanent, 2000, worker, [emud_item]}.
