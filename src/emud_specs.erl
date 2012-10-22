-module(emud_specs).
-export([childspec_player/1, childspec_room/1, childspec_item/1]).

childspec_player(Name) ->
    {Name, {emud_player, start_link, [Name]},permanent, 2000, worker, [emud_player]}.

childspec_room(Name) ->
    {Name, {emud_room, start_link, [create_room_state(Name)]},permanent, 2000, worker, [emud_room]}.
childspec_item(Name) ->
    {Name, {emud_item, start_link, [Name]},permanent, 2000, worker, [emud_item]}.

create_room_state(westroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You're in a huge lobby with beautiful marble columns and floors.\n");
create_room_state(startroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You are in a small dark room.\nThere are a lot of chairs facing a podium, \nlike in an auditorium or a court room. \nOn the desk on the podium there are a lot of papers lying around.\n");
create_room_state(restroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You're in the restroom. There's a toilet in the corner and a sink on the wall. It smells of poo.\n");
create_room_state(sheep_stable) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You entered the sheep stables.\n").

