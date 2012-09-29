-module(emud_specs).
-export([childspec_player/1, childspec_room/1, childspec_item/1,
	childspec_ai/1]).

childspec_player(Name) ->
    {Name, {emud_player, start_link, [create_player_state(Name)]},permanent, 2000, worker, [emud_player]}.

childspec_room(Name) ->
    {Name, {emud_room, start_link, [create_room_state(Name)]},permanent, 2000, worker, [emud_room]}.
childspec_item(Name) ->
    {Name, {emud_item, start_link, [create_item_state(Name)]},permanent, 2000, worker, [emud_item]}.

childspec_ai(Name) ->
    {Name, {emud_item, start_link, [create_ai_state(Name)]},permanent, 2000, worker, [emud_ai]}.

create_player_state(player2) ->
    S1 = emud_create_player:create_state(),
    S2 = emud_create_player:set_name(S1, "Dilbert"),
    S3 = emud_create_player:set_description(S2, "A huge orc, with bulging muscles and evil looking eyes.\n"),
    emud_create_player:set_short_description(S3, "An orc.\n");
create_player_state(player1) ->
    S1 = emud_create_player:create_state(),
    S2 = emud_create_player:set_name(S1, "Lars"),
    S3 = emud_create_player:set_description(S2, "A small looking human with pale skin. Probably an Erlang programmer.\n"),
    emud_create_player:set_short_description(S3, "A dork.\n").

create_room_state(westroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You're in a huge lobby with beautiful marble columns and floors.\n");
create_room_state(startroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around.\n");
create_room_state(restroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You're in the restroom. There's a toilet in the corner and a sink on the wall. It smells of poo.\n");
create_room_state(sheep_stable) ->
    S1 = emud_create_room:create_state(),
    S2 = emud_create_room:set_description(S1, "You entered the sheep stables.\n"),
    {ok, Sheep} = supervisor:start_child(emud_ai_sup, emud_specs:childspec_ai(sheep)),
    emud_create_room:add_ai(S2, Sheep).

create_item_state(poo) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A dark oblong object.\n"),
    S3 = emud_create_item:set_description(S2, "You pick up the dark, round object, it feels soft and slightly sticky.\nYou are disgusted to realize it's a poo\nand someone has been eating peanuts.\n"),
    S4 = emud_create_item:set_interaction_names(S3, ["object", "poo"]),
    S4;
create_item_state(chair) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A chair.\n"),
    S3 = emud_create_item:set_description(S2, "A dingy looking chair, made of driftwood.\n"),
    S4 = emud_create_item:set_interaction_names(S3, ["chair"]),
    S4;
create_item_state(stone) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A stone.\n"),
    S3 = emud_create_item:set_description(S2, "A nice looking stone. You feel like picking it up.\n"),
    S4 = emud_create_item:set_interaction_names(S3, ["stone"]),
    emud_create_item:set_pickup_requirements(S4, [{strength, 40}]);
create_item_state(heavy_stone) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A heavy stone.\n"),
    S3 = emud_create_item:set_description(S2, "A nice looking stone. You feel like picking it up. Looks heavy though.\n"),
    S4 = emud_create_item:set_interaction_names(S3, ["heavy stone"]),
    emud_create_item:set_pickup_requirements(S4, [{strength, 60}]).

create_ai_state(sheep) ->
    S1 = emud_create_ai:create_state(),
    S2 = emud_create_ai:set_short_description(S1, "A sheep.\n"),
    S3 = emud_create_ai:set_description(S2, "A very cute sheep.\n"),
    S4 = emud_create_ai:set_interaction_names(S3, ["sheep"]),
    S4.
