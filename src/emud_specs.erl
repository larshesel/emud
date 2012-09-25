-module(emud_specs).
-export([childspec_player/1, childspec_room/1, childspec_item/1,
	childspec_ai/1]).

childspec_player(Name) ->
    {Name, {emud_player, start_link, [Name]},permanent, 2000, worker, [emud_player]}.

childspec_room(Name) ->
    {Name, {emud_room, start_link, [create_room_state(Name)]},permanent, 2000, worker, [emud_room]}.
childspec_item(Name) ->
    {Name, {emud_item, start_link, [create_item_state(Name)]},permanent, 2000, worker, [emud_item]}.

childspec_ai(Name) ->
    {Name, {emud_item, start_link, [create_ai_state(Name)]},permanent, 2000, worker, [emud_ai]}.

create_room_state(westroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You're in a huge lobby with beautiful marble columns and floors.");
create_room_state(startroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around.");
create_room_state(restroom) ->
    S1 = emud_create_room:create_state(),
    emud_create_room:set_description(S1, "You're in the restroom. There's a toilet in the corner and a sink on the wall. It smells of poo.");
create_room_state(sheep_stable) ->
    S1 = emud_create_room:create_state(),
    S2 = emud_create_room:set_description(S1, "You entered the sheep stables."),
    {ok, Sheep} = supervisor:start_child(emud_ai_sup, emud_specs:childspec_ai(sheep)),
    emud_create_room:add_ai(S2, Sheep).

create_item_state(poo) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A dark oblong object."),
    S3 = emud_create_item:set_description(S2, "You pick up the dark, round object, it feels soft and slightly sticky. You are disgusted to realize it's a poo and someone has been eating peanuts."),
    S4 = emud_create_item:set_interaction_names(S3, ["object", "poo"]),
    S4;
create_item_state(chair) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A chair."),
    S3 = emud_create_item:set_description(S2, "A dingy looking chair, made of driftwood."),
    S4 = emud_create_item:set_interaction_names(S3, ["chair"]),
    S4;
create_item_state(stone) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A stone."),
    S3 = emud_create_item:set_description(S2, "A nice looking stone. You feel like picking it up."),
    S4 = emud_create_item:set_interaction_names(S3, ["stone"]),
    emud_create_item:set_pickup_requirements(S4, [{strength, 40}]);
create_item_state(heavy_stone) ->
    S1 = emud_create_item:create_state(),
    S2 = emud_create_item:set_short_description(S1, "A heavy stone."),
    S3 = emud_create_item:set_description(S2, "A nice looking stone. You feel like picking it up. Looks heavy though."),
    S4 = emud_create_item:set_interaction_names(S3, ["heavy_stone"]),
    emud_create_item:set_pickup_requirements(S4, [{strength, 60}]).

create_ai_state(sheep) ->
    S1 = emud_create_ai:create_state(),
    S2 = emud_create_ai:set_short_description(S1, "A sheep."),
    S3 = emud_create_ai:set_description(S2, "A very cute sheep."),
    S4 = emud_create_ai:set_interaction_names(S3, ["sheep"]),
    S4.
