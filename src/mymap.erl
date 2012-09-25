-module(mymap).

-export([init/0]).


init() ->
    {ok, StartRoom} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(startroom)),
    {ok, WestRoom} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(westroom)),

    
    {ok, Stone} = supervisor:start_child(emud_item_sup, emud_specs:childspec_item(stone)),
    ok = emud_room:add_item(StartRoom, Stone),

    {ok, HStone} = supervisor:start_child(emud_item_sup, emud_specs:childspec_item(heavy_stone)),
    ok = emud_room:add_item(StartRoom, HStone),

    emud_room:link_rooms(StartRoom, WestRoom, w),
    emud_room:link_rooms(WestRoom, StartRoom, e),

    {ok, RestRoom} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(restroom)),
    {ok, SheepStable} = supervisor:start_child(emud_room_sup, emud_specs:childspec_room(sheep_stable)),

    {ok, Poo} = supervisor:start_child(emud_item_sup, emud_specs:childspec_item(poo)),
    ok = emud_room:add_item(RestRoom, Poo),
    {ok, Chair} = supervisor:start_child(emud_item_sup, emud_specs:childspec_item(chair)),
    ok = emud_room:add_item(RestRoom, Chair),

    emud_room:link_rooms(StartRoom, RestRoom, n),
    emud_room:link_rooms(RestRoom, StartRoom, s),

    emud_room:link_rooms(StartRoom, SheepStable, s),
    emud_room:link_rooms(SheepStable, StartRoom, n),

    {ok, P1} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player1)),
    emud_player:enter(P1, StartRoom),

    {ok, P2} = supervisor:start_child(emud_player_sup, emud_specs:childspec_player(player2)),
    emud_player:enter(P2, StartRoom),

    emud_console:start(P1).

