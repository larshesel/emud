%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_console).

%% API
-export([start/1]).

start(Player) ->
    loop(Player).

loop(Player) ->
    Line = io:get_line(standard_io, 'emud> '),
    ParsedLine = parse_line(string:tokens(string:strip(Line, right, $\n), " ")),
    do_command(Player, ParsedLine),
    loop(Player).

parse_line(["help"]) ->
    {help, []};
parse_line(["inventory"]) ->
    {inventory, []};
parse_line(["go" | Args]) ->
    {go, parse_direction(Args)};
parse_line(["describe"]) ->
    {describe, []};
parse_line(["pick", "up" | Args]) ->
    {pickup, Args};
parse_line(["get" | Args]) ->
    {pickup, Args};
parse_line(_) ->
    {beg_your_pardon, []}.

do_command(Player, {Command, Args}) ->
    case Command of 
	go ->
	    handle_go(Player, Args);
	describe -> handle_describe(Player);
	beg_your_pardon -> io:fwrite("You can't do that~n", []);
	pickup -> handle_pickup(Player, Args);
	help -> print_help();
	inventory -> handle_inventory(Player)
    end.

handle_inventory(Player) ->
    {ok, Items} = emud_player:get_items(Player),
    io:fwrite("You are carrying: ~p~n", [Items]).

print_help() ->
    io:fwrite("Available commands:~n"),
    io:fwrite("  help~n"),
    io:fwrite("  describe~n"),
    io:fwrite("  go <direction>~n"),
    io:fwrite("  pick up <item>~n"),
    io:fwrite("  get <item>~n"),
    io:fwrite("  inventory~n~n").

handle_pickup(Player, [Args]) ->
    case emud_player:pickup(Player, Args) of 
	{error, _} ->
	    io:fwrite("You can't pick that up.~n");
        _ ->
	    io:fwrite("You pick up ~s.~n", [Args])
	end.

handle_go(_Player, no_such_direction) ->
    io:fwrite("You hurt your head - you can't go there.~n");
handle_go(Player, Direction) ->
    case emud_player:go(Player, Direction) of 
	{error, _} ->
	    io:fwrite("You hurt your head - you can't go there.~n");
	_ ->
	    handle_describe(Player)
	end.

parse_direction([]) -> no_such_direction;
parse_direction([Direction]) ->
    case Direction of 
	"west" -> w;
	"east" -> e;
	"north" -> n;
	"south" -> s;
	"w" -> w;
	"e" -> e;
	"n" -> n;
	"s" -> s;
	_ -> no_such_direction
    end.

handle_describe(Player) ->
    {ok, RoomDescription, Directions, _Players, Items} = emud_player:describe(Player),
    io:fwrite("~s~n", [RoomDescription]),
    io:fwrite("You can go ~p from here.~n", [format_directions(Directions)]),
    io:fwrite("Items: ~p.~n", [Items]).

format_directions(Directions) ->
    proplists:get_keys(Directions).



		  
