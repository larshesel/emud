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



parse_line(["go" | Args]) ->
    {go, parse_direction(Args)};
parse_line(["describe"]) ->
    {describe, []};
parse_line(_) ->
    {beg_your_pardon, []}.

parse_direction([]) -> no_such_direction;
parse_direction([Direction]) ->
    case Direction of 
	"west" ->
	    west;
	"east" ->
	    east;
	"north" ->
	    north;
	"south" ->
	    south;
	"w" ->
	    west;
	"e" ->
	    east;
	"n" ->
	    north;
	"s" ->
	    south;
	_ -> no_such_direction
    end.

do_command(Player, {Command, Args}) ->
    case Command of 
	go ->
	    handle_go(Player, Args);
	describe -> handle_describe(Player);
	beg_your_pardon -> io:fwrite("You can't do that~n", [])
    end.

handle_go(_Player, no_such_direction) ->
    io:fwrite("You hurt your head - you can't go there.~n");
handle_go(Player, Direction) ->
    case emud_player:go(Player, Direction) of 
	{error, _} ->
	    io:fwrite("You hurt your head - you can't go there.~n");
	_ ->
	    ok
	end.

handle_describe(Player) ->
    {ok, RoomDescription, Directions, _Players, _Items} = emud_player:describe(Player),
    io:fwrite("~s~n", [RoomDescription]),
    io:fwrite("You can go ~p from here.~n", format_directions(Directions)).

format_directions(Directions) ->
    Directions.


		  
