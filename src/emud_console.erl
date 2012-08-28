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
    Line = io:get_line(standard_io, 'emud>'),
    ParsedLine = parse_line(Line),
    do_command(Player, ParsedLine),
    loop(Player).


parse_line("go west\n") ->
    {go, [west]};
parse_line("go east\n") ->
    {go, [east]};
parse_line("describe\n") ->
    {describe, []};
parse_line(_) ->
    {beg_your_pardon, []}.



do_command(Player, {Command, Args}) ->
    case Command of 
	go ->
	    handle_go(Player, Args);
	describe -> handle_describe(Player);
	beg_your_pardon -> io:fwrite("You can't do that~n", [])
    end.

handle_go(_Player, []) ->
    {error, no_such_direction};
handle_go(_Player, [_Direction]) ->
    ok.

handle_describe(Player) ->
    {ok, RoomDescription, Directions, _Players, _Items} = emud_player:describe(Player),
    io:fwrite("~p~n", [RoomDescription]),
    io:fwrite("You can go ~p from here.~n", format_directions(Directions)).

format_directions(Directions) ->
    Directions.


		  
