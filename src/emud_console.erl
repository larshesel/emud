%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_console).

-export([start/1]).

-record(state, {output_server, player}).

start(Player) ->
    Server = list_to_atom("output_server_" ++ atom_to_list(Player)),
    State = #state{output_server = Server, player = Player},
    emud_console_output:start_link(Server),
    ok = emud_player:register_output_server(Player, Server),
    loop(State).

loop(State) ->
    Line = io:get_line(standard_io, 'emud> '),
    ParsedLine = parse_line(string:tokens(string:strip(Line, right, $\n), " ")),
    case do_command(State, ParsedLine) of 
	quit -> 
	    gen_server:call(State#state.output_server, stop),
	    ok;
	_ -> loop(State)
    end.

parse_line(["quit"]) ->
    {quit, []};

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

do_command(State, {Command, Args}) ->
    case Command of 
	quit -> quit;
	go ->
	    handle_go(State, Args);
	describe -> handle_describe(State);
	beg_your_pardon -> print(State, io_lib:format("You can't do that~n", []));
	pickup -> handle_pickup(State, Args);
	help -> print_help(State);
	inventory -> handle_inventory(State)
    end.

print(State, String) ->
    emud_console_output:write_string(State#state.output_server, String).

handle_inventory(State) ->
    {ok, Items} = emud_player:get_items(State#state.player),
    print(State, io_lib:format("You are carrying: ~p~n", [Items])).

print_help(State) ->
    print(State, io_lib:format("Available commands:~n")),
    print(State, io_lib:format("  help~n")),
    print(State, io_lib:format("  describe~n")),
    print(State, io_lib:format("  go <direction>~n")),
    print(State, io_lib:format("  pick up <item>~n")),
    print(State, io_lib:format("  get <item>~n")),
    print(State, io_lib:format("  inventory~n~n")).

handle_pickup(State, []) ->
    print(State, io_lib:format("You can't pick that up.~n"));
handle_pickup(State, [Args]) ->
    case emud_player:pickup(State#state.player, Args) of 
	{error, _} ->
	    print(State, io_lib:format("What do you want to pick up?~n"));
        _ ->
	    print(State, io_lib:format("You pick up ~s.~n", [Args]))
	end.

handle_go(State, no_such_direction) ->
    print(State, io_lib:fwrite("You hurt your head - you can't go there.~n"));
handle_go(State, Direction) ->
    case emud_player:go(State#state.player, Direction) of 
	{error, _} ->
	    print(State, io_lib:fwrite("You hurt your head - you can't go there.~n"));
	_ ->
	    handle_describe(State)
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

handle_describe(State) ->
    {ok, RoomDescription, Directions, Players, Items} = emud_player:describe(State#state.player),
    print(State, io_lib:format("~s~n", [RoomDescription])),
    print(State, io_lib:format("You can go ~p from here.~n", [format_directions(Directions)])),
    print(State, io_lib:format("~p~n", [[ X || X<-Players, X /= State#state.player]])),
    print(State, io_lib:format("Items: ~p.~n", [Items])).

format_directions(Directions) ->
    proplists:get_keys(Directions).



		  
