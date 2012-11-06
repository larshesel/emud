%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_console).

-export([start/1, login/0]).

-record(state, {output_server, player}).
-include("emud.hrl").

login() ->
    {Type, Name, Data} = emud_create_player:create_player(),
    PSpec = emud_specs:childspec_player(Type, Name, Data),
    {ok, Player} = supervisor:start_child(emud_player_sup, PSpec),
    case Type of 
	existing_player ->
	    {ok, Room} = emud_player:get_current_room(Player),
	    emud_player:enter(Player, Room);
	new_player ->
	    StartRoom = find_start_room(courtroom),
	    emud_player:enter(Player, StartRoom)
    end,
    start(Player).

find_start_room(Requested) ->
    case [Pid || {Name, Pid, _, _} <- supervisor:which_children(emud_room_sup), Requested == Name] of
	[] ->
	    {error, room_not_found};
	[Pid] -> 
	    Pid
    end.

start(Player) ->
    {ok, OutputConsole} = emud_console_output:start_link(),
    State = #state{output_server = OutputConsole, player = Player},
    ok = emud_player:register_output_server(Player, OutputConsole),
    handle_describe(State),
    loop(State).

loop(State) ->
    Line = io:get_line(standard_io, 'emud> '),
    ParsedLine = parse_line(string:tokens(string:strip(Line, right, $\n), " ")),
    case do_command(State, ParsedLine) of 
	quit -> 
	    %%gen_server:call(State#state.output_server, stop),
	    {ok, Name} = emud_player:get_name(State#state.player),
	    emud_player:save(State#state.player),
	    emud_player:quit(State#state.player),
	    supervisor:delete_child(emud_player_sup, Name),
	    ok;
	_ -> loop(State)
    end.

parse_line([]) ->
    {nop, []};
parse_line(["l" | Args]) ->
    {look_at, Args};
parse_line(["look", "at" | Args]) ->
    {look_at, Args};
parse_line(["look" | Args]) ->
    {look_at, Args};
parse_line(["quit"]) ->
    {quit, []};
parse_line(["help"]) ->
    {help, []};
parse_line(["inventory"]) ->
    {inventory, []};
parse_line(["i"]) ->
    {inventory, []};
parse_line(["go" | Args]) ->
    {go, parse_direction(Args)};
parse_line(["d"]) ->
    {describe, []};
parse_line(["describe"]) ->
    {describe, []};
parse_line(["pick", "up" | Args]) ->
    {pickup, Args};
parse_line(["get" | Args]) ->
    {pickup, Args};
parse_line(["drop" | Args]) ->
    {drop, Args};
parse_line(["crash"]) ->
    {crash, []};
parse_line(["save"]) ->
    {save, []};
parse_line(_) ->
    {beg_your_pardon, []}.

do_command(State, {Command, Args}) ->
    case Command of 
	look_at ->
	    handle_look_at(State, Args);
	nop -> 
	    ok;
	quit -> 
	    quit;
	drop -> 
	    handle_drop(State, Args);
	go ->
	    handle_go(State, Args);
	describe -> 
	    handle_describe(State);
	beg_your_pardon -> 
	    print(State, io_lib:format("You can't do that~n", []));
	pickup -> 
	    handle_pickup(State, Args);
	help -> 
	    print_help(State);
	inventory -> 
	    handle_inventory(State);
	crash -> 
	    handle_crash(State);
	save ->
	    handle_save(State)
    end.


handle_save(State) ->
    print(State, io_lib:format("Saving player... ", [])),
    emud_player:save(State#state.player),
    print(State, io_lib:format("Done~n", [])).    


handle_look_at(State, []) ->
    print(State, io_lib:format("Look at what?.~n", []));
handle_look_at(State, INList) ->
    IN = string:join(INList, " "),
    case emud_player:look_at(State#state.player, IN) of
	{ok, Message} -> 
	    print(State, Message);
	{error, Message} -> 
	    print(State, Message)
    end.

    
print(State, String) ->
    emud_console_output:write_string(State#state.output_server, String).

handle_crash(State) ->
    emud_player:crash(State#state.player).

handle_drop(State, []) ->
    print(State, io_lib:format("You can't drop that.~n", []));
handle_drop(State, INList) ->
    IN = string:join(INList, " "),
    case emud_player:drop(State#state.player, IN) of 
	{error, _} ->
	    print(State, io_lib:format("What to drop?~n", []));
        _ ->
	    print(State, io_lib:format("You drop ~s.~n", [IN]))
	end.

    
handle_inventory(State) ->
    {ok, Items} = emud_player:get_items(State#state.player),
    case Items of 
	[] -> 
	    print(State, io_lib:format("You are carrying nothing.~n",[]));
	_ ->
	    print(State, io_lib:format("You are carrying: ~n",[])),
	    [print(State, io_lib:format("~s", [X])) || X <- get_item_descriptions(Items)]
    end.

print_help(State) ->
    print(State, io_lib:format("Available commands:~n", [])),
    print(State, io_lib:format("  help~n", [])),
    print(State, io_lib:format("  describe~n", [])),
    print(State, io_lib:format("  go <direction>~n", [])),
    print(State, io_lib:format("  pick up <item>~n", [])),
    print(State, io_lib:format("  get <item>~n", [])),
    print(State, io_lib:format("  look at <item|person>~n", [])),
    print(State, io_lib:format("  inventory~n~n", [])).

handle_pickup(State, []) ->
    print(State, io_lib:format("You can't pick that up.~n", []));
handle_pickup(State, ArgList) ->
    Args = string:join(ArgList, " "),
    case emud_player:pickup(State#state.player, Args) of 
	{error, could_not_pickup_item} ->
	    print(State, io_lib:format("What do you want to pick up?~n", []));
	{error, {display_message, DisplayMessage}} ->
	    print(State, io_lib:format("~s", [DisplayMessage]));
        ok ->
	    print(State, io_lib:format("You pick up ~s.~n", [Args]))
	end.

handle_go(State, no_such_direction) ->
    print(State, io_lib:format("You hurt your head - you can't go there.~n", []));
handle_go(State, Direction) ->
    case emud_player:go(State#state.player, Direction) of 
	{error, _} ->
	    print(State, io_lib:format("You hurt your head - you can't go there.~n", []));
	_ ->
	    handle_describe(State)
	end.

parse_direction([]) -> no_such_direction;
parse_direction(DirectionList) ->
    Direction = string:join(DirectionList, " "),
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
    {ok, RoomDescription, Directions, Players, AIs, ItemPids} = emud_player:describe(State#state.player),
    print(State, io_lib:format("~s", [RoomDescription])),
    print(State, io_lib:format("You can go ~p from here.~n", [format_directions(Directions)])),
    [print(State, io_lib:format("~s.~n", [get_text(emud_player:get_name(X))])) || X<- Players, X /= State#state.player],
    [print(State, io_lib:format("ais: ~p.~n", [X])) || X<-AIs],
    [print(State, io_lib:format("~s", [X])) || X <- get_item_descriptions(ItemPids)].


get_item_descriptions(ItemPids) ->
    [ get_text(emud_item:get_short_description(X)) || X <- ItemPids ].

get_text({ok, Text}) ->
    Text.

format_directions(Directions) ->
    proplists:get_keys(Directions).



		  
