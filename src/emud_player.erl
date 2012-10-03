%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_player).

-behaviour(gen_server).

%% API
-export([start_link/1, register_output_server/2, send_msg/2]).

-export([enter/2, describe/1, get_directions/1,
	 go/2, pickup/2, get_items/1, drop/2,
	 get_description/1, get_short_description/1,
	 get_name/1, look_at/2]).

%% DEBUG
-export([crash/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("emud_player.hrl").

%%%===================================================================
%%% API
%%%===================================================================

look_at(Player, IN) ->
    gen_server:call(Player, {look_at, Player, IN}).

enter(Player, Room) ->
    gen_server:call(Player, {enter_room, Player, Room}).

get_name(Player) ->
    gen_server:call(Player, {get_name}).

get_description(Player) ->
    gen_server:call(Player, {get_description}).

get_short_description(Player) ->
    gen_server:call(Player, {get_short_description}).

describe(Player) ->
    gen_server:call(Player, {describe}).

get_directions(Player) ->
    gen_server:call(Player, {get_directions}).

go(Player, Direction) ->
    gen_server:call(Player, {go, Player, Direction}).

pickup(Player, Item) ->
    gen_server:call(Player, {pickup, Player, Item}).

get_items(Player) ->
    gen_server:call(Player, {get_items}).

register_output_server(Player, Server) ->
    gen_server:call(Player, {register_output_server, Server}).

drop(Player, Item) ->
    gen_server:call(Player, {drop, Item}).

crash(Player) ->
    gen_server:call(Player, {crash}).

send_msg(Player, Message) ->
    gen_server:cast(Player, {send_message, Message}).

start_link(State) ->
    gen_server:start_link(?MODULE, [State], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}};
init([State]) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({pickup, Player, Item}, _From, State) ->
    pickup_item(Player, Item, State);
handle_call({look_at, Player, IN}, _From, State) ->
    handle_look_at(Player, IN, State);
handle_call({enter_room, Player, Room}, _From, State) ->
    enter_room(Player, Room, State);
handle_call({go, Player, Direction}, _From, State) -> 
    {ok, Directions} = emud_room:get_directions(State#state.room),
    Room = proplists:get_value(Direction, Directions),
    enter_room(Player, Room, State);
handle_call({describe}, _From, State) ->
    {ok, RoomDescriptions} = emud_room:get_description(State#state.room),
    {ok, Directions} = emud_room:get_directions(State#state.room),
    {ok, Items} = emud_room:get_items(State#state.room),
    {ok, AIs} = emud_room:get_ais(State#state.room),
    {ok, Players} = emud_room:get_players(State#state.room),
    Reply = {ok, RoomDescriptions, Directions, Players, AIs, Items},
    {reply, Reply, State};
handle_call({get_items}, _From, State) ->
    Reply = {ok, State#state.items},
    {reply, Reply, State};
handle_call({register_output_server, Server}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{output_server = Server}};
handle_call({drop, IN}, _From, State) ->
    Matches = get_item_pids(State, IN),
    case Matches of 
	[] -> {reply, {error, no_such_item}, State};
	[Pid |_ ] -> 
	    NewState = State#state{items = lists:delete(Pid, State#state.items)},
	    %% FIXME : need to set the item state to dropped
	    emud_room:add_item(State#state.room, Pid),
	    {reply, ok, NewState}
    end;
handle_call({get_name}, _From, State) ->
    {reply, {ok, State#state.name}, State};
handle_call({get_description}, _From, State) ->
    {reply, {ok, State#state.description}, State};
handle_call({get_short_description}, _From, State) ->
    {reply, {ok, State#state.short_description}, State};
handle_call({crash}, _From, _State) ->
    0/0.


handle_cast({send_message, Message}, State) ->
    if is_pid(State#state.output_server) ->
	    emud_console_output:write_msg(State#state.output_server, Message);
       true -> ok
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% -spec get_strength(any()) -> strength().
%% get_strength(State) ->
%%     State#state.strength.

get_item_pids(State, IN) ->
    lists:filter(fun(Pid) -> 
			 {ok, Names} = emud_item:get_interaction_names(Pid),  
			 length(lists:filter(fun(Name) -> Name == IN end, Names)) > 0 
		 end, 
		 State#state.items).
    
-spec leave_old_room(room(), player()) -> ok.
leave_old_room(no_room, _Player) ->
    ok;
leave_old_room(Room, Player) ->
    ok = emud_room:leave(Room, Player).

handle_look_at(_Player, IN, State) ->
    if 
	%% are you looking a me?
	IN == State#state.name ->
	    {reply, {ok, State#state.description}, State};
	true -> 
	    PlayerItemPids = get_item_pids(State, IN),
	    {ok, RoomItemPids} = emud_room:lookup_item_by_in(State#state.room, IN),
	    {ok, PlayerPids} = emud_room:get_players(State#state.room),
	    MachedPlayerPids = [X || X<-lists:filter(fun(Pid) -> Pid /= self() end, PlayerPids), emud_player:get_name(X) == {ok, IN}],
	    if MachedPlayerPids /= [] ->
		    [Pid | _] = MachedPlayerPids,
		    {reply, emud_player:get_description(Pid), State};
	       PlayerItemPids /= [] ->
		    [Pid | _] = PlayerItemPids,
		    {reply, emud_item:get_description(Pid), State};
	       RoomItemPids /= []  ->
		    [Pid | _] = RoomItemPids,
		    {reply, emud_item:get_description(Pid), State};
	       true ->
		{reply, {ok, not_found}, State}
	    end
    end.
	    
-spec pickup_item(player(), string(), any()) -> {reply, any(), any()}.
pickup_item(Player, ItemInteractionName, State) ->
    case emud_room:lookup_item_by_in(State#state.room, ItemInteractionName) of
	{ok, [Item | _]} -> 
	    %% start negotiation with the item.
	    Reply = pick_up_item_negotiation(Player, Item),
	    case Reply of
		ok ->
		    emud_room:remove_item(State#state.room, Item),
		    NewState = State#state{items = [Item | State#state.items]},
		    {reply, Reply, NewState};
		{error, _, {display_message, DisplayMessage}} ->
		    {reply, {error, {display_message, DisplayMessage}}, State};
		{error, no_such_item} ->
		    {reply, {error, could_not_pickup_item}, State}
	    end;
	_ -> {reply, {error, could_not_pickup_item}, State}
    end.

pick_up_item_negotiation(_Player, ItemPid) ->
    error_logger:info_msg("~p: pickup: attempting pickup~n", [?MODULE]),
    case emud_item:do(ItemPid, pickup) of 
	ok ->
	    ok;
	{error, {demands, Requirements}, _} -> 
	    error_logger:info_msg("~p: Item demands requirements: ~p~n", [?MODULE, Requirements]),
	    %% FIXME : hardwired strength as requirement
	    emud_item:do(ItemPid, pickup, [{strength, 50}]);
	{error, no_such_item} ->
	    {error, no_such_item}   
    end.


enter_room(_, undefined, State) ->
    {reply, {error, no_room_in_that_direction}, State};
enter_room(Player, Room, State) ->
    Reply = emud_room:enter(Room, Player),
    case Reply of 
	ok ->
	    NewState = State#state{room = Room},
	    %% leave the old room
	    leave_old_room(State#state.room, Player),
	    {reply, Reply, NewState}
%% ;
%% 	{error, could_not_enter_room} ->
%% 	    %% we were not allowed to enter!
%% 	    {reply, Reply, State}
    end.

    
