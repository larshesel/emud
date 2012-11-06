%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_room).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([get_description/1, get_directions/1, 
	 link_rooms/3, get_players/1, get_items/1,
	 enter/2, leave/2, add_item/2, remove_item/2,
	 lookup_item_by_in/2,
	 get_ais/1, lookup_player_by_in/2, player_quit/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("emud.hrl").
-record(room_state, {directions=[], room_mod, items=[], players=[], ais=[]}).

%%%===================================================================
%%% API
%%%===================================================================

player_quit(Room, Player) ->
    gen_server:call(Room, {player_quit, Player}).

link_rooms(FromRoom, ToRoom, Direction) ->
    gen_server:call(FromRoom, {link_room, ToRoom, Direction}).

-spec get_description(room()) -> {ok, string()}.
get_description(Room) ->
    gen_server:call(Room, {get_description}).

-spec get_directions(room()) -> {ok, list(any())}.
get_directions(Room) ->
    gen_server:call(Room, {get_directions}).

-spec enter(room(), player()) -> ok.
enter(Room, Player) ->
    gen_server:call(Room, {enter_room, Player}).

-spec get_players(room()) -> {ok, list(player())}.
get_players(Room) ->
    gen_server:call(Room, {get_players}).

-spec get_items(room()) -> {ok, list(item())}.
get_items(Room) ->
    gen_server:call(Room, {get_items}).

get_ais(Room) ->
    gen_server:call(Room, {get_ais}).

-spec leave(room(), player()) -> ok.
leave(Room, Player) ->
    gen_server:call(Room, {leave_room, Player}).

-spec add_item(room(), item()) -> ok.
add_item(Room, Item) ->
    gen_server:call(Room, {add_item, Item}).

remove_item(Room, Item) ->
    gen_server:call(Room, {remove_item, Item}).

-spec lookup_item_by_in(room(), in()) -> {ok, list(item())}.
lookup_item_by_in(Room, IN) ->
    gen_server:call(Room, {lookup_item_by_in, IN}).

-spec lookup_player_by_in(room(), in()) -> {ok, list(player())}.
lookup_player_by_in(Room, IN) ->
    gen_server:call(Room, {lookup_player_by_in, IN}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(State) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(State) ->
    gen_server:start_link(?MODULE, [State], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Mod]) ->
    {ok, #room_state{room_mod = Mod}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({player_quit, Player}, _From, State) ->
    NewState = State#room_state{players = lists:delete(Player, State#room_state.players)},
    handle_cast({message_room, {player_quit, Player}}, State),
    {reply, ok, NewState};
handle_call({get_directions}, _From, State) ->
    Reply = {ok, State#room_state.directions},
    {reply, Reply, State};
handle_call({get_description}, _From, State) ->
    Mod = State#room_state.room_mod,
    Reply = {ok, Mod:description()},
    {reply, Reply, State};
handle_call({link_room, ToPid, Direction}, _From, State) ->
    Reply = ok,
    NewState = add_room(State, ToPid, Direction),
    {reply, Reply, NewState};
handle_call({enter_room, Player}, _From, State) ->
    Reply = ok,
    handle_cast({message_room, {player_entered_room, Player}}, State),
    %% or {error, could_not_enter_room, display_message}.
    NewState = State#room_state{players = [Player| State#room_state.players]},
    {reply, Reply, NewState};
handle_call({leave_room, Player}, _From, State) ->
    Reply = ok,
    NewState = State#room_state{players = lists:delete(Player, State#room_state.players)},
    handle_cast({message_room, {player_left_room, Player}}, State),
    {reply, Reply, NewState};
handle_call({get_players}, _From, State) ->
    Reply = {ok, State#room_state.players},
    {reply, Reply, State};
handle_call({get_items}, _From, State) ->
    Reply = {ok, State#room_state.items},
    {reply, Reply, State};
handle_call({get_ais}, _From, State) ->
    Reply = {ok, State#room_state.ais},
    {reply, Reply, State};
handle_call({add_item, Item}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#room_state{items = [Item | State#room_state.items]}};
handle_call({remove_item, Item}, _From, State) ->
    Reply = ok,
    NewState = State#room_state{items = lists:delete(Item, State#room_state.items)},
    {reply, Reply, NewState};
handle_call({lookup_item_by_in, IN}, _From, State) ->
    Matches = lists:filter(fun(Pid) -> 
		 		   {ok, Names} = emud_item:get_interaction_names(Pid),
				   length(lists:filter(fun(Name) -> Name == IN end, Names)) > 0
		 	   end,
		 State#room_state.items),
    Reply = {ok, Matches},
    {reply, Reply, State};
handle_call({lookup_player_by_in, IN}, _From, State) ->
    Matches = lists:filter(fun(Pid) -> 
		 		   {ok, Names} = emud_player:get_name(Pid),
				   length(lists:filter(fun(Name) -> Name == IN end, Names)) > 0
		 	   end,
		 State#room_state.items),
    Reply = {ok, Matches},
    {reply, Reply, State}.
    

handle_cast({message_room, {Type, From}}, State) ->
    [emud_player:send_msg(X, {Type, From}) || X<-State#room_state.players, X /= From],
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

    
add_room(OldState, ToPid, Direction) ->
    OldState#room_state{directions=[{Direction, ToPid}| OldState#room_state.directions]}.
