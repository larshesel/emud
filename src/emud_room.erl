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
	 enter/2, leave/2, add_item/2, remove_item/2, lookup_item/2,
	 msg_room/3, lookup_item_by_interaction_name/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("emud_room.hrl").

%%%===================================================================
%%% API
%%%===================================================================

link_rooms(FromRoom, ToRoom, Direction) ->
    gen_server:call(FromRoom, {link_room, ToRoom, Direction}).

get_description(Room) ->
    gen_server:call(Room, {get_description}).

get_directions(Room) ->
    gen_server:call(Room, {get_directions}).

enter(Room, Player) ->
    gen_server:call(Room, {enter_room, Player}).

get_players(Room) ->
    gen_server:call(Room, {get_players}).

get_items(Room) ->
    gen_server:call(Room, {get_items}).

leave(Room, Player) ->
    gen_server:call(Room, {leave_room, Player}).

add_item(Room, Item) ->
    gen_server:call(Room, {add_item, Item}).

remove_item(Room, Item) ->
    gen_server:call(Room, {remove_item, Item}).

lookup_item(Room, ItemName) ->
    gen_server:call(Room, {lookup_item, ItemName}).

lookup_item_by_interaction_name(Room, IN) ->
    gen_server:call(Room, {lookup_item_by_in, IN}).

%% CASTS
msg_room(Room, FromPlayer, String) ->
    gen_server:cast(Room, {message_room, FromPlayer, String}).


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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #room_state{}};
init([State]) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({get_directions}, _From, State) ->
    Reply = {ok, State#room_state.directions},
    {reply, Reply, State};
handle_call({get_description}, _From, State) ->
    Reply = {ok, State#room_state.description},
    {reply, Reply, State};
handle_call({link_room, ToPid, Direction}, _From, State) ->
    Reply = ok,
    NewState = add_room(State, ToPid, Direction),
    {reply, Reply, NewState};
handle_call({enter_room, Player}, _From, State) ->
    Reply = ok,
    handle_cast({message_room, Player, io_lib:format("~p entered the room.~n", [Player])}, State),
    %% or {error, could_not_enter_room, display_message}.
    NewState = State#room_state{players = [Player| State#room_state.players]},
    {reply, Reply, NewState};
handle_call({leave_room, Player}, _From, State) ->
    Reply = ok,
    NewState = State#room_state{players = lists:delete(Player, State#room_state.players)},
    handle_cast({message_room, Player, io_lib:format("~p left the room.~n", [Player])}, State),
    {reply, Reply, NewState};
handle_call({get_players}, _From, State) ->
    Reply = {ok, State#room_state.players},
    {reply, Reply, State};
handle_call({get_items}, _From, State) ->
    Reply = {ok, State#room_state.items},
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
    {reply, Reply, State}.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({message_room, FromPlayer, String}, State) ->
    [emud_player:send_msg(X, String) || X<-State#room_state.players, X /= FromPlayer],
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

    
add_room(OldState, ToPid, Direction) ->
    OldState#room_state{directions=[{Direction, ToPid}| OldState#room_state.directions]}.
