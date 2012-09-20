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
	 go/2, pickup/2, get_items/1, drop/2]).

%% DEBUG
-export([crash/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {room=no_room, items=[], output_server=none}).

%%%===================================================================
%%% API
%%%===================================================================

enter(Player, Room) ->
    gen_server:call(Player, {enter_room, Player, Room}).

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

send_msg(Player, String) ->
    gen_server:cast(Player, {send_message, String}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Name) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(_Name) ->
    gen_server:start_link(?MODULE, [], []).

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
    {ok, #state{}}.

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
handle_call({pickup, Player, Item}, _From, State) ->
    pickup_item(Player, Item, State);
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
    {ok, Players} = emud_room:get_players(State#state.room),
    Reply = {ok, RoomDescriptions, Directions, Players, Items},
    {reply, Reply, State};
handle_call({get_items}, _From, State) ->
    Reply = {ok, State#state.items},
    {reply, Reply, State};
handle_call({register_output_server, Server}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{output_server = Server}};
handle_call({drop, Item}, _From, State) ->
    NewState = State#state{items = lists:delete(Item, State#state.items)},
    emud_room:add_item(State#state.room, Item),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({crash}, _From, _State) ->
    0/0.



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
handle_cast({send_message, String}, State) ->
    case State#state.output_server of
	none -> ok;
	_ ->  emud_console_output:write_string(State#state.output_server, String)
    end,
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

leave_old_room(no_room, _Player) ->
    ok;
leave_old_room(Room, Player) ->
    ok = emud_room:leave(Room, Player).




pickup_item(_Player, ItemInteractionName, State) ->
    case emud_room:lookup_item_by_interaction_name(State#state.room, ItemInteractionName) of
	{ok, [Item | _]} -> 
	    Reply = emud_room:remove_item(State#state.room, Item),
	    case Reply of
		ok ->
		    NewState = State#state{items = [Item | State#state.items]},
		    {reply, Reply, NewState};
		_ -> {reply, {error, could_not_pickup_item}, State}
	    end;
	_ -> {reply, {error, could_not_pickup_item}, State}
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
	    {reply, Reply, NewState};
	{error, could_not_enter_room} ->
	    %% we were not allowed to enter!
	    {reply, Reply, State}
    end.

    
