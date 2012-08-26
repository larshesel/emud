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
-export([start_link/0]).

-export([get_description/1, get_directions/1, create_empty_room/0, 
	 set_description/2, link_rooms/3, get_players/1,
	 enter/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%-define(SERVER, ?MODULE). 

-record(state, {room_name, directions=[], description, items=[], players=[]}).


%%%===================================================================
%%% API
%%%===================================================================

create_empty_room() ->
    start_link().

link_rooms(FromPid, ToPid, Direction) ->
    gen_server:call(FromPid, {link_room, ToPid, Direction}).

get_description(Pid) ->
    gen_server:call(Pid, {get_description}).

get_directions(Pid) ->
    gen_server:call(Pid, {get_directions}).

set_description(Pid, Description) ->
    gen_server:call(Pid, {set_description, Description}).

enter(RoomPid, Player) ->
    gen_server:call(RoomPid, {enter_room, Player}).

get_players(RoomPid) ->
    gen_server:call(RoomPid, {get_players}).

get_items(RoomPid) ->
    gen_server:call(RoomPid, {get_items}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
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
handle_call({get_directions}, _From, State) ->
    Reply = {ok, {get_directions, State#state.directions}},
    {reply, Reply, State};
handle_call({get_description}, _From, State) ->
    Reply = {ok, {get_description, State#state.description}},
    {reply, Reply, State};
handle_call({link_room, ToPid, Direction}, _From, State) ->
    Reply = {ok, link_room},
    NewState = add_room(State, ToPid, Direction),
    {reply, Reply, NewState};
handle_call({set_description, Description}, _From, State) ->
    Reply = {ok, set_description},
    NewState = State#state{description = Description},
    {reply, Reply, NewState};
handle_call({enter_room, Player}, _From, State) ->
    Reply = {ok, enter_room},
    NewState = State#state{players = [Player, State#state.players]},
    {reply, Reply, NewState};
handle_call({get_players}, _From, State) ->
    Reply = {ok, State#state.players},
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
handle_cast(_Msg, State) ->
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
    OldState#state{directions=[{ToPid, Direction}| OldState#state.directions]}.


%% get_room(RoomName) ->
%%     #state{room_name = RoomName, 
%%      directions=[{w, ], 
%%      description="You are in a small dark room. There are a lot of chairs facing a podium, like in an auditorium or a court room. On the desk on the podium there are a lot of papers lying around.",
%%      items=[
%% 	    {"papers", invisible}, 
%% 	    {"broken_chair", visible, "a broken chair"}
%% 	   ]
%%     }.
