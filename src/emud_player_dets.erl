%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_player_dets).

-behaviour(gen_server).

%% API
-export([start_link/0, get_player/1, put_player/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

get_player(Key) ->
    gen_server:call(?SERVER, {get_player, Key}).

put_player(Key, Player) ->
    gen_server:call(?SERVER, {put_player, Key, Player}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, _Name} = dets:open_file(dets_player_states, []),
    {ok, #state{}}.

handle_call({get_player, Key}, _From, State) ->
    Reply = dets:lookup(dets_player_states, Key),
    {reply, Reply, State};
handle_call({put_player, Key, Player}, _From, State) -> 
    Reply = dets:insert(dets_player_states, {Key, Player}),
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
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
