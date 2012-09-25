%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_item).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([get_description/1, get_short_description/1, get_interaction_names/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("emud_item.hrl").

%%%===================================================================
%%% API
%%%===================================================================

get_description(Item) ->
    gen_server:call(Item, {get_description}).

get_short_description(Item) ->
    gen_server:call(Item, {get_short_description}).

get_interaction_names(Item) ->
    gen_server:call(Item, {get_interaction_names}).

start_link(State) ->
    gen_server:start_link(?MODULE, [State], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #item_state{}};
init([State]) ->
    {ok, State}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({get_description}, _From, State) ->
    Reply = {ok, State#item_state.description},
    {reply, Reply, State};
handle_call({get_short_description}, _From, State) ->
    Reply = {ok, State#item_state.short_description},
    {reply, Reply, State};
handle_call({get_interaction_names}, _From, State) ->
    {reply, {ok, State#item_state.interaction_names}, State}.


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
