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

-export([get_description/1, get_short_description/1, get_interaction_names/1,
	 do/2, do/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").

-type picked_up_state() :: picked_up | not_picked_up.

-record(item_state, {item_mod,
		     picked_up_state = not_picked_up :: picked_up_state()
		    }).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

do(Item, Action, Properties) ->
    gen_server:call(Item, {Action, Properties}).
    
do(Item, Action) ->
    do(Item, Action, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Mod]) ->
    {ok, #item_state{item_mod = Mod}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({get_description}, _From, State) ->
    Mod = State#item_state.item_mod,
    Reply = {ok, Mod:description()},
    {reply, Reply, State};
handle_call({get_short_description}, _From, State) ->
    Mod = State#item_state.item_mod,
    Reply = {ok, Mod:short_description()},
    {reply, Reply, State};
handle_call({get_interaction_names}, _From, State) ->
    Mod = State#item_state.item_mod,
    {reply, {ok, Mod:interaction_names()}, State};
handle_call({pickup, PlayerProperties}, _From, State) ->
    handle_pickup(State, PlayerProperties).

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

get_pickup_reqs(State) ->
    Mod = State#item_state.item_mod,
    Mod:pickup_requirements().

handle_pickup(State, PlayerProperties) ->
    error_logger:info_msg("~p: pickup: Requirements: ~p Capabilities: ~p~n", [?MODULE, get_pickup_reqs(State), PlayerProperties]),
    case State#item_state.picked_up_state of 
	picked_up ->
	    {reply, {error, no_such_item}, State};
	not_picked_up ->
	    %% FIXME: insert guard that checks says no, if the item 
	    %% is already picked up: picked_up_state == picked_up
	    case check_properties(get_pickup_reqs(State), PlayerProperties, []) of
		[] ->
		    NewState = State#item_state{picked_up_state = picked_up},
		    {reply, ok, NewState};
		FailedProperties ->
		    %% FIXME: Hardwired failure message
		    {reply, {error, 
			     {demands, FailedProperties}, 
			     {display_message, "You try to lift the stone, but it is too heavy.\nYour back hurts.\n"}}, State}
	    end
    end.

%% Note: N^2 algorithm. But lists are short?
check_properties([], _, Acc) -> lists:reverse(Acc);
check_properties([{RKey, RVal} | RTail], PlayerProperties, Acc) -> 
    case check_property(RKey, proplists:get_value(RKey, PlayerProperties, false), RVal) of 
	false -> check_properties(RTail, PlayerProperties, [RKey | Acc]);
	true -> check_properties(RTail, PlayerProperties, Acc)
    end.

%% actual property tests
check_property(_, false, _) ->
    false;
check_property(strength, Actual, Required) ->
    Actual >= Required;
check_property(charisma, Actual, Required) ->
    Actual >= Required.
    
-ifdef(TEST).
check_properties_test_() ->
    [?_assertEqual([], check_properties([], [], [])),
     ?_assertEqual([], check_properties([], [{some_property, 50}], [])),
     ?_assertEqual([some_property], check_properties([{some_property, 50}], [], [])),
     ?_assertEqual([strength, charisma], 
      		   check_properties([{strength, 40}, {charisma, 40}], 
      				    [{strength, 30}, {power, 10}, {charisma, 30}],
      				    []))
    ].
-endif.
