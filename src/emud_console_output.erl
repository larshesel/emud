%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 31 Aug 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(emud_console_output).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([write_string/2, write_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

write_msg(Server, Message) ->
    gen_server:cast(Server, {write_message, Message}).

write_string(Server, String) ->
    gen_server:cast(Server, {write_string, String}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({write_string, String}, State) ->
    io:fwrite("~s", [String]),
    {noreply, State};
handle_cast({write_message, Message}, State) ->
    handle_message(Message),
    {noreply, State}.

handle_message({player_left_room, Pid})->
    io:fwrite("~s left the room.~n", [get_text(emud_player:get_name(Pid))]);
handle_message({player_entered_room, Pid})->
    io:fwrite("~s entered the room.~n", [get_text(emud_player:get_name(Pid))]);
handle_message({player_quit, Pid}) -> 
    io:fwrite("someone is a quitter.~n", []).

get_text({ok, Text}) ->
    Text.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
