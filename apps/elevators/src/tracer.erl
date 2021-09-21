%%%----------------------------------------------------------------------
%%% File    : tracer.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Text-based event tracer that reports system events.
%%% Created :  4 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(tracer).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(gen_event).

-include_lib("kernel/include/logger.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([])
%%  Initializes the event handler.
%%----------------------------------------------------------------------
-spec init([]) -> {ok, []}.
init([]) ->
    {ok, []}.

%%----------------------------------------------------------------------
%% handle_event(Event, [])
%%  Prints info on the event that has occured.
%%----------------------------------------------------------------------
-spec handle_event(atom(), []) -> {ok, []}.
handle_event(Event, []) ->
    ?LOG_NOTICE(#{event => Event}),
    {ok, []}.

%%----------------------------------------------------------------------
%% handle_call not used, but we will print info anyway.
%%----------------------------------------------------------------------
-spec handle_call(term(), term()) -> {ok, ok, term()}.
handle_call(Request, State) ->
    ?LOG_NOTICE(#{request => Request, state => State}),
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% handle_info not used, but we will print info anyway.
%%----------------------------------------------------------------------
-spec handle_info(term(), term()) -> {ok, term()}.
handle_info(Info, State) ->
    ?LOG_NOTICE(#{info => Info, state => State}),
    {ok, State}.

%%----------------------------------------------------------------------
%% terminate has nothing to clean up.
%%----------------------------------------------------------------------
-spec terminate(term(), term()) -> ok.
terminate(Reason, State) ->
    ?LOG_NOTICE(#{reason => Reason, state => State}),
    ok.

%%----------------------------------------------------------------------
%% code_change has no state to convert.
%%----------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
