%%%----------------------------------------------------------------------
%%% File    : elevators.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Elevator application module.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elevators).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Start the top-level supervisor with the appropriate values.
%%----------------------------------------------------------------------
-spec start(normal, []) ->
          {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(normal, []) ->
    case sim_sup:start_link(3, 6, 2) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%----------------------------------------------------------------------
%% Stop has nothing special to do.
%%----------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
