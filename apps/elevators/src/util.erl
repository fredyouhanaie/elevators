%%% File    : util.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Start functions for the elevator system.
%%% Created :  1 Sep 1999 by Håkan Huss <hakan@erlang.ericsson.se>

-module(util).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-export([start/3, start_trace/3, stop/0, start_sup/3,
         top_proc/4, top_proc_sup/3]).

%%----------------------------------------------------------------------
%% start(IFloor, NFloors, NElevs)
%%  Starts the system without supervision (except for the graphics
%%  portion).
%%----------------------------------------------------------------------
-spec start(pos_integer(), pos_integer(), pos_integer()) -> pid().
start(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc, [IFloor, NFloors, NElevs, []]).

%%----------------------------------------------------------------------
%% start_trace(IFloor, NFloors, NElevs)
%%  Starts the system without supervision (except for the graphics
%%  portion). Installs the tracer event handler.
%%----------------------------------------------------------------------
-spec start_trace(pos_integer(), pos_integer(), pos_integer()) -> pid().
start_trace(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc, [IFloor, NFloors, NElevs, [{tracer, []}]]).

%%----------------------------------------------------------------------
%% start_sup(IFloor, NFloors, NElevs)
%%  Starts the system with supervision.
%%----------------------------------------------------------------------
-spec start_sup(pos_integer(), pos_integer(), pos_integer()) -> pid().
start_sup(IFloor, NFloors, NElevs) ->
    spawn(util, top_proc_sup, [IFloor, NFloors, NElevs]).

%%----------------------------------------------------------------------
%% stop()
%%  Attempts to stop the system
%%----------------------------------------------------------------------
-spec stop() -> stopped.
stop() ->
    case whereis (top_proc) of
        undefined ->
            ok;
        _ ->
            top_proc ! stop
    end,
    case whereis (top_proc_sup) of
        undefined ->
            ok;
        _ ->
            top_proc_sup ! stop
    end,
    stopped.

%%----------------------------------------------------------------------
%% top_proc(IFloor, NFloors, NElevs, Handlers)
%%  Starts and links to the important processes, then blocks until it
%%  gets a stop messsage.
%%----------------------------------------------------------------------
-spec top_proc(pos_integer(), pos_integer(), pos_integer(), [any()]) -> no_return().
top_proc(IFloor, NFloors, NElevs, Handlers) ->
    register(top_proc, self()),
    {ok, _SPid} = scheduler:start_link(),
    sys_event:start_link([{display, [IFloor, NFloors, NElevs]}] ++ Handlers),
    _EPids = lists:foreach(fun (ENo) ->
                                   {ok, _EPid} = elevator:start_link(ENo)
                           end,
                           lists:seq(1, NElevs)),
    block().

%%----------------------------------------------------------------------
%% top_proc(IFloor, NFloors, NElevs, Handlers)
%%  As above, but with supervision.
%%----------------------------------------------------------------------
-spec top_proc_sup(pos_integer(), pos_integer(), pos_integer()) -> no_return().
top_proc_sup(IFloor, NFloors, NElevs) ->
    register(top_proc_sup, self()),
    sim_sup:start_link(IFloor, NFloors, NElevs),
    block().

-spec block() -> no_return().
block() ->
    receive
        stop ->
            exit(shutdown)
    end.
