%%%----------------------------------------------------------------------
%%% File    : elevator.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Elevator interface functions for elevator exercise
%%% Created :  3 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elevator).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-ifdef(USE_GEN_FSM).
-behaviour(gen_fsm).
-else.
-behaviour(gen_statem).
-endif.

%% External exports
-export([start_link/1]).
-export([reset/3, move/2, stop/1, open/1, close/1,
         approaching/2, at_floor/2, get_state/1]).

-ifdef(USE_GEN_FSM).
%% gen_fsm callbacks
-export([init/1, handle_sync_event/4, handle_event/3,
         handle_info/3, terminate/3, code_change/4]).
-export([uninitialized/2, open/2, closed/2, moving/2, stopping/2]).
-else.
%% gen_statem callbacks
-export([init/1, callback_mode/0,
         terminate/3, code_change/4]).
-export([uninitialized/3, open/3, closed/3, moving/3, stopping/3]).
-endif.

%%%----------------------------------------------------------------------
%%%
%%% The elevator is represented by an FSM with four states:
%%%  open:     Standing at a floor with the doors open
%%%  closed:   Standing at a floor with the doors closed
%%%  moving:   Moving
%%%  stopping: Moving, but will stop at the next floor
%%%
%%% In addition to the state, the FSM has information about its number and
%%% a floor. The floor is the current floor if the elevator is standing
%%% still, otherwise the last floor it passed.
%%%
%%% When the FSM starts, it's in the state uninitialized, where it only reacts
%%% to the event {reset, Floor}, which causes it to enter the state closed.
%%%
%%% Apart from this, the states, events and corresponding actions are:
%%%
%%%
%%%      State   |     open     |    closed    |    moving     |   stopping
%%% Event        |              |              |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% open         |      N/A     | Open doors   |      N/A      |      N/A
%%%              |              | -> open      |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% close        | Close doors  |      N/A     |      N/A      |      N/A
%%%              | Inform sched |              |               |
%%%              | -> closed    |              |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% {move, Dir}  |      N/A     | Start moving |      N/A      |      N/A
%%%              |              | -> moving    |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% {approaching,|      N/A     |      N/A     | Inform sched  |      N/A
%%%  Floor}      |              |              | -> moving     |
%%%              |              |              | -> stopping   |
%%% -------------+--------------+--------------+---------------+--------------
%%% {at, Floor}  |      N/A     |      N/A     | Update floor, | Update floor,
%%%              |              |              | inform sched  | inform sched
%%%              |              |              | -> moving     | -> open
%%%
%%% (sched = scheduler)
%%%
%%% There is also a synchronous event to retrieve the state of an
%%% elevator, get_state, which is handled in all states. It returns
%%% the tuple {State, Floor}.
%%%
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
start_link(ENo) ->
    gen_fsm:start_link(elevator, [ENo], []).
-else.
-spec start_link(pos_integer()) -> ignore | {error, term()} | {ok, pid()}.
start_link(ENo) ->
    gen_statem:start_link(elevator, [ENo], []).
-endif.

-ifdef(USE_GEN_FSM).
reset(Elev, State, Floor) ->
    gen_fsm:send_event(Elev, {reset, State, Floor}).
-else.
-spec reset(gen_statem:server_ref(), term(), pos_integer()) -> ok.
reset(Elev, State, Floor) ->
    gen_statem:cast(Elev, {reset, State, Floor}).
-endif.

-ifdef(USE_GEN_FSM).
move(Elev, Dir) ->
    gen_fsm:send_event(Elev, {move, Dir}).
-else.
-spec move(gen_statem:server_ref(), up | down) -> ok.
move(Elev, Dir) ->
    gen_statem:cast(Elev, {move, Dir}).
-endif.

-ifdef(USE_GEN_FSM).
stop(Elev) ->
    gen_fsm:send_event(Elev, stop).
-else.
-spec stop(gen_statem:server_ref()) -> ok.
stop(Elev) ->
    gen_statem:cast(Elev, stop).
-endif.

-ifdef(USE_GEN_FSM).
open(Elev) ->
    gen_fsm:send_event(Elev, open).
-else.
-spec open(gen_statem:server_ref()) -> ok.
open(Elev) ->
    gen_statem:cast(Elev, open).
-endif.

-ifdef(USE_GEN_FSM).
close(Elev) ->
    gen_fsm:send_event(Elev, close).
-else.
-spec close(gen_statem:server_ref()) -> ok.
close(Elev) ->
    gen_statem:cast(Elev, close).
-endif.

-ifdef(USE_GEN_FSM).
approaching(Elev, Floor) ->
    gen_fsm:send_event(Elev, {approaching, Floor}).
-else.
-spec approaching(gen_statem:server_ref(), pos_integer()) -> ok.
approaching(Elev, Floor) ->
    gen_statem:cast(Elev, {approaching, Floor}).
-endif.

-ifdef(USE_GEN_FSM).
at_floor(Elev, Floor) ->
    gen_fsm:send_event(Elev, {at_floor, Floor}).
-else.
-spec at_floor(gen_statem:server_ref(), pos_integer()) -> ok.
at_floor(Elev, Floor) ->
    gen_statem:cast(Elev, {at_floor, Floor}).
-endif.

-ifdef(USE_GEN_FSM).
get_state(Elev) ->
    gen_fsm:sync_send_all_state_event(Elev, get_state).
-else.
-spec get_state(gen_statem:server_ref()) -> any().
get_state(Elev) ->
    gen_statem:call(Elev, get_state).
-endif.


%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm / gen_statem
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([ENo])
%%  ENo = int()
%%
%% Initializes an elevator FSM for elevator number ENo.
%%----------------------------------------------------------------------
-spec init([any(), ...]) -> {ok, uninitialized, {pos_integer(), unknown_floor}}.
init([ENo]) ->
    sys_event:controller_started(ENo, self()),
    {ok, uninitialized, {ENo, unknown_floor}}.

-ifdef(USE_GEN_FSM).
-else.
-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.
-endif.

%%----------------------------------------------------------------------
%% State callbacks
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
uninitialized({reset, State, Floor}, {ENo, unknown_floor}) ->
    scheduler:set_controller(ENo, self()),
    sys_event:initialized(ENo, State, Floor),
    {next_state, State, {ENo, Floor}};
uninitialized(_Other, Data) ->
    {next_state, uninitialized, Data}.
-else.
-spec uninitialized(cast | {call, gen_statem:server_ref()}, atom(), term()) ->
          {keep_state,
           {pos_integer(), pos_integer()},
           {reply, gen_statem:server_ref(), {pos_integer(), uninitialized, pos_integer()}}
          } |
          {next_state, {pos_integer(), pos_integer()}}.
uninitialized({call, From}, get_state, {Eno, Floor}) ->
    {keep_state, {Eno, Floor}, {reply, From, {Eno, uninitialized, Floor}}};
uninitialized(cast, {reset, State, Floor}, {ENo, unknown_floor}) ->
    scheduler:set_controller(ENo, self()),
    sys_event:initialized(ENo, State, Floor),
    {next_state, State, {ENo, Floor}};
uninitialized(cast, _Other, Data) ->
    {next_state, uninitialized, Data}.
-endif.

-ifdef(USE_GEN_FSM).
open(close, {ENo, Floor}) ->
    sys_event:close(ENo),
    scheduler:closed(ENo, Floor),
    {next_state, closed, {ENo, Floor}}.
-else.
-spec open(cast | {call, gen_statem:server_ref()},
           atom(), {pos_integer(), pos_integer()}) ->
          {keep_state, {pos_integer(), pos_integer()}} |
          {keep_state, {pos_integer(), pos_integer()},
           {reply, gen_statem:server_ref(), {pos_integer(), open, pos_integer()}}} |
          {next_state, closed, {pos_integer(), pos_integer()}}.
open({call, From}, get_state, {Eno, Floor}) ->
    {keep_state, {Eno, Floor}, {reply, From, {Eno, open, Floor}}};
open(cast, close, {ENo, Floor}) ->
    sys_event:close(ENo),
    scheduler:closed(ENo, Floor),
    {next_state, closed, {ENo, Floor}};
open(cast, _Other, Data) ->
    {keep_state, Data}.
-endif.

-ifdef(USE_GEN_FSM).
closed(open, {ENo, Floor}) ->
    sys_event:open(ENo),
    timer:apply_after(1000, elevator, close, [self()]),
    {next_state, open, {ENo, Floor}};
closed({move, Dir}, {ENo, Floor}) ->
    sys_event:move(ENo, Dir),
    {next_state, moving, {ENo, Floor}}.
-else.
-spec closed(cast | {call, gen_statem:server_ref()}, atom(), {pos_integer(), pos_integer()}) ->
          {keep_state, {pos_integer(), pos_integer()}} |
          {keep_state, {pos_integer(), pos_integer()},
           {reply, gen_statem:server_ref(), {pos_integer(), closed ,pos_integer()}}} |
          {next_state, moving | open,{pos_integer(), pos_integer()}}.
closed({call, From}, get_state, {Eno, Floor}) ->
    {keep_state, {Eno, Floor}, {reply, From, {Eno, closed, Floor}}};
closed(cast, open, {ENo, Floor}) ->
    sys_event:open(ENo),
    timer:apply_after(1000, elevator, close, [self()]),
    {next_state, open, {ENo, Floor}};
closed(cast, {move, Dir}, {ENo, Floor}) ->
    sys_event:move(ENo, Dir),
    {next_state, moving, {ENo, Floor}};
closed(cast, _Other, Data) ->
    {keep_state, Data}.
-endif.

-ifdef(USE_GEN_FSM).
moving({approaching, NewFloor}, {ENo, Floor}) ->
    sys_event:approaching(ENo, NewFloor),
    case scheduler:approaching(ENo, NewFloor) of
        {ok, stop} ->
            sys_event:stopping(ENo),
            {next_state, stopping, {ENo, Floor}};
        {ok, continue} ->
            {next_state, moving, {ENo, Floor}};
        _Other ->
            sys_event:stopping(ENo),
            {next_state, stopping, {ENo, Floor}}
    end;
moving({at_floor, NewFloor}, {ENo, _Floor}) ->
    scheduler:passing(ENo, NewFloor),
    {next_state, moving, {ENo, NewFloor}}.
-else.
-spec moving(cast | {call, gen_statem:server_ref()}, atom(), {pos_integer(), pos_integer()}) ->
          {keep_state, {pos_integer(), pos_integer()}} |
          {keep_state,{pos_integer(), pos_integer()},
           {reply, gen_statem:server_ref(), {pos_integer(), moving, pos_integer()}}} |
          {next_state, moving | stopping, {pos_integer(), pos_integer()}}.
moving({call, From}, get_state, {Eno, Floor}) ->
    {keep_state, {Eno, Floor}, {reply, From, {Eno, moving, Floor}}};
moving(cast, {approaching, NewFloor}, {ENo, Floor}) ->
    sys_event:approaching(ENo, NewFloor),
    case scheduler:approaching(ENo, NewFloor) of
        {ok, stop} ->
            sys_event:stopping(ENo),
            {next_state, stopping, {ENo, Floor}};
        {ok, continue} ->
            {next_state, moving, {ENo, Floor}};
        _Other ->
            sys_event:stopping(ENo),
            {next_state, stopping, {ENo, Floor}}
    end;
moving(cast, {at_floor, NewFloor}, {ENo, _Floor}) ->
    scheduler:passing(ENo, NewFloor),
    {next_state, moving, {ENo, NewFloor}};
moving(cast, _Other, Data) ->
    {keep_state, Data}.
-endif.

-ifdef(USE_GEN_FSM).
stopping({at_floor, NewFloor}, {ENo, _Floor}) ->
    sys_event:stopped_at(ENo, NewFloor),
    sys_event:open(ENo),
    scheduler:open(ENo, NewFloor),
    timer:apply_after(1000, elevator, close, [self()]),
    {next_state, open, {ENo, NewFloor}}.
-else.
-spec stopping(cast | {call, gen_statem:server_ref()}, atom(), {pos_integer(), pos_integer()}) ->
          {keep_state, {pos_integer(), pos_integer()}} |
          {keep_state, {pos_integer(), pos_integer()},
           {reply, gen_statem:server_ref(), {pos_integer(), stopping, pos_integer()}}} |
          {next_state, open, {pos_integer(), pos_integer()}}.
stopping({call, From}, get_state, {Eno, Floor}) ->
    {keep_state, {Eno, Floor}, {reply, From, {Eno, stopping, Floor}}};
stopping(cast, {at_floor, NewFloor}, {ENo, _Floor}) ->
    sys_event:stopped_at(ENo, NewFloor),
    sys_event:open(ENo),
    scheduler:open(ENo, NewFloor),
    timer:apply_after(1000, elevator, close, [self()]),
    {next_state, open, {ENo, NewFloor}};
stopping(cast, _Other, Data) ->
    {keep_state, Data}.
-endif.

%%----------------------------------------------------------------------
%% The get_state event.
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
handle_sync_event(get_state, _From, State, {ENo, Floor}) ->
    {reply, {ENo, State, Floor}, State, {ENo, Floor}}.
-else.
-endif.

%%----------------------------------------------------------------------
%% No all_state_events should be sent.
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
handle_event(Event, State, {ENo, Floor}) ->
    {stop, {unknown_event, Event, {ENo, State, Floor}}}.
-else.
-endif.

%%----------------------------------------------------------------------
%% No messages should be sent.
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
handle_info(Msg, State, {ENo, Floor}) ->
    {stop, {unknown_message, Msg, {ENo, State, Floor}}}.
-else.
-endif.

%%----------------------------------------------------------------------
%% Code change is a no-op (no previous version exists).
%%----------------------------------------------------------------------
-spec code_change(term(), term(), term(), term()) -> {ok, term(), term()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%----------------------------------------------------------------------
%% Cleanup.
%%----------------------------------------------------------------------
-spec terminate(term(), term(), {pos_integer(), pos_integer()}) -> ok.
terminate(_Reason, _State, {_ENo, _Floor}) ->
    ok.
