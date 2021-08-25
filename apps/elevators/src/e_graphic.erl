%%%----------------------------------------------------------------------
%%% File    : e_graphic.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Control process for the graphics of an elevator.
%%% Created :  3 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(e_graphic).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-ifdef(USE_GEN_FSM).
-behaviour(gen_fsm).
-else.
-behaviour(gen_statem).
-endif.

-ifdef(GS_MOCK).
-define(GS, ?GS_MOCK).
-else.
-define(GS, gs).
-endif.

%% External exports
-export([start_link/3]).
-export([open/1, close/1, stop/1, move/2, set_controller/2]).
-export([get_floor/3]).

%% gen_fsm callbacks
-ifdef(USE_GEN_FSM).
-export([init/1, open/2, closed/2, moving/2, stopping/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-else.
-export([callback_mode/0]).
-export([init/1, open/3, closed/3, moving/3, stopping/3]).
-export([terminate/3, code_change/4]).
-endif.

%%%----------------------------------------------------------------------
%%% 
%%% The graphical elevator is represented by an FSM with four states:
%%%  open:     Standing at a floor with the doors open
%%%  closed:   Standing at a floor with the doors closed
%%%  moving:   Moving
%%%
%%% In addition to the state, the FSM has information about its position,
%%% and the floor pixel coordinates in order to be able to detrmine when
%%% a floor is being approached/passed.
%%%
%%% The states, events and corresponding actions are:
%%%
%%%
%%%      State |     open     |    closed    |    moving     |   stopping    
%%% Event      |              |              |               |               
%%% -----------+--------------+--------------+---------------+-------------- 
%%% open       |      N/A     | Open doors   |      N/A      |      N/A      
%%%            |              | -> open      |               |               
%%% -----------+--------------+--------------+---------------+-------------- 
%%% close      | Close doors  |      N/A     |      N/A      |      N/A      
%%%            | -> closed    |              |               |               
%%% -----------+--------------+--------------+---------------+-------------- 
%%% stop       |      N/A     |      N/A     | Start stopping|      N/A      
%%%            |              |              | -> stopping   |               
%%% -----------+--------------+--------------+---------------+-------------- 
%%% {move, Dir}|      N/A     | Start moving |      N/A      |      N/A      
%%%            |              | -> moving    |               |               
%%% -----------+--------------+--------------+---------------+--------------
%%% {step, Dir}|      N/A     |      N/A     | take step     | take step
%%%            |              |              | check position| -> stopping
%%%            |              |              | -> moving     | -> closed
%%% -----------+--------------+--------------+---------------+-------------- 
%%% {epid, EP} |         Set controlling process of elevator to EP 
%%% -----------+--------------+--------------+---------------+-------------- 
%%%
%%%----------------------------------------------------------------------
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
start_link(Pos, ElevG, Floors) ->
    gen_fsm:start_link(e_graphic, [Pos, ElevG, Floors], []).
-else.
start_link(Pos, ElevG, Floors) ->
    gen_statem:start_link(e_graphic, [Pos, ElevG, Floors], []).
-endif.

-ifdef(USE_GEN_FSM).
open(Elev) ->
    gen_fsm:send_event(Elev, open).
-else.
open(Elev) ->
    gen_statem:cast(Elev, open).
-endif.

-ifdef(USE_GEN_FSM).
close(Elev) ->
    gen_fsm:send_event(Elev, close).
-else.
close(Elev) ->
    gen_statem:cast(Elev, close).
-endif.

-ifdef(USE_GEN_FSM).
stop(Elev) ->
    gen_fsm:send_event(Elev, stop).
-else.
stop(Elev) ->
    gen_statem:cast(Elev, stop).
-endif.

-ifdef(USE_GEN_FSM).
move(Elev, Dir) ->
    gen_fsm:send_event(Elev, {move, Dir}).
-else.
move(Elev, Dir) ->
    gen_statem:cast(Elev, {move, Dir}).
-endif.

-ifdef(USE_GEN_FSM).
set_controller(Elev, EPid) ->
    gen_fsm:send_all_state_event(Elev, {epid, EPid}).
-else.
set_controller(Elev, EPid) ->
    gen_statem:call(Elev, {epid, EPid}).
-endif.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([Pos, ElevG, Floors]) ->
    {ok, closed, {Pos, ElevG, nopid, nodir, Floors}}.

-ifdef(USE_GEN_FSM).
-else.
callback_mode() ->
    state_functions.
-endif.

-ifdef(USE_GEN_FSM).
open(close, {Pos, ElevG, EPid, nodir, Floors}) ->
    ?GS:config(ElevG, {fill, black}),
    {next_state, closed, {Pos, ElevG, EPid, nodir, Floors}}.
-else.
open(cast, close, {Pos, ElevG, EPid, nodir, Floors}) ->
    ?GS:config(ElevG, {fill, black}),
    {next_state, closed, {Pos, ElevG, EPid, nodir, Floors}}.
-endif.

-ifdef(USE_GEN_FSM).
closed(open, {Pos, ElevG, EPid, nodir, Floors}) ->
    ?GS:config(ElevG, {fill, cyan}),
    {next_state, open, {Pos, ElevG, EPid, nodir, Floors}};
closed({move, Dir}, {Pos, ElevG, EPid, nodir, Floors}) ->
    gen_fsm:send_event(self(), {step, Dir}),
    {next_state, moving, {Pos, ElevG, EPid, Dir, Floors}}.
-else.
closed(cast, open, {Pos, ElevG, EPid, nodir, Floors}) ->
    ?GS:config(ElevG, {fill, cyan}),
    {next_state, open, {Pos, ElevG, EPid, nodir, Floors}};
closed(cast, {move, Dir}, {Pos, ElevG, EPid, nodir, Floors}) ->
    gen_statem:cast(self(), {step, Dir}),
    {next_state, moving, {Pos, ElevG, EPid, Dir, Floors}}.
-endif.

-ifdef(USE_GEN_FSM).
moving({step, Dir}, {Pos, ElevG, EPid, Dir, Floors}) ->
    Dy = dy(Dir),
    NewPos = Pos + Dy,
    ?GS:config(ElevG, {move, {0, Dy}}),
    check_position(NewPos, Dir, EPid, Floors),
    timer:apply_after(200, gen_fsm, send_event, [self(), {step, Dir}]),
    {next_state, moving, {NewPos, ElevG, EPid, Dir, Floors}};
moving(stop, {Pos, ElevG, EPid, Dir, Floors}) ->
    {next_state, stopping, {Pos, ElevG, EPid, Dir, Floors}}.
-else.
moving(cast, {step, Dir}, {Pos, ElevG, EPid, Dir, Floors}) ->
    Dy = dy(Dir),
    NewPos = Pos + Dy,
    ?GS:config(ElevG, {move, {0, Dy}}),
    check_position(NewPos, Dir, EPid, Floors),
    timer:apply_after(200, gen_fsm, send_event, [self(), {step, Dir}]),
    {next_state, moving, {NewPos, ElevG, EPid, Dir, Floors}};
moving(cast, stop, {Pos, ElevG, EPid, Dir, Floors}) ->
    {next_state, stopping, {Pos, ElevG, EPid, Dir, Floors}}.
-endif.

-ifdef(USE_GEN_FSM).
stopping({step, Dir}, {Pos, ElevG, EPid, Dir, Floors}) ->
    case at_floor(Pos, Floors) of
	false ->
	    Dy = dy(Dir),
	    NewPos = Pos + Dy,
	    ?GS:config(ElevG, {move, {0, Dy}}),
	    timer:apply_after(200, gen_fsm, send_event, [self(), {step, Dir}]),
	    {next_state, stopping, {NewPos, ElevG, EPid, Dir, Floors}};
	{true, Floor} ->
	    elevator:at_floor(EPid, Floor),
	    {next_state, closed, {Pos, ElevG, EPid, nodir, Floors}}
    end.
-else.
stopping(cast, {step, Dir}, {Pos, ElevG, EPid, Dir, Floors}) ->
    case at_floor(Pos, Floors) of
	false ->
	    Dy = dy(Dir),
	    NewPos = Pos + Dy,
	    ?GS:config(ElevG, {move, {0, Dy}}),
	    timer:apply_after(200, gen_fsm, send_event, [self(), {step, Dir}]),
	    {next_state, stopping, {NewPos, ElevG, EPid, Dir, Floors}};
	{true, Floor} ->
	    elevator:at_floor(EPid, Floor),
	    {next_state, closed, {Pos, ElevG, EPid, nodir, Floors}}
    end.
-endif.

%%----------------------------------------------------------------------
%% Only all state event is to update the control process pid.
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
handle_event({epid, EPid}, State, {Pos, ElevG, _OldPid, Dir, Floors}) ->
    elevator:reset(EPid, State, get_floor(Pos, Dir, Floors)),
    {next_state, State, {Pos, ElevG, EPid, Dir, Floors}}.
-else.
-endif.

%%----------------------------------------------------------------------
%% No sync events defined.
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.
-else.
-endif.

%%----------------------------------------------------------------------
%% No info expected.
%%----------------------------------------------------------------------
-ifdef(USE_GEN_FSM).
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.
-else.
-endif.

%%----------------------------------------------------------------------
%% terminate has nothing to clean up.
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%%----------------------------------------------------------------------
%% Code change is a no-op (no previous version exists).
%%----------------------------------------------------------------------
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% dy(Dir) -> int()
%%  Dir = up | down
%%
%% Returns the y-offset to move the graphical elevator with when it's
%% travelling in the direction Dir.
%%----------------------------------------------------------------------
dy(up) -> -10;
dy(down) -> 10.


%%----------------------------------------------------------------------
%% check_position(Pos, Dir, EPid, Floors)
%%  Pos = int()
%%  Dir = up | down
%%  EPid = pid()
%%  Floors = [{FloorNo, YPos}, ...]
%%  FloorNo = int()
%%  YPos = int()
%%
%% Checks whether an elevator at position Pos, travelling in the direction
%% dir is approaching a floor. If so, the elevator control process EPid
%% is informed.
%%----------------------------------------------------------------------
check_position(Pos, Dir, EPid, Floors) ->
    case lists:keysearch(Pos + 2 * dy(Dir), 2, Floors) of
	{value, {Floor, _}} ->
	    elevator:approaching(EPid, Floor);
	_ ->
	    check_arrived(Pos, EPid, Floors)
    end.

%%----------------------------------------------------------------------
%% check_arrived(Pos, EPid, Floors)
%%  Pos = int()
%%  EPid = pid()
%%  Floors = [{FloorNo, YPos}, ...]
%%  FloorNo = int()
%%  YPos = int()
%%
%% Checks whether an elevator at position Pos is at a floor. If so, the
%% elevator control process EPid is informed.
%%----------------------------------------------------------------------
check_arrived(Pos, EPid, Floors) ->
    case at_floor(Pos, Floors) of
	{true, Floor} ->
	    elevator:at_floor(EPid, Floor);
	false ->
	    ok
    end. 

%%----------------------------------------------------------------------
%% at_floor(Pos, Floors) -> {true, FloorNo} | false
%%  Pos = int()
%%  Floors = [{FloorNo, YPos}, ...]
%%  FloorNo = int()
%%  YPos = int()
%%
%% Checks whether an elevator at position Pos is at a floor.
%%----------------------------------------------------------------------
at_floor(Pos, Floors) ->
    case lists:keysearch(Pos, 2, Floors) of
	{value, {Floor, _}} ->
	    {true, Floor};
	false ->
	    false
    end. 
    
%%----------------------------------------------------------------------
%% get_floor(Pos, Dir, Floors) -> FloorNo
%%  Pos = int()
%%  Dir = nodir | up | down
%%  Floors = [{FloorNo, YPos}, ...]
%%  FloorNo = int()
%%  YPos = int()
%%
%% Retrieves the last floor passed when the graphical elevator is at Pos,
%% travelling in the direction Dir (or standing still at a floor).
%%----------------------------------------------------------------------
get_floor(Pos, nodir, Floors) ->
    {value, {Floor, _}} = lists:keysearch(Pos, 2, Floors),
    Floor;
get_floor(Pos, up, Floors) ->
    find(1, Pos, Floors, infinity, none);
get_floor(Pos, down, Floors) ->
    find(-1, Pos, Floors, infinity, none).

find(_Sign, _Pos, [], _Min, MinFloor) ->
    MinFloor;
find(Sign, Pos, [{_F, Y} | Floors], Min, MinF) when Sign * (Y - Pos) < 0 ->
    find(Sign, Pos, Floors, Min, MinF);
find(Sign, Pos, [{F, Y} | Floors], Min, _MinF) when Sign * (Y - Pos) < Min ->
    find(Sign, Pos, Floors, Sign * (Y - Pos), F);
find(Sign, Pos, [{_F, _Y} | Floors], Min, MinF) ->
    find(Sign, Pos, Floors, Min, MinF).
