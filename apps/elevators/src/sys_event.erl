%%%----------------------------------------------------------------------
%%% File    : sys_event.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : The system event manager.
%%% Created :  3 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(sys_event).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

%% External exports
-export([start_link/1, add_handler/2]).
-export([initialized/3, open/1, close/1, move/2, stopping/1,
         approaching/2, stopped_at/2, passing/2,
         f_button_pressed/1, e_button_pressed/2,
         controller_started/2]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start_link([any()]) -> {error, term()} | {ok, pid()}.
start_link(Handlers) ->
    Ret = gen_event:start_link({local, sys_event}),
    lists:foreach(fun({Name, Arg}) -> add_handler(Name, Arg) end, Handlers),
    Ret.

-spec add_handler(atom(), [term()]) -> any().
add_handler(Module, Args) ->
    gen_event:add_handler(sys_event, Module, Args).

%%----------------------------------------------------------------------
%% initialized(ENo, State, Floor)
%%  An elevator has been initialized.
%%----------------------------------------------------------------------
-spec initialized(pos_integer(), term(), pos_integer()) -> ok.
initialized(ENo, State, Floor) ->
    gen_event:notify(sys_event, {reset, ENo, State, Floor}).

%%----------------------------------------------------------------------
%% open(ENo)
%%  The doors of an elevator have opened.
%%----------------------------------------------------------------------
-spec open(pos_integer()) -> ok.
open(ENo) ->
    gen_event:notify(sys_event, {open, ENo}).

%%----------------------------------------------------------------------
%% close(ENo)
%%  The doors of an elevator have closed.
%%----------------------------------------------------------------------
-spec close(pos_integer()) -> ok.
close(ENo) ->
    gen_event:notify(sys_event, {close, ENo}).

%%----------------------------------------------------------------------
%% move(ENo, Dir)
%%  An elevator has started moving in the direction Dir.
%%
%% Types:
%%  Dir = up | down
%%----------------------------------------------------------------------
-spec move(pos_integer(), up | down) -> ok.
move(ENo, Dir) ->
    gen_event:notify(sys_event, {move, ENo, Dir}).

%%----------------------------------------------------------------------
%% stop(ENo)
%%  An elevator will stop at the next floor.
%%----------------------------------------------------------------------
-spec stopping(pos_integer()) -> ok.
stopping(ENo) ->
    gen_event:notify(sys_event, {stopping, ENo}).

%%----------------------------------------------------------------------
%% approaching(ENo, Floor)
%%  An elevator is nearing a floor.
%%----------------------------------------------------------------------
-spec approaching(pos_integer(), pos_integer()) -> ok.
approaching(ENo, Floor) ->
    gen_event:notify(sys_event, {approaching, ENo, Floor}).

%%----------------------------------------------------------------------
%% stopped_at(ENo, Floor)
%%  An elevator has stopped at a floor.
%%----------------------------------------------------------------------
-spec stopped_at(pos_integer(), pos_integer()) -> ok.
stopped_at(ENo, Floor) ->
    gen_event:notify(sys_event, {stopped_at, ENo, Floor}).

%%----------------------------------------------------------------------
%% passing(ENo, Floor)
%%  An elevator is passing a floor.
%%----------------------------------------------------------------------
-spec passing(pos_integer(), pos_integer()) -> ok.
passing(ENo, Floor) ->
    gen_event:notify(sys_event, {passing, ENo, Floor}).

%%----------------------------------------------------------------------
%% e_button_pressed(ENo, Floor)
%%  A floor button in an elevator has been pressed.
%%----------------------------------------------------------------------
-spec e_button_pressed(pos_integer(), pos_integer()) -> ok.
e_button_pressed(ENo, Floor) ->
    gen_event:notify(sys_event, {e_button, ENo, Floor}).

%%----------------------------------------------------------------------
%% f_button_pressed(Floor)
%%  A call button has been pressed on a floor.
%%----------------------------------------------------------------------
-spec f_button_pressed(pos_integer()) -> ok.
f_button_pressed(Floor) ->
    gen_event:notify(sys_event, {f_button, Floor}).

%%----------------------------------------------------------------------
%% controller_started(ENo, EPid)
%%  An elevator control process has been started (or restarted).
%%----------------------------------------------------------------------
-spec controller_started(pos_integer(), pid()) -> ok.
controller_started(ENo, EPid) ->
    gen_event:notify(sys_event, {controller_started, ENo, EPid}).
