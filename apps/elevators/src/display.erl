%%%----------------------------------------------------------------------
%%% File    : display.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Graphical display event handler.
%%% Created :  4 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(display).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(GS_MOCK).
-define(GS, ?GS_MOCK).
-else.
-define(GS, gs).
-endif.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

-define(I, 10).     % Pad
-define(BW, 50).    % Quit button width
-define(BH, 30).    % Quit button height

-define(EW, 50).    % Elevator width
-define(EH, 70).    % Elevator height
-define(F, 10).     % Thickness of line denoting limit between floors

-define(FBW, 40).   % Floor button width
-define(FBH, 30).   % Floor button height

-define(EBW, 25).   % Elevator button width
-define(EBH, 25).   % Elevator button height

%%----------------------------------------------------------------------
%% init([Floor, NFloors, EPids])
%%  Initializes the display, with the elevator initially at Floor and
%%  a total of NFloors floors. EPids is a list of the elevator control
%%  process pids.
%%----------------------------------------------------------------------
-spec init([number(), ...]) -> {ok, [{pos_integer(), undefined | pid()}]}.
init([Floor, NFloors, NElevs]) ->
    %% Compute some values needed later on
    CW = NElevs*?EW + 2*?F + (NElevs-1)*?EW, % Canvas width
    CH = NFloors*(?EH+?F),           % Canvas height

    WW = ?I+?FBW+?I+CW+?I,           % Window width
    WH = ?I+CH+?I+((NFloors + 1) div 2)*?EBH+?I+?BH+?I,  % Window height

    %% Create the actual window
    Win = ?GS:create(window, ?GS:start(), [{title, "Elevators"},
                                           {x, 300}, {y, 100},
                                           {width, WW}, {height, WH}]),

    %% This frame holds the floor buttons
    FButtonF = ?GS:create(frame, Win, [{x, ?I}, {y, ?I},
                                       {width, ?FBW}, {height, CH}]),

    %% The canvas is used to draw the floors and elevators on
    Canvas = ?GS:create(canvas, Win, [{x, 2*?I+?FBW}, {y, ?I},
                                      {width, CW}, {height, CH},
                                      {bw, 2}, {relief, raised}]),

    Floors = draw_floors(NFloors, 0, Canvas, CW, FButtonF),

    %% This frame holds the elevator buttons
    EButtonF = ?GS:create(frame, Win, [{x, 2*?I+?FBW}, {y, 2*?I+CH},
                                       {width, CW},
                                       {height, ((NFloors + 1) div 2)*?EBH}]),

    %% Pos is to original position of the elevators (top)
    Pos = CH - Floor*(?EH+?F),

    ElevGs = draw_elevators(1, NElevs, NFloors, ?F, Pos, Canvas, CH, EButtonF),

    %% A quit button completes the window
    _Quit = ?GS:create(button, Win, [{x, WW/2-?BW/2}, {y, WH-?I-?BH},
                                     {width, ?BW}, {height, ?BH},
                                     {label, {text, "Quit"}},
                                     {data, quit}]),

    ?GS:config(Win, {map, true}),

    {ok, start_e_graphics(1, Pos, ElevGs, Floors, [])}.

%%----------------------------------------------------------------------
%% handle_event handles the sys events and dispatches the interesting ones
%% to the graphical elevator.
%%----------------------------------------------------------------------
-spec handle_event({close, pos_integer()} |
                   {f_button, pos_integer()} |
                   {open, pos_integer()} |
                   {stopping, pos_integer()} |
                   {approaching | controller_started | e_button | move | passing | stopped_at,
                    pos_integer(), atom()} |
                   {reset, pos_integer(), term(), pos_integer()},
                   maybe_improper_list()) -> {ok, term()}.
handle_event({open, ENo}, ElevGs) ->
    {value, {_, EG}} = lists:keysearch(ENo, 1, ElevGs),
    e_graphic:open(EG),
    {ok, ElevGs};
handle_event({close, ENo}, ElevGs) ->
    {value, {_, EG}} = lists:keysearch(ENo, 1, ElevGs),
    e_graphic:close(EG),
    {ok, ElevGs};
handle_event({move, ENo, Dir}, ElevGs) ->
    {value, {_, EG}} = lists:keysearch(ENo, 1, ElevGs),
    e_graphic:move(EG, Dir),
    {ok, ElevGs};
handle_event({stopping, ENo}, ElevGs) ->
    {value, {_, EG}} = lists:keysearch(ENo, 1, ElevGs),
    e_graphic:stop(EG),
    {ok, ElevGs};
handle_event({controller_started, ENo, EPid}, ElevGs) ->
    {value, {_, EG}} = lists:keysearch(ENo, 1, ElevGs),
    e_graphic:set_controller(EG, EPid),
    {ok, ElevGs};
%%----------------------------------------------------------------------
%% Events generated by the graphical elevator are ignored...
%%----------------------------------------------------------------------
handle_event({approaching, _ENo, _Floor}, ElevGs) ->
    {ok, ElevGs};
handle_event({stopped_at, _ENo, _Floor}, ElevGs) ->
    {ok, ElevGs};
handle_event({passing, _ENo, _Floor}, ElevGs) ->
    {ok, ElevGs};
handle_event({reset, _ENo, _State, _Floor}, ElevGs) ->
    {ok, ElevGs};
%%----------------------------------------------------------------------
%% ...and so are events generated by ourselves.
%%----------------------------------------------------------------------
handle_event({f_button, _Floor}, ElevGs) ->
    {ok, ElevGs};
handle_event({e_button, _ENo, _Floor}, ElevGs) ->
    {ok, ElevGs}.

%%----------------------------------------------------------------------
%% handle_call not used
%%----------------------------------------------------------------------
-spec handle_call(term(), term()) -> {ok, ok, term()}.
handle_call(_Request, State) ->
    {ok, ok, State}.

%%----------------------------------------------------------------------
%% handle_info takes care of graphic events.
%%----------------------------------------------------------------------
-spec handle_info({gs, term(), click,
                   quit | {floor, pos_integer()} | {elevator, pos_integer(), pos_integer()},
                   term()}, maybe_improper_list()) ->
          {ok, term()}.
handle_info({gs, _Obj, click, {floor, Floor}, _Args}, ElevGs) ->
    scheduler:f_button_pressed(Floor),
    {ok, ElevGs};
handle_info({gs, _Obj, click, {elevator, ENo, Floor}, _Args}, ElevGs) ->
    scheduler:e_button_pressed(ENo, Floor),
    {ok, ElevGs};
handle_info({gs, _Obj, click, quit, _Args}, ElevGs) ->
    case application:get_application() of
        undefined ->
            util:stop();
        {ok, App} ->
            spawn(application, stop, [App])
    end,
    {ok, ElevGs}.

%%----------------------------------------------------------------------
%% terminate has nothing to clean up.
%%----------------------------------------------------------------------
-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% code_change has no state to convert.
%%----------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%--------------------------------------------------------------------------
%%% draw_floors(F, Ybase, Canvas, CW, FButtonF) -> Floors
%%%   F -> 1..6  Floor number, also recursion variable
%%%   Ybase -> int()  Top Y coordinate for floor
%%%   Canvas -> GS object, Canvas on which to draw the floors (= lines)
%%%   CW -> int()  Canvas width in pixels
%%%   FButtonF -> GS object, Frame in which to place the floor buttons
%%%   Floors -> [{F,Y}]  List associating floors with pixel coordinates
%%%
%%% Recursive function for drawing the floors (= lines on the canvas) and
%%% creating the floor buttons.
%%%--------------------------------------------------------------------------
-spec draw_floors(non_neg_integer(), non_neg_integer(), term(), number(), term()) ->
          [{pos_integer(), non_neg_integer()}].
draw_floors(0, _, _, _, _) ->
    [];
draw_floors(F, Ybase, Canvas, CW, FButtonF) ->

    %% Compute where to draw the line and create it
    %% Ybase + floor height - half line width
    Yline = Ybase + (?EH+?F) - ?F/2,
    ?GS:create(line, Canvas, [{coords, [{0,Yline}, {CW,Yline}]},
                              {width, ?F}]),

    %% Compute where to place the button and create it
    %% Ybase + half floor height - half button height
    Ybutton = Ybase + (?EH+?F)/2 - ?FBH/2,
    ?GS:create(button, FButtonF, [{x, 0}, {y, Ybutton},
                                  {width, ?FBW}, {height, ?FBH},
                                  {label, {text, "Come"}},
                                  {data, {floor, F}}]),

    [{F, Ybase}|draw_floors(F-1, Ybase+(?EH+?F), Canvas, CW, FButtonF)].

%%%--------------------------------------------------------------------------
%%% draw_elevators(E, Xbase, Y, Canvas, CH, EButtonF) -> ElevGs
%%%   E -> 1..3  Elevator number, also recursion variable
%%%   Xbase -> int()  Left X coordinate for elevator
%%%   Y -> int()  Top Y coordinate for elevators (same for all three)
%%%   Canvas -> GS object, Canvas on which to draw the elevators (= rects)
%%%   CH -> int()  Canvas height in pixels
%%%   EButtonF -> GS object, Frame in which to place the elevator buttons
%%%   ElevGs -> List of GS objects, the three elevators
%%%
%%% Recursive function for drawing the elevators (= rectangles on the canvas)
%%% and creating the elevator buttons.
%%%--------------------------------------------------------------------------
-spec draw_elevators(pos_integer(), number(), non_neg_integer(), pos_integer(),
                     number(), term(), number(), term()) -> [any()].
draw_elevators(E, NElevs, _, _, _, _, _, _) when E > NElevs ->
    [];
draw_elevators(E, N, NFloors, Xbase, Y, Canvas, CH, EButtonF) ->

    %% Draw the elevator
    ElevGui = ?GS:create(rectangle, Canvas,
                         [{coords,
                           [{Xbase, Y}, {Xbase+?EW, Y+?EH}]},
                          {fill, black}, {bw, 2}]),

    %% Draw its buttons
    draw_buttons(1, E, NFloors, Xbase+?EW/2, 0, EButtonF),

    [ElevGui|draw_elevators(E+1, N, NFloors, Xbase+2*?EW, Y, Canvas,
                            CH, EButtonF)].

%%%--------------------------------------------------------------------------
%%% draw_buttons(B, E, NFloors, X, Y, EButtonF)
%%%   B -> 1..NFloors  Button (floor) number, also recursion variable
%%%   E -> int()       Elevator number
%%%   NFloors -> int() Number of floors
%%%   X -> int()       Middle X coordinate
%%%   Y -> int()       Top Y coordinate
%%%   EButtonF ->      GS object, Frame in which to place the elevator buttons
%%%
%%% Recursive function for drawing the NFloors buttons for the elevator.
%%%--------------------------------------------------------------------------
-spec draw_buttons(pos_integer(), pos_integer(), non_neg_integer(),
                   float(), non_neg_integer(), term()) -> done.
draw_buttons(B, _E, NFloors, _X, _Y, _EButtonF) when B > NFloors ->
    done;
draw_buttons(B, E, NFloors, X, Y, EButtonF) when B rem 2 /=0 ->
    %% Button with odd numbers are placed left of X
    ?GS:create(button, EButtonF, [{x, X-?EBW}, {y, Y},
                                  {width, ?EBW}, {height, ?EBH},
                                  {label, {text, B}},
                                  {data, {elevator, E, B}}]),
    draw_buttons(B+1, E, NFloors, X, Y, EButtonF);
draw_buttons(B, E, NFloors, X, Y, EButtonF) when B rem 2 ==0 ->
    %% Buttons with even numbers are placed right of X
    ?GS:create(button, EButtonF, [{x, X}, {y, Y},
                                  {width, ?EBW}, {height, ?EBH},
                                  {label, {text, B}},
                                  {data, {elevator, E, B}}]),
    %% Increase Y for next two buttons
    draw_buttons(B+1, E, NFloors, X, Y+?EBH, EButtonF).

-spec start_e_graphics(pos_integer(), number(), [any()],
                       [{pos_integer(), non_neg_integer()}],
                       [{pos_integer(), undefined | pid()}]) ->
          [{pos_integer(), undefined | pid()}].
start_e_graphics(_N, _Pos, [], _Floors, Result) ->
    lists:reverse(Result);
start_e_graphics(N, Pos, [EG | ElevGs], Floors, Result) ->
    {ok, GC} =
        case whereis(g_sup) of
            undefined ->
                e_graphic:start_link(Pos, EG, Floors);
            _Pid ->
                supervisor:start_child(g_sup,
                                       {N,
                                        {e_graphic, start_link,
                                         [Pos, EG, Floors]},
                                        permanent, 2000, worker, [e_graphic]})
        end,
    start_e_graphics(N+1, Pos, ElevGs, Floors, [{N, GC} | Result]).
