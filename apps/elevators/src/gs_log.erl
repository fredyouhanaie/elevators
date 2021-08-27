%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% A mock module that logs `gs' function calls made by other
%%% modules. This is a temporary replacement until the `wx' based
%%% version has been created.
%%%
%%% This module is written specifically for the elevators
%%% project. Hence we have only provided the `gs' functions used by
%%% the elevators modules.
%%%
%%% @end
%%% Created : 23 Aug 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(gs_log).

-export([start/0, create/3, config/2]).
-export([fbutton/1, ebutton/2, qbutton/0]).

-include_lib("kernel/include/logger.hrl").

-define(GS_owner, {gs_log, display}).

%%--------------------------------------------------------------------

start() ->
    persistent_term:put(?GS_owner, self()),
    ObjId = erlang:make_ref(),
    ?LOG_NOTICE(#{objid => ObjId}),
    ObjId.

%%--------------------------------------------------------------------

create(Widget, Window, Props) ->
    ObjId = erlang:make_ref(),
    ?LOG_NOTICE(#{widget => Widget,
                  window => Window,
                  props => Props,
                  objid => ObjId}),
    erlang:make_ref().

%%--------------------------------------------------------------------

config(Window, Config) ->
    ?LOG_NOTICE(#{window => Window,
                  config => Config}),
    ok.

%%--------------------------------------------------------------------

%% generate event as if the call button on `Floor' has been pressed
%%
fbutton(Floor) ->
    GS_owner_pid = persistent_term:get(?GS_owner),
    GS_owner_pid ! {gs, dummy_obj_id, click, {floor, Floor}, []}.

%%--------------------------------------------------------------------

%% generate event as if the `Floor' button on elevator `Eno' has been
%% pressed
%%
ebutton(ENo, Floor) ->
    GS_owner_pid = persistent_term:get(?GS_owner),
    GS_owner_pid ! {gs, dummy_obj_id, click, {elevator, ENo, Floor}, []}.

%%--------------------------------------------------------------------

%% generate the `Quit' button event
%%
qbutton() ->
    GS_owner_pid = persistent_term:get(?GS_owner),
    GS_owner_pid ! {gs, dummy_obj_id, click, quit, []}.
    
%%--------------------------------------------------------------------
