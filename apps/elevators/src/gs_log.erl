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

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

start() ->
    ObjId = erlang:make_ref(),
    ?LOG_NOTICE("gs:start", #{objid => ObjId}),
    ObjId.

%%--------------------------------------------------------------------

create(Widget, Window, Props) ->
    ObjId = erlang:make_ref(),
    ?LOG_NOTICE("gs:create",
                #{widget => Widget,
                  window => Window,
                  props => Props,
                  objid => ObjId}),
    erlang:make_ref().

%%--------------------------------------------------------------------

config(Window, Config) ->
    ?LOG_NOTICE("gs:config",
                #{window => Window,
                  config => Config}),
    ok.

%%--------------------------------------------------------------------
