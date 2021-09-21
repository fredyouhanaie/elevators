%%%----------------------------------------------------------------------
%%% Copyright (c) 1999, Ericsson Utvecklings AB
%%% File    : elev_sup.erl
%%% Author  : Håkan Huss <haken@erlang.ericsson.se>
%%% Purpose : Elevator process supervisor.
%%% Created : 28 Aug 1999 by Håkan Huss <haken@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elev_sup).
-author('haken@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start_link(pos_integer()) -> supervisor:startlink_ret().
start_link(NElevs) ->
    supervisor:start_link({local, elev_sup}, elev_sup, [NElevs]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init
%%----------------------------------------------------------------------
-spec init([non_neg_integer(), ...]) ->
          {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([NElevs]) ->
    ChildList = make_child_specs(NElevs),
    SupFlags = {one_for_one, 10, 3600},
    {ok,{SupFlags, ChildList}}.

%%----------------------------------------------------------------------
%% make_child_specs(NElevs)
%%  Returns a list of child specifications for NElevs elevator control
%%  processes.
%%----------------------------------------------------------------------
-spec make_child_specs(non_neg_integer()) -> [supervisor:child_spec()].
make_child_specs(0) -> [];
make_child_specs(NElevs) ->
    [{NElevs,{elevator,start_link,[NElevs]},permanent,2000,worker,[elevator]}|
     make_child_specs(NElevs-1)].

