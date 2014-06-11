%%%-------------------------------------------------------------------
%%% @author moritzspindelhirn
%%% @copyright (C) 2014, HAW
%%% @doc
%%% Starter for the Ggt-processes
%%%
%%% @end
%%% Created : 08. Jun 2014 13:42
%%%-------------------------------------------------------------------
-module(starter).
-author("moritzspindelhirn").

%% API
-export([start/1]).

%% Imports
-import(werkzeug, [get_config_value/2, logging/2]).
-include("messages.hrl").
-include("constants.hrl").

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Start the starter module.
%%          After start the starter will contact the nameserver for the coordinator
%%          Name and then ask the coordinator for the starter config values.
%%          After that the starter will start ggt processes in the ggt module ( ggt:start/6 ).
%%          For more information on the start of the ggt modules see the spawn_single_ggt/4 function.
%% Args: String, []
%%   or: String, [String, ...]
%% Returns: None
%%----------------------------------------------------------------------
start(Number) ->
  Nameserver = get_nameserver(),
  Koordinator = get_coordinator(Number, Nameserver),
  Config = get_ggt_vals(Number, Koordinator),
  spawn_ggts(Number, Config),
  log(Number, "Done~n").

%%----------------------------------------------------------------------
%% Function: log/2 log/3
%% Purpose: Log the message. If params are given use io_lib:format to format the message with the given params
%% Args: String, []
%%   or: String, [String, ...]
%% Returns: None
%%----------------------------------------------------------------------
log(Number, Msg) ->
  log(Number, Msg, []).
log(Number, Msg, []) ->
  Filename = io_lib:format("starter~p_ggt_~p.log", [Number, node()]),
  logging(Filename, Msg);
log(Number, Msg, Params) ->
  log(Number, io_lib:format(Msg, Params), []).

%%----------------------------------------------------------------------
%% Function: get_nameserver/0
%% Purpose: Get the PID of the nameserver
%% Args: None
%% Returns: PID
%%----------------------------------------------------------------------
get_nameserver() ->
  {ok, Config} = file:consult("ggt.cfg"),
  {ok, Nameservername} = get_config_value(nameserver, Config),
  global:whereis_name(Nameservername).

%%----------------------------------------------------------------------
%% Function: get_coordinator/2
%% Purpose: Get the PID of the coordinator by asking the nameserver
%% Args: PID of the nameserver
%% Returns: PID of coordinator
%%      or: error
%%----------------------------------------------------------------------
get_coordinator(_Number, Nameserver) ->
  {ok, Config} = file:consult("ggt.cfg"),
  {ok, Koordinatorname} = get_config_value(coordinator, Config),
  Nameserver ! {self(), {?LOOKUP, Koordinatorname}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      error;
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode
  end.

%%----------------------------------------------------------------------
%% Function: get_ggt_vals/1
%% Purpose: Ask the coordinator for ggt config values.
%%          If error is given simply log the error and return error again.
%% Args: PID of the coordinator
%%   or: error
%% Returns: {TTW, TTT, GGTs}
%%      or: error
%%----------------------------------------------------------------------
get_ggt_vals(Number, error) ->
  log(Number, "Error getting coordinator from nameserver.~n"),
  error;
get_ggt_vals(Number, Koordinator) ->
  Koordinator ! {get_ggt_vals, self()},
  receive
    {ggt_vals, TTW, TTT , GGTs} ->
      {TTW, TTT, GGTs};
    Unknown ->
      log(Number, "Received unknown message. ~p~n", [Unknown]),
      error
  end.

%%----------------------------------------------------------------------
%% Function: spawn_ggts/2
%% Purpose: Calls spawn_single_ggt/2 for each process to start.
%% Args: Number, {TTW, TTT, GGTs}
%%   or: Number, error
%% Returns: None
%%----------------------------------------------------------------------
spawn_ggts(Number, error) ->
  log(Number, "[END] - Unable to spawn ggt processes.~n");
spawn_ggts(Number, {_TTW, _TTT, 0}) ->
  log(Number, "[END] - Started all GGT processes.~n");
spawn_ggts(Number, {TTW, TTT, GGTs}) ->
  spawn_single_ggt(Number, GGTs, TTW, TTT),
  spawn_ggts(Number, {TTW, TTT, GGTs - 1}).

%%----------------------------------------------------------------------
%% Function: spawn_single_ggt/4
%% Purpose: Spawn a new ggt process ( ggt:start/6 )
%% Args: StarterNumber, GgtNumber, TTW, TTT
%% Returns: None
%%----------------------------------------------------------------------
spawn_single_ggt(StarterNumber, GgtNumber, TTW, TTT) ->
  Nameserver = get_nameserver(),
  Coordinator = get_coordinator(StarterNumber, Nameserver),
  NewPID = spawn(ggt, start, [StarterNumber, GgtNumber, TTW, TTT, Nameserver, Coordinator]),
  log(StarterNumber, "Started new ggt process with values (~p, ~p, ~p, ~p, ~p, ~p) with PID ~p~n", [
    StarterNumber,
    GgtNumber,
    TTW,
    TTT,
    Nameserver,
    Coordinator,
    NewPID
  ]).
