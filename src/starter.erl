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
  Koordinator = get_coordinator(Nameserver),
  Config = get_ggt_vals(Koordinator),
  spawn_ggts(Number, Config),
  log("Done").

%%----------------------------------------------------------------------
%% Function: log/1 log/2
%% Purpose: Log the message. If params are given use io_lib:format to format the message with the given params
%% Args: String, []
%%   or: String, [String, ...]
%% Returns: None
%%----------------------------------------------------------------------
log(Msg) ->
  log(Msg, []).
log(Msg, []) ->
  Filename = "",
  logging(Filename, Msg);
log(Msg, Params) ->
  Filename = "",
  logging(Filename, io_lib:format(Msg, Params)).

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
%% Function: get_coordinator/1
%% Purpose: Get the PID of the coordinator by asking the nameserver
%% Args: PID of the nameserver
%% Returns: PID of coordinator
%%      or: error
%%----------------------------------------------------------------------
get_coordinator(Nameserver) ->
  {ok, Config} = file:consult("ggt.cfg"),
  Koordinatorname = get_config_value(coordinator, Config),
  Nameserver ! {?LOOKUP, Koordinatorname},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      error;
    {?LOOKUP_RES, ServiceAtNode} ->
      global:whereis_name(ServiceAtNode)
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
get_ggt_vals(error) ->
  log("Error getting coordinator from nameserver."),
  error;
get_ggt_vals(Koordinator) ->
  Koordinator ! {get_ggt_vals, self()},
  receive
    {ggt_vals, TTW, TTT , GGTs} ->
      {TTW, TTT, GGTs};
    true ->
      log("Received unknown message."),
      error
  end.

%%----------------------------------------------------------------------
%% Function: spawn_ggts/2
%% Purpose: Calls spawn_single_ggt/2 for each process to start.
%% Args: Number, {TTW, TTT, GGTs}
%%   or: Number, error
%% Returns: None
%%----------------------------------------------------------------------
spawn_ggts(_Number, error) ->
  log("[END] - Unable to spawn ggt processes.");
spawn_ggts(_Number, {_TTW, _TTT, 0}) ->
  log("[END] - Started all GGT processes.");
spawn_ggts(Number, {TTW, TTT, GGTs}) ->
  log("Spawn new ggt with ttw=~p and ttt=~p", [TTW, TTT]),
  spawn_single_ggt(Number, GGTs, TTW, TTT),
  spawn_ggts(Number, {TTT, TTW, GGTs - 1}).

%%----------------------------------------------------------------------
%% Function: spawn_single_ggt/4
%% Purpose: Spawn a new ggt process ( ggt:start/6 )
%% Args: StarterNumber, GgtNumber, TTW, TTT
%% Returns: None
%%----------------------------------------------------------------------
spawn_single_ggt(StarterNumber, GgtNumber, TTW, TTT) ->
  Nameserver = get_nameserver(),
  spawn(ggt, start, [StarterNumber, GgtNumber, TTW, TTT, Nameserver, get_coordinator(Nameserver)]).