%%%-------------------------------------------------------------------
%%% @author moritzspindelhirn
%%% @copyright (C) 2014, HAW
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2014 17:01
%%%-------------------------------------------------------------------
-module(koordinator).
-author("moritzspindelhirn").

%% API
-export([start/0]).

%% Imports
-import(werkzeug, [get_config_value/2, logging/2]).
-include("messages.hrl").
-include("constants.hrl").

start() ->
  bind_at_nameserver(),
  reset().

init(skip) ->
  done.
init(GgtNameList) ->
  ShuffledList = werkzeug:shuffle(GgtNameList),

reset() ->
  timer:send_after(get_registertime(), rt_over),
  GgtNameList = register([]).

%%----------------------------------------------------------------------
%% Function: bind_at_nameserver/0
%% Purpose: Register the coordinator at the nameserver
%% Returns: None
%%----------------------------------------------------------------------
bind_at_nameserver() ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?REBIND, "koordinator", node()}},
  receive
    {?REBIND_RES, ?OK} ->
      ?OK;
    Message ->
      log("Unknown Message received: ~p~n", [Message]),
      ?NOK
  end.

%%----------------------------------------------------------------------
%% Function: get_coordinator_name/0
%% Purpose: Get the name to register the coordinator with at the nameserver
%% Args: None
%% Returns: String
%%----------------------------------------------------------------------
register(GgtNameList) ->
  receive
    {get_ggt_vals,PID} ->
      PID ! {ggt_vals, get_ttw(), get_ttt(), get_ggts()},
      register(GgtNameList);
    {check_in, GgtName} ->
      register([GgtName|GgtNameList]);
    {rt_over} ->
      log("Register phase over."),
      GgtNameList;
    {reset} ->
      reset(),
      skip
  end.

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
  Filename = io_lib:format("coord_coord_~p.log", [node()]),
  logging(Filename, Msg);
log(Msg, Params) ->
  log(io_lib:format(Msg, Params), []).

%%----------------------------------------------------------------------
%% Function: get_nameserver/0
%% Purpose: Get the PID of the nameserver
%% Args: None
%% Returns: PID
%%----------------------------------------------------------------------
get_nameserver() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  {ok, Nameservername} = get_config_value(nameserver, Config),
  global:whereis_name(Nameservername).

%%----------------------------------------------------------------------
%% Function: get_registertime/0
%% Purpose: Get the PID of the nameserver
%% Args: None
%% Returns: PID
%%----------------------------------------------------------------------
get_registertime() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  {ok, Sec} = get_config_value(registertime, Config),
  get_as_ms(string:to_integer(Sec)).
get_as_ms({error, Reason}) ->
  %% fallback to default time
  20000;
get_as_ms({Int, _Rest}) ->
  Int.

%%----------------------------------------------------------------------
%% Function: get_ttw/0 get_ttt/0 get_ggts/0
%% Purpose: Get a value from the config.
%% Args: None
%% Returns: String
%%----------------------------------------------------------------------
get_ttw() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  {ok, Sec} = get_config_value(ttw, Config),
  Sec.
get_ttt() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  {ok, Sec} = get_config_value(ttt, Config),
  Sec.
get_ggts() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  {ok, Amount} = get_config_value(ggtprostarter, Config),
  Amount.