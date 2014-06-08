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
-export([]).

%% Imports
-import(werkzeug, [get_config_value/2]).
-include("messages.hrl").
-include("constants.hrl").

start() ->
  {ok, Config} = file:consult("nameservice.cfg"),
  {ok, Nameservername} = get_config_value(name, Config),
  Nameserver = global:whereis_name(Nameservername),
  Nameserver ! {},

spawn_ggt() ->
  noop.