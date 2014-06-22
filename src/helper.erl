%%%-------------------------------------------------------------------
%%% @author Tobi
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jun 2014 20:10
%%%-------------------------------------------------------------------
-module(helper).
-author("Tobi").

%% API
-export([start_koor/1, start_starter/2, trigger_calc/2]).



start_koor(Nameservice) ->
  net_adm:ping(Nameservice),
  koordinator:start().

start_starter(Nameservice, StarterNumber) ->
  net_adm:ping(Nameservice),
  starter:start(StarterNumber).

trigger_calc(Koordinator, Number) ->
  net_adm:ping(Koordinator),
  K = global:whereis_name(koordinator),
  K ! {calc, Number}.