%%%-------------------------------------------------------------------
%%% @author Tobi
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2014 19:36
%%%-------------------------------------------------------------------
-module(ggt).
-author("Tobi").

%% API
-export([start/6]).

%% Imports
-import(werkzeug, [get_config_value/2, logging/2]).
-include("messages.hrl").
-include("constants.hrl").

%% Defines
-define(TEAMNR, 9).
-define(PRAKNR, 3).
-define(HOSTNAME, "toabs").
-define(I, "(initial)::").
-define(PP, "(pre-process)::").
-define(P, "(processing)::").


start(StarterNumber, GgtNumber, TTW, TTT, Nameserver, Coordinator) ->
  init(io_lib:format("39~s_~s", [GgtNumber, StarterNumber]), TTW, TTT, Nameserver, Coordinator).


init(name, TTW, TTT, Nameserver, Coordinator) ->
  Coordinator ! {check_in, name},
  log(name, "~s~s checking in to coordinator ~s as service '~s':~s", [?I, self(), Coordinator, name, now()]),
  Nameserver ! {self(),{rebind, name, node()}},
  log(name, "~sregistered at ~s:~s", [?I, Nameserver, now()]),
  global:register_name(name, self()),
  log("~s~s globally registered as ~s:~s", [?I, self(), name, now()]),

  receive
    {set_neighbours,LeftN,RightN} ->
      log(name, "~sreceived neighbours and is changing to pre_process:~s", [?I, now()]),
      pre_process(name, 0, TTW, TTT, Nameserver, Coordinator, LeftN, RightN);

    {whats_on, From} ->
      From ! {i_am, "init"},
      log(name, "~ssend status init to ~s:~s", [?I, From, now()]),
      init(name, TTW, TTT, Nameserver, Coordinator);

    {kill} ->
      die(name, Nameserver)
  end.

pre_process(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN) ->
  log(name, "~swaiting for Mi to be set:~s", [?PP, now()]),
  receive
    {set_pmi, MiNeu} ->
      process_pmi(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu);

    {whats_on, From} ->
      From ! {i_am, "pre_process"},
      log(name, "~ssend status init to ~s:~s", [?PP, From, now()]),
      pre_process(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN);

    {kill} ->
      die(name, Nameserver)
  end.

process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi) ->
  receive
    {set_pmi, MiNeu} ->
      process_pmi(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu);

    {send,Y} ->
      log(name, "~sreceived a send y=~s:~s", [?P, Y, now()]),
      if
        Y < Mi ->
          log(name, "~scalculating new Mi:~s", [?P, now()]),
          timer:sleep(TTW),
          MiNew = (Mi-1) rem (Y+1),
          LeftN ! {send, MiNew},
          RightN ! {send, MiNew},
          Coordinator ! {brief_mi,{name, MiNew, now()}},
          log(name, "~snotified neighbours and coordinator about new Mi = ~s:~s", [?P, MiNew, now()]),
          NewTimer = werkzeug:reset_timer(Timer, TTT * 1000, {start_vote}),
          TSN = now(),
          log(name, "~stimer was reset:~s", [?P, now()]),
          process(name, TSN, TTW, NewTimer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

        true ->
          process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi)
      end;

    {vote,Initiator} ->
      log(name, "~sreceived a vote:~s", [?P, now()]),
      if
        Initiator =:= name ->
          Coordinator !{brief_term, {name, Mi, now()}, self()},
          log(name, "~sinform coordinator about terminate:~s", [?P, now()]);

        true ->
          TSD = timer:now_diff(now(), TS),
          if
            TSD >= TTT div 2 ->
              log(name, "~sinform neightbour about vote:~s", [?P, now()]),
              LeftN ! {vote, Initiator};

          true ->
            process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi)
          end
      end;

    {start_vote} ->
      TSD = timer:now_diff(now(), TS),
      if
        TSD >= TTT ->
          log(name, "~sstarting a vote:~s", [?P, now()]),
          LeftN ! {vote, name},
          process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

      true ->
        process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi)
      end;

    {tell_mi, From} ->
      From ! {mi, Mi},
      process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

    {whats_on,From} ->
      From ! {i_am, "Process"},
      process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

    {kill} ->
      die(name, Nameserver)
  end.

process_pmi(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu) ->
  log(name, "~sreveived set_pmi ~s also reset the timer:~s", [?PP, MiNeu, now()]),
  {ok, Timer} = timer:send_after(TTT * 1000, {start_vote}),
  TS = now(),
  process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu).

die(name, Nameserver) ->
  log(name, "(dying)::unregistering ~s from ~s:~s", [name, Nameserver, now()]),
  Nameserver ! {self(),{unbind,name}},
  log(name, "(dying)::globally unbinding '~s' with ~s:~s", [name, self(), now()]),
  global:unregister_name(name).


%%----------------------------------------------------------------------
%% Function: log/2 log/3
%% Purpose: Log the message. If params are given use io_lib:format to format the message with the given params
%% Args: String, []
%%   or: String, [String, ...]
%% Returns: None
%%----------------------------------------------------------------------
log(Name, Msg) ->
  log(Name, Msg, []).
log(Name, Msg, []) ->
  Filename = io_lib:format("~s_ggt_~p@~s.log", [Name, node(), ?HOSTNAME]),
  logging(Filename, Msg);
log(Name, Msg, Params) ->
  log(Name, io_lib:format("ggt:'~s' ~s", [Name, io_lib:format(Msg, Params)])).
