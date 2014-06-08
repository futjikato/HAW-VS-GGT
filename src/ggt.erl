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

%% Defines
-define(TEAMNR, 9).
-define(PRAKNR, 3).


start(StarterNumber, GgtNumber, TTW, TTT, Nameserver, Coordinator) ->
  init(io_lib:format("39~s_~s", [GgtNumber, StarterNumber]), TTW, TTT, Nameserver, Coordinator).


init(name, TTW, TTT, Nameserver, Coordinator) ->
  Coordinator ! {check_in, name},
  Nameserver ! {self(),{rebind, name, node()}},
  global:register_name(name, self()),
  receive
    {set_neighbours,LeftN,RightN} ->
      pre_process(name, 0, TTW, TTT, Nameserver, Coordinator, LeftN, RightN);

    {whats_on, From} ->
      From ! {i_am, "init"},
      init(name, TTW, TTT, Nameserver, Coordinator);

    {kill} ->
      noop
  end.

pre_process(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN) ->
  receive
    {set_pmi, MiNeu} ->
      {ok, Timer} = timer:send_after(TTT * 1000, {start_vote, self()}),
      TS = now(),
      process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu);

    {whats_on, From} ->
      From ! {i_am, "pre_process"},
      pre_process(name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN);

    {kill} ->
      noop
  end.

process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi) ->
  receive
    {send,Y} ->
      if
        Y < Mi ->
          timer:sleep(TTW),
          MiNew = (Mi-1) rem (Y+1),
          LeftN ! {send, MiNew},
          RightN ! {send, MiNew},
          Coordinator ! {brief_mi,{name, MiNew, now()}},
          NewTimer = werkzeug:reset_timer(Timer, TTT * 1000, {start_vote, _}),
          TSN = now(),
          process(name, TSN, TTW, NewTimer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

        true ->
          process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi)
      end;

    {vote,Initiator} ->
      if
        Initiator =:= name ->
          Coordinator !{brief_term, {name, Mi, now()}, self()};

        true ->
          if
            endtimer:now_diff(now(), TS) >= TTT div 2 ->
              LeftN ! {vote, Initiator};

          true ->
            process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi)
          end
      end;

    {start_vote, _} ->
      LeftN ! {vote, name},
      process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

    {tell_mi, From} ->
      From ! {mi, Mi},
      process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

    {whats_on,From} ->
      From ! {i_am, "Process"},
      process(name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi);

    {kill} ->
      noop
  end.


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
  Filename = io_lib:format("~s_ggt_~p.log", [Name, node()]),
  logging(Filename, Msg);
log(Name, Msg, Params) ->
  log(Name, io_lib:format(Msg, Params), []).
