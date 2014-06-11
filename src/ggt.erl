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
  init(lists:concat(["39",GgtNumber,'_', StarterNumber]), TTW, TTT, Nameserver, Coordinator, "init").


init(Name, TTW, TTT, Nameserver, Coordinator, Status) ->
  register(list_to_atom(Name), self()),
  Coordinator ! {check_in, list_to_atom(Name)},
  log(Name, "~s~p checking in to coordinator ~p as service '~s':~s", [?I, self(), Coordinator, Name, werkzeug:timeMilliSecond()]),
  Nameserver ! {self(),{rebind, list_to_atom(Name), node()}},
  log(Name, "~sregistered at ~p:~s", [?I, Nameserver, werkzeug:timeMilliSecond()]),
  global:register_name(list_to_atom(Name), self()),
  log(Name, "~s~p globally registered as ~s:~s", [?I, self(), Name, werkzeug:timeMilliSecond()]),

  receive
    {set_neighbours,LeftN,RightN} ->
      log(Name, "~sreceived neighbours and is changing to pre_process:~s", [?I, werkzeug:timeMilliSecond()]),
      pre_process(Name, 0, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, "received neighbous is now waiting for PMi");

    {whats_on, From} ->
      From ! {i_am, Status},
      log(Name, "~ssend status init to ~p:~s", [?I, From, werkzeug:timeMilliSecond()]),
      init(Name, TTW, TTT, Nameserver, Coordinator, Status);

    {kill} ->
      die(Name, Nameserver)
  end.

pre_process(Name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, Status) ->
  log(Name, "~swaiting for Mi to be set:~s", [?PP, werkzeug:timeMilliSecond()]),
  receive
    {set_pmi, MiNeu} ->
      log(Name, "~sreceived Mi = ~p:~s", [?PP, MiNeu, werkzeug:timeMilliSecond()]),
      process_pmi(Name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu);

    {whats_on, From} ->
      From ! {i_am, "pre_process"},
      log(Name, "~ssend status init to ~p:~s", [?PP, From, werkzeug:timeMilliSecond()]),
      pre_process(Name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, Status);

    {kill} ->
      die(Name, Nameserver)
  end.

process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, Status) ->
  receive
    {set_pmi, MiNeu} ->
      process_pmi(Name, TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu);

    {send,Y} ->
      log(Name, "~sreceived a send y=~p:~s", [?P, Y, werkzeug:timeMilliSecond()]),
      if
        Y < Mi ->
          log(Name, "~scalculating new Mi:~s", [?P, werkzeug:timeMilliSecond()]),
	  PIDWorker = spawn(fun() -> worker(TTW, Mi, Y, self()) end),
	  MiNew = dispatcherLoop(PIDWorker, Mi, Name, Nameserver),
          %timer:sleep(TTW),
          %MiNew = (Mi-1) rem (Y+1),
          LeftN ! {send, MiNew},
          RightN ! {send, MiNew},
          Coordinator ! {brief_mi,{Name, MiNew, werkzeug:timeMilliSecond()}},
          log(Name, "~snotified neighbours and coordinator about new Mi = ~p:~s", [?P, MiNew, werkzeug:timeMilliSecond()]),
          NewTimer = werkzeug:reset_timer(Timer, TTT * 1000, {start_vote}),
          TSN = now(),
          log(Name, "~stimer was reset:~s", [?P, werkzeug:timeMilliSecond()]),
          process(Name, TSN, TTW, NewTimer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, lists:concat(["calculated new Mi = ", MiNew]));

        true ->
          process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, "refused to work with new Y because it is smaller than Mi")
      end;

    {vote,Initiator} ->
      log(Name, "~sreceived a vote:~s", [?P, werkzeug:timeMilliSecond()]),
      if
        Initiator =:= Name ->
          Coordinator !{brief_term, {Name, Mi, werkzeug:timeMilliSecond()}, self()},
          log(Name, "~sinform coordinator about terminate:~s", [?P, werkzeug:timeMilliSecond()]),
          process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, "informed coordinator about termination");

        true ->
          TSD = timer:now_diff(now(), TS),
          if
            TSD >= TTT div 2 ->
              log(Name, "~sinform neightbour about vote:~s", [?P, werkzeug:timeMilliSecond()]),
              LeftN ! {vote, Initiator},
              process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, "voted for termination");

          true ->
	    log(Name, "~sAbstimmung verworfen (letztes Event ist ~p ms her):~s", [?P, TSD, werkzeug:timeMilliSecond()]),
            process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, "rejected vote to terminate")
          end
      end;

    {start_vote} ->
      TSD = timer:now_diff(now(), TS),
      if
        TSD >= TTT ->
          log(Name, "~sstarting a vote:~s", [?P, werkzeug:timeMilliSecond()]),
          LeftN ! {vote, Name},
          process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, "started vote");

      true ->
        process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, Status)
      end;

    {tell_mi, From} ->
      From ! {mi, Mi},
      process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, Status);

    {whats_on,From} ->
      From ! {i_am, Status},
      process(Name, TS, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, Mi, Status);

    {kill} ->
      die(Name, Nameserver)
  end.

worker(TTW, Mi, Y, PID) ->
  timer:sleep(TTW),
  MiNew = (Mi-1) rem (Y+1),
  PID ! {result, MiNew}.

dispatcherLoop(PIDW, Mi, Name, NS) -> 
  receive
    {whats_on, From} ->
      From ! {i_am, "sleeping/working"},
      dispatcherLoop(PIDW, Mi, Name, NS);
    {result, MiNew} ->
      MiNew;
    {kill} ->
      exit(PIDW, kill),
      die(Name, NS)
  end.

process_pmi(Name, _TS, TTW, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu) ->
  log(Name, "~sreveived set_pmi ~p also reset the timer:~s", [?PP, MiNeu, werkzeug:timeMilliSecond()]),
  {ok, Timer} = timer:send_after(TTT * 1000, {start_vote}),
  TSNew = now(),
  process(Name, TSNew, TTW, Timer, TTT, Nameserver, Coordinator, LeftN, RightN, MiNeu, "got a new Mi to work with").

die(Name, Nameserver) ->
  log(Name, "(dying)::unregistering ~s from ~p:~s", [Name, Nameserver, werkzeug:timeMilliSecond()]),
  Nameserver ! {self(),{unbind,list_to_atom(Name)}},
  log(Name, "(dying)::globally unbinding '~s' with ~p:~s", [Name, self(), werkzeug:timeMilliSecond()]),
  global:unregister_name(list_to_atom(Name)).


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
  log(Name, io_lib:format("ggt:'~s' ~s~n", [Name, io_lib:format(Msg, Params)])).
