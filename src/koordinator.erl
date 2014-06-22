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
  timer:send_after(get_registertime(), self(), rt_over),
  workloop(register, [], 0).

workloop(register, GgtNameList, ToggleFlag) ->
  log("Registertime ~pms~n", [get_registertime()]),
  receive
    {get_ggt_vals,PID} ->
      log("Send ggt vals to ~p~n", [PID]),
      PID ! {ggt_vals, get_ttw(), get_ttt(), get_ggts()},
      workloop(register, GgtNameList, ToggleFlag);
    {check_in, GgtName} ->
      workloop(register, [GgtName|GgtNameList], ToggleFlag);
    rt_over ->
      log("Register phase over.~n"),
      workloop(step, GgtNameList, ToggleFlag);
    reset ->
      workloop(register, [], ToggleFlag);
    step ->
      workloop(step, GgtNameList, ToggleFlag);
    kill ->
      kill_ggts(GgtNameList),
      log("kill command received.~n");
    toggle ->
      workloop(register, GgtNameList, switch_toggle(ToggleFlag));
    prompt ->
      ask_ggt_tell_mi(GgtNameList),
      workloop(step, GgtNameList, ToggleFlag);
    whats_on ->
      ask_ggt_whats_on(GgtNameList),
      workloop(step, GgtNameList, ToggleFlag);
    Unknown ->
      log("Received unknown message: ~p~n", [Unknown]),
      workloop(register, GgtNameList, ToggleFlag)
  end;
workloop(step, GgtNameList, ToggleFlag) ->
  ShuffledList = werkzeug:shuffle(GgtNameList),
  LastElem = get_last(ShuffledList),
  [FirstElem|Tail] = ShuffledList,
  send_ring(none, FirstElem, FirstElem, LastElem, Tail),
  receive
    {calc,WggT} ->
      pre_ready(GgtNameList, WggT),
      workloop(ready, GgtNameList, WggT, ToggleFlag, -1);
    reset ->
      workloop(register, [], ToggleFlag);
    step ->
      workloop(step, ShuffledList, ToggleFlag);
    kill ->
      kill_ggts(ShuffledList),
      log("kill command received.");
    toggle ->
      workloop(step, ShuffledList, switch_toggle(ToggleFlag));
    prompt ->
      ask_ggt_tell_mi(ShuffledList),
      workloop(step, ShuffledList, ToggleFlag);
    whats_on ->
      ask_ggt_whats_on(ShuffledList),
      workloop(step, ShuffledList, ToggleFlag);
    Unknown ->
      log("Received unknown message: ~p~n", [Unknown]),
      workloop(step, GgtNameList, ToggleFlag)
  end.

pre_ready(GgtNameList, WggT) ->
  StartValues = werkzeug:bestimme_mis(WggT, length(GgtNameList)),
  send_mi(GgtNameList, StartValues, length(GgtNameList)),
  SendValues = werkzeug:bestimme_mis(WggT, length(GgtNameList)),
  send_y(GgtNameList, SendValues, max(2, round(length(GgtNameList) / 100 * 15))).

workloop(ready, GgtNameList, WggT, ToggleFlag, CurrentMi) ->
  receive
    {brief_mi, {GgtName, GgTMi, GgTZeit}} ->
      log("Received brief_mi with value ~p from ~s at ~s~n", [GgTMi, GgtName, GgTZeit]),
      workloop(ready, GgtNameList, WggT, ToggleFlag, min(GgTMi, CurrentMi));
    {brief_term, {GgtName, GgTMi, GgTZeit}, FromPID} ->
      NewCurrentMi = received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, FromPID, ToggleFlag),
      workloop(ready, GgtNameList, WggT, ToggleFlag, NewCurrentMi);
    reset ->
      workloop(register, [], ToggleFlag);
    step ->
      workloop(step, GgtNameList, ToggleFlag);
    kill ->
      kill_ggts(GgtNameList),
      log("kill command received.");
    toggle ->
      workloop(ready, GgtNameList, WggT, switch_toggle(ToggleFlag), CurrentMi);
    prompt ->
      ask_ggt_tell_mi(GgtNameList),
      workloop(ready, GgtNameList, WggT, ToggleFlag, CurrentMi);
    whats_on ->
      ask_ggt_whats_on(GgtNameList),
      workloop(ready, GgtNameList, WggT, ToggleFlag, CurrentMi);
    Unknown ->
      log("Received unknown message: ~p~n", [Unknown]),
      workloop(ready, GgtNameList, WggT, ToggleFlag, CurrentMi)
  end.

send_mi([], _StartValues, _Counter) ->
  log("Send value to all ggt processes"); %% only possible if <= 2 ggt processes where registered
send_mi(_GgtNameList, [], _Counter) ->
  log("[SEVERE] - Send all starter values"); %% should never ever occure
send_mi(_GgtNameList, _StartValues, 0)  ->
  log("Done sending.~n");
send_mi([Name], [Value], Counter) ->
  send_pmi(Name, Value),
  send_mi([], [], Counter - 1);
send_mi([Name|Tail], [Value], Counter) ->
  send_pmi(Name, Value),
  send_mi(Tail, [], Counter - 1);
send_mi([Name], [Value|Tail], Counter) ->
  send_pmi(Name, Value),
  send_mi([], Tail, Counter - 1);
send_mi([Name|NameTail], [Value|ValueTail], Counter) ->
  send_pmi(Name, Value),
  send_mi(NameTail, ValueTail, Counter - 1).


send_pmi(Name, Value) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send pmi to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {set_pmi, Value},
      log("Send pmi ~p to ~s~n", [Value, Name])
  end.

send_y([], _StartValues, _Counter) ->
  log("Send value to all ggt processes"); %% only possible if <= 2 ggt processes where registered
send_y(_GgtNameList, [], _Counter) ->
  log("[SEVERE] - Send all starter values"); %% should never ever occure
send_y(_GgtNameList, _StartValues, 0)  ->
  log("Done sending.~n");
send_y([Name], [Value], Counter) ->
  send_sy(Name, Value),
  send_y([], [], Counter - 1);
send_y([Name|Tail], [Value], Counter) ->
  send_sy(Name, Value),
  send_y(Tail, [], Counter - 1);
send_y([Name], [Value|Tail], Counter) ->
  send_sy(Name, Value),
  send_y([], Tail, Counter - 1);
send_y([Name|NameTail], [Value|ValueTail], Counter) ->
  send_sy(Name, Value),
  send_y(NameTail, ValueTail, Counter - 1).

send_sy(Name, Value) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send y to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {send, Value},
      log("Send value ~p to ~s~n", [Value, Name])
  end.


received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, _ToggleFlag) when GgTMi < CurrentMi ->
  log("Received new mi ~p from ~s at ~s~n", [GgTMi, GgtName, GgTZeit]),
  GgTMi;
received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, _ToggleFlag) when GgTMi == CurrentMi ->
  log("Received same mi ~p from ~s at ~s~n", [GgTMi, GgtName, GgTZeit]),
  CurrentMi;
received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, ToggleFlag) when ToggleFlag == 1 , GgTMi > CurrentMi ->
  log("Received to hight mi ~p from ~s at ~s with toggle flag set to notification.~n", [GgTMi, GgtName, GgTZeit]),
  spawn(fun() -> send_value(GgtName, CurrentMi) end),
  CurrentMi;
received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, _ToggleFlag) when GgTMi > CurrentMi ->
  log("Received to hight mi ~p from ~s at ~s with toggle flag set to ignore.~n", [GgTMi, GgtName, GgTZeit]),
  CurrentMi.

send_value(Name, Value) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send new mi value to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {send, Value},
      log("Send value ~p to ~s~n", [Value, Name])
  end.


%%----------------------------------------------------------------------
%% Function: kill_ggts/1
%% Purpose: Send a kill command to all known ggt processes
%%----------------------------------------------------------------------
kill_ggts([]) ->
  noop;
kill_ggts([Name]) ->
  send_kill(Name);
kill_ggts([Name|Tail]) ->
  send_kill(Name),
  kill_ggts(Tail).

send_kill(Name) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send kill command to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {kill},
      log("Send kill to ~s~n", [Name])
  end.

%%----------------------------------------------------------------------
%% Function: bind_at_nameserver/0
%% Purpose: Register the coordinator at the nameserver
%% Returns: None
%%----------------------------------------------------------------------
bind_at_nameserver() ->
  register(koordinator, self()),
  global:register_name(koordinator, self()),
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?REBIND, koordinator, node()}},
  receive
    {?REBIND_RES, ?OK} ->
      log("Bind ok"),
      ?OK;
    Message ->
      log("Unknown Message received: ~p~n", [Message]),
      ?NOK
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
get_as_ms({error, _Reason}) ->
  %% fallback to default time
  20000;
get_as_ms({Int, _Rest}) ->
  Int * 1000.

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

%%----------------------------------------------------------------------
%% Function: switch_toggle/1ShuffledList
%% Purpose: toggle toggle flag
%% Args: 1
%%   or: 0
%% Returns: 0
%%      or: 1
%%----------------------------------------------------------------------
switch_toggle(0) -> 1;
switch_toggle(1) -> 0.

send_ring(none, Current, First, LastElem, [Head|Tail]) ->
  spawn(fun() -> send_nei(Current, LastElem, Head) end),
  send_ring(Current, Head, First, LastElem, Tail);
send_ring(Last, Current, First, LastElem, [_AgainLast]) ->
  spawn(fun() -> send_nei(Current, Last, LastElem) end),
  spawn(fun() -> send_nei(LastElem, Current, First) end);
send_ring(Last, Current, First, LastElem, [Head|Tail]) ->
  spawn(fun() -> send_nei(Current, Last, Head) end),
  send_ring(Current, Head, First, LastElem, Tail).


send_nei(Receiver, LeftN, RighN) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Receiver}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("state(init) Unable to send set_neighbours to client. Nameserver does not know ~p~n", [Receiver]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {set_neighbours, LeftN, RighN},
      log("Send neighbours (~p / ~p) to ~s~n", [LeftN, RighN, Receiver])
  end.

ask_ggt_tell_mi([Name]) ->
  send_tell_mi(Name);
ask_ggt_tell_mi([Name|Tail]) ->
  send_tell_mi(Name),
  ask_ggt_tell_mi(Tail).

ask_ggt_whats_on([]) ->
  log("No ggts to ask");
ask_ggt_whats_on([Name]) ->
  send_whats_on(Name);
ask_ggt_whats_on([Name|Tail]) ->
  send_whats_on(Name),
  ask_ggt_whats_on(Tail).

send_whats_on(Name) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send whats_on to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {?WHATSON, self()},
      log("Send whats_on to ~s~n", [Name]),
      receive
        {?WHATSON_RES, Status} ->
          log("Received ~s from ~s~n", [Status, Name]);
        Unknown ->
          log("Received unknown response for whats_on from ~s : ~s~n", [Name, Unknown])
      end
  end.

send_tell_mi(Name) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send tell_mi to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      ServiceAtNode ! {?TELLMI, self()},
      log("Send tell_mi to ~s~n", [Name]),
      receive
        {?TELLMI_RES, Mi} ->
          log("Received Mi ~p from ~s~n", [Mi, Name]);
        Unknown ->
          log("Received unknown response for tell_mi from ~s : ~s~n", [Name, Unknown])
      end
  end.

get_last([]) ->
 log("Last of emty array is not good");
get_last([Head]) ->
  Head;
get_last([_Head|Tail]) ->
  get_last(Tail).
