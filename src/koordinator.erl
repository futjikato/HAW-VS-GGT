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
  spawn(fun() -> workloop(register, [], 0) end).

workloop(register, GgtNameList, ToggleFlag) ->
  log("Registertime ~pms~n", [get_registertime()]),
  timer:send_after(get_registertime(), self(), rt_over),
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
    Unknown ->
      log("Received unknown message: ~p~n", [Unknown]),
      workloop(register, GgtNameList, ToggleFlag)
  end.
workloop(ready, GgtNameList, WggT, ToggleFlag, CurrentMi) ->
  StartValues = werkzeug:bestimme_mis(WggT, length(GgtNameList)),
  send(GgtNameList, StartValues, 0, max(2, round(length(GgtNameList) / 100 * 15))),
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
    Unknown ->
      log("Received unknown message: ~p~n", [Unknown]),
      workloop(register, GgtNameList, ToggleFlag)
  end.

send([], _StartValues, _Counter, _SendAmount) ->
  log("Send value to all ggt processes"); %% only possible if <= 2 ggt processes where registered
send(_GgtNameList, [], _Counter, _SendAmount) ->
  log("[SEVERE] - Send all starter values"); %% should never ever occure
send([Name|NameTail], [Value|ValueTail], Counter, SendAmount) when SendAmount < Counter ->
  spawn(fun() -> send_pmi(Name, Value) end),
  send(NameTail, ValueTail, Counter + 1, SendAmount);
send([Name|Tail], [Value], Counter, SendAmount) when SendAmount < Counter ->
  spawn(fun() -> send_pmi(Name, Value) end),
  send(Tail, [], Counter + 1, SendAmount);
send([Name], [Value|Tail], Counter, SendAmount) when SendAmount < Counter ->
  spawn(fun() -> send_pmi(Name, Value) end),
  send([], Tail, Counter + 1, SendAmount);
send([Name], [Value], Counter, SendAmount) when SendAmount < Counter ->
  spawn(fun() -> send_pmi(Name, Value) end),
  send([], [], Counter + 1, SendAmount);
send(_GgtNameList, _StartValues, _Counter, SendAmount)  ->
  log("Send ~p values to ggt processes. Done sending.~n", [SendAmount]),
  noop.

send_pmi(Name, Value) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send pmi to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      net_adm:ping(ServiceAtNode),
      GgtPID = global:whereis_name(Name),
      GgtPID ! {set_pmi, Value},
      log("Send value ~p to ~s~n", [Value, Name])
  end.


received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, _ToggleFlag) when GgTMi < CurrentMi ->
  log("Received new mi ~p from ~s at ~s~n", [GgTMi, GgtName, GgTZeit]),
  GgTMi;
received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, _ToggleFlag) when GgTMi == CurrentMi ->
  log("Received same mi ~p from ~s at ~s~n", [GgTMi, GgtName, GgTZeit]),
  CurrentMi;
received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, ToggleFlag) when ToggleFlag == 1 , GgTMi > CurrentMi ->
  log("Received to hight mi ~p from ~s at ~s with toggle flag set to notification.", [GgTMi, GgtName, GgTZeit]),
  spawn(fun() -> send_value(GgtName, CurrentMi) end),
  CurrentMi;
received_brief_term(GgtName, GgTMi, CurrentMi, GgTZeit, _FromPID, _ToggleFlag) when GgTMi > CurrentMi ->
  log("Received to hight mi ~p from ~s at ~s with toggle flag set to ignore.", [GgTMi, GgtName, GgTZeit]),
  CurrentMi.

send_value(Name, Value) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send new mi value to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      net_adm:ping(ServiceAtNode),
      GgtPID = global:whereis_name(Name),
      GgtPID ! {send, Value},
      log("Send value ~p to ~s~n", [Value, Name])
  end.


%%----------------------------------------------------------------------
%% Function: kill_ggts/1
%% Purpose: Send a kill command to all known ggt processes
%%----------------------------------------------------------------------
kill_ggts([]) ->
  noop;
kill_ggts([Name]) ->
  spawn(fun() -> send_kill(Name) end);
kill_ggts([Name|Tail]) ->
  spawn(fun() -> send_kill(Name) end),
  kill_ggts(Tail).

send_kill(Name) ->
  Nameserver = get_nameserver(),
  Nameserver ! {self(), {?LOOKUP, Name}},
  receive
    {?LOOKUP_RES, ?UNDEFINED} ->
      log("Unable to send kill command to client. Nameserver does not know ~p~n", [Name]);
    {?LOOKUP_RES, ServiceAtNode} ->
      net_adm:ping(ServiceAtNode),
      GgtPID = global:whereis_name(Name),
      GgtPID ! {kill},
      log("Send kill to ~s~n", [Name])
  end.

%%----------------------------------------------------------------------
%% Function: bind_at_nameserver/0
%% Purpose: Register the coordinator at the nameserver
%% Returns: None
%%----------------------------------------------------------------------
bind_at_nameserver() ->
  global:register_name(koordinator, self()),
  register(koordinator, self()),
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
%% Function: switch_toggle/1
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
      log("Unable to send kill command to client. Nameserver does not know ~p~n", [Receiver]);
    {?LOOKUP_RES, ServiceAtNode} ->
      net_adm:ping(ServiceAtNode),
      GgtPID = global:whereis_name(Receiver),
      GgtPID ! {set_neighbours, LeftN, RighN},
      log("Send neighbours (~p / ~p) to ~s~n", [LeftN, RighN, Receiver])
  end.

get_last([]) ->
 log("Last of emty array is not good");
get_last([Head]) ->
  Head;
get_last([_Head|Tail]) ->
  get_last(Tail).