-module(nameservice).
-author("Birgit Wendholt").

%% API
-export([start/0]).

-import(global, [register_name/2]).
-import(lists, [keymember/3, keyreplace/4, keyfind/3, keydelete/3]).
-import(tools, [log/3]).
-import(werkzeug, [to_String/1, timeMilliSecond/0, get_config_value/2,list2String/1]).
-include("messages.hrl").
-include("constants.hrl").

start() ->
  {ok, Config} = file:consult("nameservice.cfg"),
  {ok, Name} = get_config_value(name, Config),
  register_name(Name, self()),
  ns_loop(Name, []).


% nameservice loop
% bounded services are maintained in the Services list [{servicename,servicenode},....]
ns_loop(Name, Services) ->
  receive
    {Pid, {?REBIND, Service, Node}} ->
      ns_loop(Name, rebind(Name, Pid, Service, Node, Services));
    {Pid, {?LOOKUP, Service}} ->
      ns_loop(Name, lookup(Name, Pid, Service, Services));
    {Pid, {?UNBIND, Service}} ->
      ns_loop(Name, unbind(Name, Pid, Service, Services));
    ?SHOWCONTENT -> ns_loop(Name,show_content(Name,Services))
  end.


rebind(Name, Pid, Service, Node, Services) ->
  log_ns(Name, "rebinding ~p at node ~s", [Service, to_String(Node)]),
  Pid ! {?REBIND_RES, ok},
  ContainsKey = keymember(Service, 1, Services),
  if ContainsKey ->
    keyreplace(Service, 1, Services, {Service, Node});
    true ->
      [{Service, Node} | Services]
  end.

lookup(Name, Pid, Service, Services) ->
  ServiceAtNode = keyfind(Service, 1, Services),
  if (ServiceAtNode == false) ->
    log_ns(Name, "lookup service ~p::remote ref not found", [Service]),
    Pid ! {?LOOKUP_RES, ?UNDEFINED};
    true ->
      log_ns(Name, "lookup service ~p::remote ref is ~s", [Service, to_String(ServiceAtNode)]),
      Pid ! {?LOOKUP_RES, ServiceAtNode}
  end,
  Services.

unbind(Name, Pid, Service, Services) ->
  ContainsKey = keymember(Service, 1, Services),
  if ContainsKey ->
    ServiceAtNode = keyfind(Service, 1, Services),
    log_ns(Name, "unbinding service ~p:: successful", [Service]),
    NewServices = keydelete(Service, 1, Services),
    Pid ! {?UNBIND_RES, ServiceAtNode},
    NewServices;
    true ->
      log_ns(Name, "unbinding service ~p:: not found in table", [Service]),
      Pid ! ?NOK,
      Services
  end.

show_content(Name,Services) ->
  log_ns(Name,"all registered services::~s",[list2String(Services)]),
  Services.


% consistent logs for nameservice
log_ns(Name, MsgFormat, MsgArgs) ->
  Header = "NS <~p>::",
  Trailer = ":(~s)~n",
  TotalMsgFormat = Header ++ MsgFormat ++ Trailer,
  log(Name, TotalMsgFormat, [Name | MsgArgs] ++ [timeMilliSecond()]).



