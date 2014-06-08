%%%-------------------------------------------------------------------
%%% @author Birgit Wendholt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mai 2014 06:14
%%%-------------------------------------------------------------------
-module(tools).
-author("Birgit Wendholt").

%% API
-export([log/3]).
-import(werkzeug, [logstop/0, logging/2, to_String/1]).
-include("messages.hrl").
-include("constants.hrl").


% Name - atom
% Format - format string
% Args - List of Args

log(Name, Format, Args) ->
  Filename = "logs/" ++ to_String(Name) ++ ?NAMEDELIM ++ to_String(node()) ++ ".log",
  logging(Filename, io_lib:format(Format, Args)).


