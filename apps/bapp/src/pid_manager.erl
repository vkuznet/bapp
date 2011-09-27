%% -*- mode: erlang -*-
-module(pid_manager).
-author('vkuznet@gmail.com').
-export([start/0, info/0, add/3, info_node/1, info_pid/1, info_guid/1, delete/2]).
-export([pinfo/2]).
-define(TABLE, pid_table).

%% ------------------------------------------------------------------
%% initialization function
%% ------------------------------------------------------------------
start() ->
    ets:new(?TABLE, [bag, named_table, public]).

info() ->
    ets:info(?TABLE).

%% ------------------------------------------------------------------
%% Process info wrapper obtain Pid info on remote Node.
%% ------------------------------------------------------------------
pinfo(Node, PidStr) ->
    ProcInfo = rpc:call(Node, erlang, process_info, [list_to_pid(PidStr)]),
    case ProcInfo of
        undefined ->
            undefined;
        _other ->
            [{K,V} || {K,V} <- ProcInfo, K==status]
    end.

%% ------------------------------------------------------------------
%% add entry into table for any active Pid and a given node.
%% ------------------------------------------------------------------
add(GUID, Node, PidStr) ->
    case pinfo(Node, PidStr) of
        undefined ->
            skip;
        _other ->
            ets:insert(?TABLE, {GUID, Node, PidStr})
    end.

%% ------------------------------------------------------------------
%% delete Pid from the table based on its status
%% ------------------------------------------------------------------
delete(Node, PidStr) ->
    case pinfo(Node, PidStr) of
        undefined ->
            ets:match_delete(?TABLE, {'_', Node, PidStr});
        _other ->
            skip
    end.

%% ------------------------------------------------------------------
%% provides information about given node and at the same time
%% removed finished processes from global table
%% ------------------------------------------------------------------
info_node(Node) ->
    Tup = ets:match_object(pid_table, {'_', Node, '_'}),
    [{G,N,P,pinfo(N,P)} || {G,N,P} <- Tup, delete(N,P) /= true].

%% ------------------------------------------------------------------
%% provides information about given pid
%% ------------------------------------------------------------------
info_pid(PidStr) ->
    Tup = ets:match_object(pid_table, {'_', '_', PidStr}),
    [{G,N,P,pinfo(N,P)} || {G,N,P} <- Tup].

%% ------------------------------------------------------------------
%% provides information about given guid
%% ------------------------------------------------------------------
info_guid(Guid) ->
    Tup = ets:lookup(?TABLE, Guid),
    [{G,N,P,pinfo(N,P)} || {G,N,P} <- Tup, delete(N,P) /= true].
