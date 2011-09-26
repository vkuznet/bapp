%% -*- mode: nitrogen -*-
-module(pid_manager).
-author('vkuznet@gmail.com').
-export([start/0, info/0, add/3, info_node/1, info_pid/1, info_guid/1, delete/2]).
-export([process_info_wrapper/2]).
-define(TABLE, pid_table).

%% ------------------------------------------------------------------
%% initialization function
%% ------------------------------------------------------------------
start() ->
    ets:new(?TABLE, [bag, named_table, public]).

info() ->
    ets:info(?TABLE).

%% ------------------------------------------------------------------
%% Process info wrapper call to given Node, Pid.
%% Please note: in order to make rpc:call I wrap pid to list and
%% unwrap it back (plain pass of Pid does not work).
%% ------------------------------------------------------------------
process_info_wrapper(Node, Pid) ->
    ProcInfo = rpc:call(Node, erlang, process_info, [list_to_pid(pid_to_list(Pid))]),
    case ProcInfo of
        undefined ->
            undefined;
        _other ->
            [{K,V} || {K,V} <- ProcInfo, K==status]
    end.

%% ------------------------------------------------------------------
%% add entry into table for any active Pid and a given node.
%% ------------------------------------------------------------------
add(GUID, Node, Pid) ->
%    ProcInfo = rpc:call(Node, erlang, process_info, [list_to_pid(pid_to_list(Pid))]),
%    case ProcInfo of
    case process_info_wrapper(Node, Pid) of
        undefined ->
            skip;
        _other ->
            ets:insert(?TABLE, {GUID, Node, Pid})
    end.

%% ------------------------------------------------------------------
%% delete Pid from the table based on its status
%% ------------------------------------------------------------------
delete(Node, Pid) ->
%    ProcInfo = rpc:call(Node, erlang, process_info, [list_to_pid(pid_to_list(Pid))]),
%    case ProcInfo of
    case process_info_wrapper(Node, Pid) of
        undefined ->
            ets:match_delete(?TABLE, {'_', '_', Pid});
        _other ->
            skip
    end.

%% ------------------------------------------------------------------
%% provides information about given node and at the same time
%% removed finished processes from global table
%% ------------------------------------------------------------------
info_node(Node) ->
    Tup = ets:match_object(pid_table, {'_', Node, '_'}),
    [{G,N,P,process_info_wrapper(N,P)} || {G,N,P} <- Tup, delete(N,P) /= true].

%% ------------------------------------------------------------------
%% provides information about given pid
%% ------------------------------------------------------------------
info_pid(Pid) ->
    Tup = ets:match_object(pid_table, {'_', '_', Pid}),
    [{G,N,P,process_info_wrapper(N,P)} || {G,N,P} <- Tup].

%% ------------------------------------------------------------------
%% provides information about given guid
%% ------------------------------------------------------------------
info_guid(Guid) ->
    Tup = ets:lookup(?TABLE, Guid),
    [{G,N,P,process_info_wrapper(N,P)} || {G,N,P} <- Tup].
