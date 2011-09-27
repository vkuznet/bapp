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
%% Helper function to extract Pid ids for given position.
%% The Pid has structure the following strucuture: <A.B.C>, where
%% A represents node number, B and C are process numbers
%% (first 15 bits and 16-18 bits, respectively), for more info see
%% http://stackoverflow.com/questions/243363/can-someone-explain-the-structure-of-a-pid-in-erlang
%% ------------------------------------------------------------------
pid_pos(Pos, Pid) ->
    lists:nth(Pos, [binary_to_list(S) || S <- re:split( pid_to_list(Pid), "\\." )] ).

%% ------------------------------------------------------------------
%% Process info wrapper obtain Pid info on remote Node.
%% We match on a second/third ids of Pid using remote processes call,
%% which return Pid table on remote node.
%% ------------------------------------------------------------------
pinfo(Node, PidStr) ->
    List = [binary_to_list(S) || S <- re:split(PidStr, "\\.")],
    Processes = rpc:call(Node, erlang, processes, []),
    MatchPid = [P || P <- Processes, pid_pos(2, P) == lists:nth(2, List),
                                     pid_pos(3, P) == lists:nth(3, List) ],
    if  length(MatchPid) == 1 ->
        ProcInfo = rpc:call(Node, erlang, process_info, [hd(MatchPid)]),
        case ProcInfo of
            undefined ->
                undefined;
            _other ->
                [{K,V} || {K,V} <- ProcInfo, K==status]
        end;
        true -> undefined
    end.

%% ------------------------------------------------------------------
%% add entry into table for any active Pid and a given node.
%% ------------------------------------------------------------------
add(GUID, Node, Pid) ->
    case pinfo(Node, pid_to_list(Pid)) of
        undefined ->
            skip;
        _other ->
            ets:insert(?TABLE, {GUID, Node, Pid})
    end.

%% ------------------------------------------------------------------
%% delete Pid from the table based on its status
%% ------------------------------------------------------------------
delete(Node, Pid) ->
    case pinfo(Node, pid_to_list(Pid)) of
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
    [{G,N,P,pinfo(N,pid_to_list(P))} || {G,N,P} <- Tup, delete(N,P) /= true].

%% ------------------------------------------------------------------
%% provides information about given pid
%% ------------------------------------------------------------------
info_pid(Pid) ->
    Tup = ets:match_object(pid_table, {'_', '_', Pid}),
    [{G,N,P,pinfo(N,pid_to_list(P))} || {G,N,P} <- Tup].

%% ------------------------------------------------------------------
%% provides information about given guid
%% ------------------------------------------------------------------
info_guid(Guid) ->
    Tup = ets:lookup(?TABLE, Guid),
    [{G,N,P,pinfo(N,pid_to_list(P))} || {G,N,P} <- Tup].
