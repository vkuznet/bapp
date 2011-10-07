%% -*- mode: erlang -*-
-module(sync_worker).
-author('vkuznet@gmail.com').
-export([execute/2,black_box/3]).

%% ------------------------------------------------------------------
%% Concurrently execute black_box function for a given set of files
%% ------------------------------------------------------------------

execute(Cmd, Files) ->
    Nodes = [node()|nodes()],
    io:format("Available nodes: ~p~n", [Nodes]),
    Parent = self(),
    Pids = [worker(Parent, Nodes, Cmd, File) || File <- Files],
    Results = wait_all(Pids),
    Results.

%% ------------------------------------------------------------------
%% Black box function, which invoke run command over the given file
%% It passes the result via standard message to a given caller.
%% ------------------------------------------------------------------

black_box(From, Cmd, File) ->
    Result = cmd:run(Cmd ++ " " ++ File),
    From ! {self(), Result}.

%% ------------------------------------------------------------------
%% Worker code, it spawn black box function either on a local node
%% or on a participating node (which it picks up randomly, more logic can
%% be done to check status of the job on a node and send a job to
%% least busiest node). Worker returns PID of the spawed process.
%% ------------------------------------------------------------------

worker(Parent, Nodes, Cmd, File) ->
    if  length(Nodes) == 1 ->
        Pid = spawn(fun() -> black_box(Parent, Cmd, File) end);
        true ->
            Node = lists:nth(random:uniform(length(Nodes)), Nodes),
            Pid = spawn(Node, ?MODULE, black_box, [Parent, Cmd, File])
    end,
    Pid.

%% ------------------------------------------------------------------
%% collect results for a given Pid list
%% ------------------------------------------------------------------

wait_all(Pids) ->
    [receive {Pid, Result} -> Result end || Pid <- Pids].

