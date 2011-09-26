%% -*- mode: nitrogen -*-
-module(cmd).
-author('vkuznet@gmail.com').
-export([run/1, run/2, test/0]).
%
% Code credit:
% http://piotrga.wordpress.com/2010/04/02/how-to-run-a-system-command-in-erlang/

%% ------------------------------------------------------------------
%% Default API to run given command, default timeout is 5s
%% ------------------------------------------------------------------
run(Cmd) ->
    run(Cmd, 5000).

%% ------------------------------------------------------------------
%% Main API to run given command and timeout asynchronously
%% ------------------------------------------------------------------
run(Cmd, Timeout) ->
    io:format("Calling ~p ~p, timeout=~p~n", [node(), Cmd, Timeout]),
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port, [], Timeout).

%% ------------------------------------------------------------------
%% Receive loop, listen on given port and collect the data
%% ------------------------------------------------------------------
loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, [NewData|Data], Timeout);
        {Port, {exit_status, 0}} -> lists:reverse(Data);
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
        throw(timeout)
    end.

%% ------------------------------------------------------------------
%% Test functions
%% ------------------------------------------------------------------
test() ->
    ls_command(),
    shouldReturnCommandResult(),
    shouldThrowAfterTimeout(),
    shouldThrowIfCmdFailed(),
    {ok, "Tests PASSED"}.

ls_command() ->
    Output = run("ls"),
    io:format("output of ls: ~p~n", [Output]).

shouldReturnCommandResult() ->
    Output = run("echo Hello"),
    io:format("output of echo: ~p~n", [Output]),
    ["Hello\n"] = Output.

shouldThrowAfterTimeout()->
    timeout = (catch run("sleep 10", 20)).

shouldThrowIfCmdFailed()->
    {commandfailed, _} = (catch run("wrongcommand")),
    {commandfailed, _} = (catch run("ls nonexistingfile")).

