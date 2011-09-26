#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname testnode@vklaptop -setcookie cookiestring
-module(client).
-compile(export_all).
-define(NODE, mynode@vklaptop).
-define(FROM, testnode@vklaptop).
-define(STATUS, 1).

%% ------------------------------------------------------------------
%% print helper function
%% ------------------------------------------------------------------
print(Msg) ->
   io:format("~p~n", [Msg]).

%% ------------------------------------------------------------------
%% Test function, which submit RPC message to Erlang server
%% server function is bapp_server:handle_call which accepts the
%% following messages:
%%    {status, node, Node} obtain status of the given Node
%%    {status, guid, Guid} obtain status of jobs for a given Guid
%%    {status, pid, {Node, Pid}} obtain status for given Node, Pid
%%    {process, Cmd, Dir} run given command with a given directory
%% ------------------------------------------------------------------

test(Node, Arg) ->
    case Arg of
        {status, node, N} ->
            Res = rpc:call(Node, bapp_server, handle_call, [{status, node, N}, ?FROM, ?STATUS]);
        {status, pid, {N, Pid}} ->
            Res = rpc:call(Node, bapp_server, handle_call, [{status, pid, {N, Pid}}, ?FROM, ?STATUS]);
        {status, guid, Guid} ->
            Res = rpc:call(Node, bapp_server, handle_call, [{status, guid, Guid}, ?FROM, ?STATUS]);
        {process, Cmd, Dir} ->
            Res = rpc:call(Node, bapp_server, handle_call, [{process, Cmd, Dir}, ?FROM, ?STATUS])
    end,
    print(Res).


%% ------------------------------------------------------------------
%% Handle usage message
%% ------------------------------------------------------------------
usage() ->
    io:format("Usage: erl_client.erl <action list>\n"),
    io:format("Actions:\n"),
    io:format("   status node Node\n"),
    io:format("   status guid Guid\n"),
    io:format("   status pid Node Pid\n"),
    io:format("   process cmd dir\n"),
    halt(1).

%% ------------------------------------------------------------------
%% Main processing function
%% ------------------------------------------------------------------
main([]) ->
    usage();
main(Args) ->
    Head = list_to_atom(hd(Args)),
    case Head of
        status ->
            Sec  = list_to_atom(lists:nth(2, Args)),
            case Sec of
                node ->
                    Tup = {status, node, tl(Args)};
                pid ->
                    Node= list_to_atom(lists:nth(3, Args)),
                    Pid = lists:nth(4, Args),
                    Tup = {status, pid, {Node, Pid}};
                guid  ->
                    Tup = {status, guid, tl(Args)}
            end;
        process ->
            Tup = {process, lists:nth(2, Args), tl(Args)}
    end,
    test(?NODE, Tup).
