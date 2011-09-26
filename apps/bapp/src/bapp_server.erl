%% -*- mode: erlang -*-
-module(bapp_server).
-behaviour(gen_server).
-author('vkuznet@gmail.com').
-include_lib("kernel/include/file.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    inets:start(),
    Table = pid_manager:start(),
    io:format("Status of ~p: ~p~n", [Table, pid_manager:info()]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

%% ------------------------------------------------------------------
%% handle_call for various server messages:
%% {status, node, Node} to obtain status of the given Node
%% {status, guid, Guid} to obtain status of jobs for a given Guid
%% {status, pid, {Node, Pid}} to obtain status of given Pid on a Node
%% {process, Cmd, Dir} to run given command with a given directory
%% ------------------------------------------------------------------
handle_call({status, node, Node}, _From, _State) ->
%    _R1 = pid_manager:info_node(Node), % info_node will clean-up undefined
    Result = pid_manager:info_node(Node),
    {reply, ok, Result};
handle_call({status, guid, Guid}, _From, _State) ->
    Result = pid_manager:info_guid(Guid),
    {reply, ok, Result};
handle_call({status, pid, {Node, PidStr}}, _From, _State) ->
    Result = pid_manager:pinfo(Node, PidStr),
    {reply, ok, Result};
handle_call({process, Cmd, Dir}, _From, _State) ->
    Files = filelib:wildcard(Dir ++ "/*.in"),
%    Result = sync_worker:execute(Cmd, Files),
    Result = worker:execute(Cmd, Files),
    {reply, ok, Result};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

