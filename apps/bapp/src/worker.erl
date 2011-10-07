%% -*- mode: erlang -*-
-module(worker).
-author('vkuznet@gmail.com').
-export([execute/2, get_slot/1, loop/3, get_unix_timestamp/1]).
-define(SLEEP_INTERVAL, 5000).
-define(NODE_THRESHOLD, 2).

%% ------------------------------------------------------------------
%% Establish connection with given set of nodes
%% ------------------------------------------------------------------

%connect([N|Tail]) ->
%    case net_kernel:connect(N) of
%        true ->
%            ok;
%        false ->
%            skip
%    end,
%    connect(Tail);
%connect([]) ->
%    ok.

%% ------------------------------------------------------------------
%% Get UNIX timestamp
%% Courtesy of
%% http://erlangdevelopers.splinder.com/post/16144262/stdlib-unix-timestamp-in-erlang
%% ------------------------------------------------------------------
get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

%% ------------------------------------------------------------------
%% Concurrently execute black_box function for a given set of files 
%% and given command. The logic is following. We start wait_loop to
%% collect possible results and pass its Pid into main loop which
%% executes black_box function. The set of results are set externally.
%% We use get_unix_timestamp as a GUID.
%% ------------------------------------------------------------------
execute(Cmd, Files) ->
    GUID = get_unix_timestamp(now()),
    Pid = spawn(?MODULE, loop, [GUID, Cmd, Files]),
    {GUID, Pid}.

%% ------------------------------------------------------------------
%% Provides first available slot on participating nodes
%% ------------------------------------------------------------------
get_slot([Node|Nodes]) ->
%    io:format("get_slot ~p~n", [[Node|Nodes]]),
    Status = pid_manager:info_node(Node),
%    io:format("Status ~p~n", [Status]),
    if  length(Status) < ?NODE_THRESHOLD ->
        {available, Node};
        true ->
           get_slot(Nodes)
    end;
get_slot([]) ->
    none.

%% ------------------------------------------------------------------
%% Main loop which process given set of files with given command.
%% Files are extracted one by one and put into black_box for
%% execution.
%% ------------------------------------------------------------------
loop(GUID, Cmd, [File|Files]) ->
%    connect(['mynode@lnxcu9', 'mynode@lnx301', 'mynode@lnx7228']),
    Nodes = [node()|nodes()],
    case get_slot(Nodes) of
         none ->
            timer:sleep(?SLEEP_INTERVAL),
            loop(GUID, Cmd, [File|Files]);
         {available, Node} ->
            % NOTE: this is not atomic operation, I get Pid and then
            % add it to pid_manager, but during that time, process can
            % be done/fail/etc., such that pid_manager will not be able to
            % add it.
            io:format("~nNew job ~p ~p~n", [Node, Cmd++" "++File]),
            Pid = spawn(Node, os, cmd, [Cmd++" "++File]),
            case pid_manager:add(GUID, Node, pid_to_list(Pid)) of
                skip ->
                    loop(GUID, Cmd, [File|Files]);
                true ->
                    loop(GUID, Cmd, lists:delete(File, Files));
                _other ->
                    throw({error, "Unable to add Pid"})
            end
    end;
loop(_Id, _Cmd, []) ->
    {ok, done}.
