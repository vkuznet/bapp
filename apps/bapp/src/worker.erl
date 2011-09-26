%% -*- mode: nitrogen -*-
-module(worker).
-author('vkuznet@gmail.com').
-export([execute/2, get_slot/1, loop/3]).
-define(SLEEP_INTERVAL, 5000).
-define(NODE_THRESHOLD, 2).

%% ------------------------------------------------------------------
%% Establish connection with given set of nodes
%% ------------------------------------------------------------------

connect([N|Tail]) ->
    case net_kernel:connect(N) of
        true ->
%            io:format("node ~p is available~n", [N]);
            ok;
        false ->
%            io:format("node ~p is unavailable~n", [N])
            skip
    end,
    connect(Tail);
connect([]) ->
    ok.

%% ------------------------------------------------------------------
%% Concurrently execute black_box function for a given set of files 
%% and given command. The logic is following. We start wait_loop to
%% collect possible results and pass its Pid into main loop which
%% executes black_box function. The set of results are set externally
%% ------------------------------------------------------------------
execute(Cmd, Files) ->
    GUID = 1, % global unique id for request
    Pid = spawn(?MODULE, loop, [GUID, Cmd, Files]),
    {GUID, Pid}.

%% ------------------------------------------------------------------
%% Provides first available slot on participating nodes
%% ------------------------------------------------------------------
get_slot([Node|Nodes]) ->
    io:format("get_slot ~p~n", [[Node|Nodes]]),
    Status = pid_manager:info_node(Node),
    io:format("Status ~p~n", [Status]),
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
    connect(['mynode@lnxcu9', 'mynode@lnx301', 'mynode@lnx7228']),
    Nodes = [node()|nodes()],
    case get_slot(Nodes) of
         none ->
            timer:sleep(?SLEEP_INTERVAL),
            loop(GUID, Cmd, [File|Files]);
         {available, Node} ->
%            DevNull = "2>&1 1>& /dev/null",
            DevNull = " ",
            % NOTE: this is not atomic operation, I get Pid and then
            % add it to pid_manager, but during that time, process can
            % be done/fail/etc., such that pid_manager will not be able to
            % add it.
            io:format("Execute ~p ~p~n", [Node, Cmd++" "++File++" "++DevNull]),
            Pid = spawn(Node, os, cmd, [Cmd++" "++File++" "++DevNull]),
            case pid_manager:add(GUID, Node, Pid) of
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
