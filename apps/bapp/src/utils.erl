%% -*- mode: erlang -*-
-module(utils).
-author('vkuznet@gmail.com').
-compile(export_all).

%% ------------------------------------------------------------------
%% Print given message to stdout
%% ------------------------------------------------------------------
print(Msg) ->
    io:format("~p~n", [Msg]).
