-module(nodefinder_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    { ok, Addr } = application:get_env (nodefinder, addr),
    { ok, Port } = application:get_env (nodefinder, port),
    { ok, Ttl } = application:get_env (nodefinder, multicast_ttl),
    nodefindersup:start_link (Addr, Port, Ttl).


stop(_State) ->
    application:stop (nodefinder).
