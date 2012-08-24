-module(emud).

-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(os_mon),
    application:start(emud).

stop() ->
    application:stop(emud).
