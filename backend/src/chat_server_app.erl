%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:set_env(sasl, sasl_error_logger, false),
    application:set_env(kernel, error_logger, {file, "log/error.log"}),
    error_logger:tty(true),
    error_logger:logfile({open, "log/error.log"}),
    
    {ok, _} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", chat_websocket_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    chat_server_sup:start_link().

stop(_State) ->
    ok.