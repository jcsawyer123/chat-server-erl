%%%-------------------------------------------------------------------
%% @doc chat_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 1},
    ChildSpecs = [
        #{id => room_manager,
            start => {room_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [room_manager]},
        #{id => user_manager,
            start => {user_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [user_manager]}
    ],
    {ok, {SupFlags, ChildSpecs}}.