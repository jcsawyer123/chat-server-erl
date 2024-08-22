-module(user_manager).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([login/2, logout/1, broadcast/2, get_user/1, get_username/1, send_message/2]).

-record(state, {users = #{}}).

% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Username, Pid) ->
    gen_server:call(?MODULE, {login, Username, Pid}).

logout(Pid) ->
    gen_server:cast(?MODULE, {logout, Pid}).

broadcast(From, Content) ->
    % TODO: Does not broadcast currently
    gen_server:cast(?MODULE, {broadcast, From, Content}).

get_user(Username) ->
    gen_server:call(?MODULE, {get_user, Username}).

get_username(Pid) ->
    gen_server:call(?MODULE, {get_username, Pid}).

send_message(Pid, Message) ->
    gen_server:cast(?MODULE, {send_message, Pid, Message}).

% Callback functions
init([]) ->
    {ok, #state{}}.

handle_call({login, Username, Pid}, _From, State) ->
    handle_login(Username, Pid, State);
handle_call({get_username, Pid}, _From, State) ->
    handle_get_username(Pid, State);
handle_call({get_user, Username}, _From, State) ->
    handle_get_user(Username, State);
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({logout, Pid}, State) ->
    handle_logout(Pid, State);
handle_cast({broadcast, From, Content}, State) ->
    handle_broadcast(From, Content, State);
handle_cast({send_message, Pid, Message}, State) ->
    handle_send_message(Pid, Message, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
handle_login(Username, Pid, State) ->
    case maps:is_key(Username, State#state.users) of
        true ->
            {reply, {error, username_taken}, State};
        false ->
            {reply, ok, State#state{users = maps:put(Username, Pid, State#state.users)}}
    end.

handle_get_username(Pid, State) ->
    Username = maps:fold(fun(K, V, Acc) -> if V =:= Pid -> K; true -> Acc end end, undefined, State#state.users),
    {reply, Username, State}.

handle_get_user(Username, State) ->
    {reply, maps:find(Username, State#state.users), State}.

handle_logout(Pid, State) ->
    Username = maps:fold(fun(K, V, Acc) -> if V =:= Pid -> K; true -> Acc end end, undefined, State#state.users),
    case Username of
        undefined ->
            {noreply, State};
        _ ->
            room_manager:user_logout(Pid),
            {noreply, State#state{users = maps:remove(Username, State#state.users)}}
    end.

handle_broadcast(From, Content, State) ->
    FromUsername = maps:fold(fun(K, V, Acc) -> if V =:= From -> K; true -> Acc end end, undefined, State#state.users),
    Message = #{type => <<"broadcast">>, from => FromUsername, content => Content},
    [Pid ! {send, Message} || Pid <- maps:values(State#state.users), Pid =/= From],
    {noreply, State}.

handle_send_message(Pid, Message, State) ->
    Pid ! {send, Message},
    {noreply, State}.