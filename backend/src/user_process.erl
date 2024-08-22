-module(user_process).
-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_message/2, join_room/2, leave_room/2, get_rooms/1, get_username/1, is_in_room/2]).

-record(state, {
    ws_pid,
    username,
    rooms = sets:new()
}).

% API functions
start_link(WsPid, Username) ->
    gen_server:start_link(?MODULE, {WsPid, Username}, []).

send_message(Pid, Message) ->
    gen_server:cast(Pid, {send_message, Message}).

join_room(Pid, Room) when is_list(Room) ->
    gen_server:call(Pid, {join_room, Room});
join_room(Pid, Room) when is_binary(Room) ->
    gen_server:call(Pid, {join_room, binary_to_list(Room)}).

leave_room(Pid, Room) when is_list(Room) ->
    gen_server:call(Pid, {leave_room, Room});
leave_room(Pid, Room) when is_binary(Room) ->
    gen_server:call(Pid, {leave_room, binary_to_list(Room)}).

get_rooms(Pid) ->
    gen_server:call(Pid, get_rooms).

get_username(Pid) ->
    gen_server:call(Pid, get_username).

is_in_room(Pid, Room) when is_list(Room) ->
    gen_server:call(Pid, {is_in_room, Room});
is_in_room(Pid, Room) when is_binary(Room) ->
    gen_server:call(Pid, {is_in_room, binary_to_list(Room)}).

% Callback functions
init({WsPid, Username}) ->
    case global:register_name({username, Username}, self()) of
        yes ->
            {ok, #state{ws_pid = WsPid, username = Username}};
        no ->
            {stop, username_taken}
    end.

handle_call({join_room, Room}, _From, State) ->
    handle_join_room(Room, State);
handle_call({leave_room, Room}, _From, State) ->
    handle_leave_room(Room, State);
handle_call(get_rooms, _From, State) ->
    {reply, sets:to_list(State#state.rooms), State};
handle_call(get_username, _From, State) ->
    {reply, State#state.username, State};
handle_call({is_in_room, Room}, _From, State) ->
    {reply, sets:is_element(Room, State#state.rooms), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send_message, Message}, State) ->
    handle_send_message(Message, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{username = Username}) ->
    global:unregister_name({username, Username}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
handle_join_room(Room, State) ->
    case sets:is_element(Room, State#state.rooms) of
        true ->
            {reply, {error, already_in_room}, State};
        false ->
            NewRooms = sets:add_element(Room, State#state.rooms),
            {reply, ok, State#state{rooms = NewRooms}}
    end.

handle_leave_room(Room, State) ->
    case sets:is_element(Room, State#state.rooms) of
        true ->
            NewRooms = sets:del_element(Room, State#state.rooms),
            {reply, ok, State#state{rooms = NewRooms}};
        false ->
            {reply, {error, not_in_room}, State}
    end.

handle_send_message(#{type := <<"ping">>} = Message, State) ->
    State#state.ws_pid ! {send, Message},
    {noreply, State};
handle_send_message(Message, State) ->
    State#state.ws_pid ! {send, Message},
    {noreply, State}.