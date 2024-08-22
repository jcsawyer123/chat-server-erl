-module(room_manager).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([join/2, leave/2, send_message/3, get_all_rooms/0, get_users_in_room/1, get_server_info/0, user_logout/1]).

-record(state, {rooms = #{}}).

% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(UserPid, Room) when is_list(Room) ->
    gen_server:call(?MODULE, {join, UserPid, Room});
join(UserPid, Room) when is_binary(Room) ->
    gen_server:call(?MODULE, {join, UserPid, binary_to_list(Room)}).

leave(UserPid, Room) when is_list(Room) ->
    gen_server:call(?MODULE, {leave, UserPid, Room});
leave(UserPid, Room) when is_binary(Room) ->
    gen_server:call(?MODULE, {leave, UserPid, binary_to_list(Room)}).

user_logout(UserPid) ->
    gen_server:cast(?MODULE, {user_logout, UserPid}).
    
send_message(From, Room, Content) when is_list(Room) ->
    gen_server:call(?MODULE, {send_message, From, Room, Content});
send_message(From, Room, Content) when is_binary(Room) ->
    gen_server:call(?MODULE, {send_message, From, binary_to_list(Room), Content}).

get_all_rooms() ->
    gen_server:call(?MODULE, get_all_rooms).

get_users_in_room(Room) when is_list(Room) ->
    gen_server:call(?MODULE, {get_users_in_room, Room});
get_users_in_room(Room) when is_binary(Room) ->
    gen_server:call(?MODULE, {get_users_in_room, binary_to_list(Room)}).

get_server_info() ->
    gen_server:call(?MODULE, get_server_info).

% Callback functions
init([]) ->
    {ok, #state{}}.

handle_call({join, UserPid, Room}, _From, State) ->
    handle_join(UserPid, Room, State);
handle_call({leave, UserPid, Room}, _From, State) ->
    handle_leave(UserPid, Room, State);
handle_call({send_message, From, Room, Content}, _From, State) ->
    handle_send_message(From, Room, Content, State);
handle_call(get_all_rooms, _From, State) ->
    {reply, [list_to_binary(Room) || Room <- maps:keys(State#state.rooms)], State};
handle_call({get_users_in_room, Room}, _From, State) ->
    handle_get_users_in_room(Room, State);
handle_call(get_server_info, _From, State) ->
    handle_get_server_info(State);
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({user_logout, UserPid}, State) ->
    handle_user_logout(UserPid, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
% Internal functions
handle_join(UserPid, Room, State) ->
    NewRooms = maps:update_with(Room, fun(Users) -> [UserPid | Users] end, [UserPid], State#state.rooms),
    Username = user_manager:get_username(UserPid),
    JoinNotification = #{type => <<"user_joined">>, room => list_to_binary(Room), username => Username},
    broadcast_to_room(Room, JoinNotification, UserPid, NewRooms),
    {reply, ok, State#state{rooms = NewRooms}}.

handle_leave(UserPid, Room, State) ->
    NewRooms = case maps:find(Room, State#state.rooms) of
        {ok, Users} ->
            UpdatedUsers = lists:delete(UserPid, Users),
            Username = user_manager:get_username(UserPid),
            LeaveNotification = #{type => <<"user_left">>, room => list_to_binary(Room), username => Username},
            broadcast_to_room(Room, LeaveNotification, UserPid, State#state.rooms),
            case UpdatedUsers of
                [] -> maps:remove(Room, State#state.rooms);
                _ -> maps:update(Room, UpdatedUsers, State#state.rooms)
            end;
        error ->
            State#state.rooms
    end,
    {reply, ok, State#state{rooms = NewRooms}}.

handle_send_message(From, Room, Content, State) ->
    case maps:find(Room, State#state.rooms) of
        {ok, Users} ->
            case lists:member(From, Users) of
                true ->
                    Username = user_manager:get_username(From),
                    Message = #{type => <<"message">>, room => list_to_binary(Room), from => Username, content => Content},
                    broadcast_to_room(Room, Message, State#state.rooms),
                    {reply, ok, State};
                false ->
                    {reply, {error, not_in_room}, State}
            end;
        error ->
            {reply, {error, room_not_found}, State}
    end.

handle_get_users_in_room(Room, State) ->
    case maps:find(Room, State#state.rooms) of
        {ok, Users} -> {reply, {ok, Users}, State};
        error -> {reply, {error, room_not_found}, State}
    end.

handle_get_server_info(State) ->
    TotalRooms = maps:size(State#state.rooms),
    TotalUsers = lists:sum([length(Users) || Users <- maps:values(State#state.rooms)]),
    AvgUsersPerRoom = calculate_avg_users_per_room(TotalRooms, TotalUsers),
    {LargestRoomName, LargestRoomSize} = find_largest_room(State#state.rooms),
    Info = #{
        total_rooms => TotalRooms,
        total_users => TotalUsers,
        avg_users_per_room => AvgUsersPerRoom,
        largest_room => #{name => list_to_binary(LargestRoomName), size => LargestRoomSize}
    },
    {reply, Info, State}.

handle_user_logout(UserPid, State) ->
    NewRooms = maps:map(
        fun(_Room, Users) -> lists:delete(UserPid, Users) end,
        State#state.rooms
    ),
    FinalRooms = maps:filter(fun(_Room, Users) -> length(Users) > 0 end, NewRooms),
    {noreply, State#state{rooms = FinalRooms}}.

broadcast_to_room(Room, Message, Rooms) ->
    broadcast_to_room(Room, Message, undefined, Rooms).

broadcast_to_room(Room, Message, ExcludePid, Rooms) ->
    case maps:find(Room, Rooms) of
        {ok, Users} ->
            [user_manager:send_message(UserPid, Message) || UserPid <- Users, UserPid =/= ExcludePid];
        error ->
            ok
    end.

calculate_avg_users_per_room(0, _) -> 0;
calculate_avg_users_per_room(TotalRooms, TotalUsers) ->
    float_to_binary(TotalUsers / TotalRooms, [{decimals, 2}]).

find_largest_room(Rooms) ->
    RoomSizes = [{Room, length(Users)} || {Room, Users} <- maps:to_list(Rooms)],
    case RoomSizes of
        [] -> {null, 0};
        _ -> lists:max(RoomSizes)
    end.