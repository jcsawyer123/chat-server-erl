-module(chat_protocol).
-export([handle_message/2]).

handle_message(#{<<"type">> := Type} = Message, State) ->
    handle_message_by_type(Type, Message, State);
handle_message(_, _State) ->
    {error, invalid_message_format}.

handle_message_by_type(<<"login">>, #{<<"username">> := Username}, State) ->
    case user_manager:login(Username, self()) of
        ok -> {reply_and_update, #{type => <<"login_ack">>, username => Username}, State#{user_pid => self()}};
        {error, Reason} -> log_and_return_error(login, Reason)
    end;

handle_message_by_type(<<"join">>, #{<<"room">> := Room}, #{user_pid := UserPid}) ->
    handle_room_action(join, UserPid, binary_to_list(Room));

handle_message_by_type(<<"leave">>, #{<<"room">> := Room}, #{user_pid := UserPid}) ->
    handle_room_action(leave, UserPid, binary_to_list(Room));

handle_message_by_type(<<"message">>, #{<<"room">> := Room, <<"content">> := Content}, #{user_pid := UserPid}) ->
    case room_manager:send_message(UserPid, binary_to_list(Room), Content) of
        ok -> {reply, #{type => <<"message_ack">>, room => Room}};
        {error, Reason} -> {error, Reason}
    end;

handle_message_by_type(<<"broadcast">>, #{<<"content">> := Content}, #{user_pid := UserPid}) ->
    case user_manager:broadcast(UserPid, Content) of
        ok -> {reply, #{type => <<"broadcast_ack">>}};
        {error, Reason} -> {error, Reason}
    end;

handle_message_by_type(<<"get_all_rooms">>, _Message, _State) ->
    Rooms = room_manager:get_all_rooms(),
    {reply, #{type => <<"all_rooms">>, rooms => Rooms}};

handle_message_by_type(<<"get_users_in_room">>, #{<<"room">> := Room}, _State) ->
    case room_manager:get_users_in_room(binary_to_list(Room)) of
        {ok, Users} -> {reply, #{type => <<"users_in_room">>, room => Room, users => Users}};
        {error, Reason} -> {error, Reason}
    end;

handle_message_by_type(<<"get_server_info">>, _Message, _State) ->
    Info = room_manager:get_server_info(),
    {reply, #{type => <<"server_info">>, info => Info}};

handle_message_by_type(Type, _Message, _State) ->
    {error, {unknown_message_type, Type}}.

handle_room_action(Action, UserPid, Room) ->
    case room_manager:Action(UserPid, Room) of
        ok -> {reply, #{type => <<(atom_to_binary(Action))/binary, "_ack">>, room => list_to_binary(Room)}};
        {error, Reason} -> {error, Reason}
    end.

log_and_return_error(Operation, Reason) ->
    io:format('~p error: ~p~n', [Operation, Reason]),
    {error, Reason}.