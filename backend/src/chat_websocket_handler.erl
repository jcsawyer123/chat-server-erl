-module(chat_websocket_handler).
-behaviour(cowboy_websocket).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(PING_INTERVAL, 60000).
-define(PING_TIMEOUT, 10000).
-define(MAX_MISSED_PONGS, 3).

init(Req, _State) ->
    {cowboy_websocket, Req, #{missed_pongs => 0}}.

websocket_init(State) ->
    schedule_ping(),
    {ok, State}.

websocket_handle(pong, State) ->
    {ok, State#{missed_pongs => 0}};
websocket_handle({text, Json}, State) ->
    try
        Message = jsx:decode(Json, [return_maps]),
        handle_decoded_message(Message, State)
    catch
        error:badarg ->
            log_error("Invalid JSON received: ~p", [Json]),
            error_reply(<<"invalid_json">>, State);
        _:_ ->
            log_error("Exception while decoding message: ~p", [Json]),
            error_reply(<<"internal_error">>, State)
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(ping, State) ->
    handle_ping(State);
websocket_info(check_pong, State) ->
    handle_check_pong(State);
websocket_info({send, Message}, State) ->
    {reply, {text, jsx:encode(Message)}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, _State) ->
    log_info("WebSocket connection terminated for user ~p. Reason: ~p", [self(), Reason]),
    user_manager:logout(self()),
    ok.

% Internal functions

handle_decoded_message(Message, State) ->
    try
        log_info("Decoded message: ~p", [Message]),
        case chat_protocol:handle_message(Message, State) of
            {reply, Reply} -> {reply, {text, jsx:encode(Reply)}, State};
            {update, NewState} -> {ok, NewState};
            {reply_and_update, Reply, NewState} -> {reply, {text, jsx:encode(Reply)}, NewState};
            {error, Reason} ->
                log_error("Error handling message: ~p", [Reason]),
                error_reply(Reason, State)
        end
    catch
        Error:_ ->
            log_error("Exception while handling message: ~p ~p", [Error, Message]),
            error_reply(<<"internal_error">>, State)
    end.

handle_ping(State) ->
    schedule_ping(),
    case maps:get(missed_pongs, State, 0) of
        MissedPongs when MissedPongs >= ?MAX_MISSED_PONGS ->
            log_warning("User ~p disconnected due to ~p missed pongs", [self(), MissedPongs]),
            {stop, normal, State};
        MissedPongs ->
            erlang:send_after(?PING_TIMEOUT, self(), check_pong),
            {reply, ping, State#{missed_pongs => MissedPongs + 1}}
    end.

handle_check_pong(#{missed_pongs := MissedPongs} = State) ->
    case MissedPongs of
        ?MAX_MISSED_PONGS ->
            log_warning("User ~p disconnected due to ~p missed pongs", [self(), MissedPongs]),
            {stop, normal, State};
        _ ->
            log_missed_pongs(MissedPongs),
            {ok, State}
    end.

schedule_ping() ->
    erlang:send_after(?PING_INTERVAL, self(), ping).

error_reply(Reason, State) ->
    Reply = #{type => <<"error">>, reason => format_error(Reason)},
    {reply, {text, jsx:encode(Reply)}, State}.

format_error(room_not_found) -> <<"Room not found">>;
format_error(user_not_found) -> <<"User not found">>;
format_error(not_in_room) -> <<"User is not in the room">>;
format_error(username_taken) -> <<"Username taken">>;
format_error({unknown_message_type, Type}) -> list_to_binary(io_lib:format("Unknown message type: ~s", [Type]));
format_error(invalid_message_format) -> <<"Invalid message format">>;
format_error(_) -> <<"Internal error">>.

log_info(Format, Args) -> error_logger:info_msg(Format, Args).
log_error(Format, Args) -> error_logger:error_msg(Format, Args).
log_warning(Format, Args) -> error_logger:warning_msg(Format, Args).

log_missed_pongs(MissedPongs) when MissedPongs > 0 ->
    log_info("User ~p missed ~p pong(s)", [self(), MissedPongs]);
log_missed_pongs(_) -> ok.