-module(chat_global).
-behaviour(ow_zone).

-export([
         init/1,
         handle_join/4,
         handle_part/3,
         handle_rpc/5,
         handle_tick/2,
         rpc_info/0
        ]).

-export([
         start/0,
         stop/0,
         join/2,
         part/1,
         send/2
        ]).

-define(SERVER, ?MODULE).

%% API 

% Overworld RPCs
-define(CHAT_JOIN, 16#1001).
-define(CHAT_PART, 16#1002).
-define(CHAT_SEND, 16#1003).
-define(CHAT_XFER, 16#1004).

rpc_info() ->
    [
        #{
            opcode => ?CHAT_JOIN,
            c2s_handler => {?MODULE, join, 2},
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_PART,
            c2s_handler => {?MODULE, part, 1},
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_SEND,
            c2s_handler => {?MODULE, send, 2},
            c2s_proto => chat_msg,
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_XFER,
            s2c_call => state_transfer,
            encoder => chat_pb
        }
    ].

start() ->
    ow_zone:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    ow_zone:stop(?SERVER).

join(Msg, Session) ->
    ow_zone:join(?SERVER, Msg, Session).

part(Session) ->
    ow_zone:part(?SERVER, Session).

send(Msg, Session) ->
    ow_zone:rpc(?SERVER, chat_msg, Msg, Session).

% Required callbacks
init([]) ->
    InitialState = #{ msgs => [] },
    {ok, InitialState}.

handle_join(_Msg, Session, _Players, State) ->
    ID = ow_session:get_id(Session),
    logger:notice("Player ~p has joined the server!", [ID]),
    {ok, noreply, State}.

handle_part(Session, _Players, State) ->
    ID = ow_session:get_id(Session),
    logger:notice("Player ~p has left the server!", [ID]),
    {ok, noreply, State}.

handle_rpc(chat_msg, Msg, Session, Players, State) ->
    ID = ow_session:get_id(Session),
    % Make sure the player has actually joined the one
    State1 = 
        case ow_zone:is_player(ID, Players) of
            false ->
                State;
            true -> 
                % get the text of the message sent
                Text = maps:get(text, Msg),
                % get the buffer of all msgs
                Msgs = maps:get(msgs, State, []),
                % Add this new message along with the ID to the buffer
                Msgs1 = [ #{id => ID, text => Text} | Msgs ],
                % Update the state
                State#{ msgs := Msgs1 }
        end,
    {ok, noreply, State1}.

handle_tick(_Players, State = []) ->
    {ok, noreply, State};
handle_tick(_Players, State) ->
    % Empty the buffer after sending it to everyone.
    State1 = #{ msgs => []},
    Reply = {'@zone', {state_transfer, State}},
    {ok, Reply, State1}.
