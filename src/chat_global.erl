-module(chat_global).
-behaviour(ow_zone).

-export([
         init/1,
         handle_join/3,
         handle_part/2,
         handle_rpc/4,
         handle_tick/2,
         rpc_info/0
        ]).

-export([
         start/0,
         stop/0,
         join/2,
         part/1,
         list/2,
         send/2
        ]).

-define(SERVER, ?MODULE).

%% API 

% Overworld RPCs
-define(CHAT_JOIN, 16#1001).
-define(CHAT_PART, 16#1002).
-define(CHAT_SEND, 16#1003).
-define(CHAT_XFER, 16#1004).
-define(CHAT_LIST, 16#1005).

rpc_info() ->
    [
        #{
            opcode => ?CHAT_JOIN,
            c2s_handler => {?MODULE, join, 2},
            s2c_call => join,
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_LIST,
            c2s_handler => {?MODULE, list, 2},
            c2s_proto => plist,
            s2c_call => plist,
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_PART,
            c2s_handler => {?MODULE, part, 1},
            s2c_call => part,
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_SEND,
            c2s_handler => {?MODULE, send, 2},
            c2s_proto => privmsg,
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

list(Msg, Session) ->
    ow_zone:rpc(?SERVER, list, Msg, Session).

send(Msg, Session) ->
    ow_zone:rpc(?SERVER, privmsg, Msg, Session).

% Required callbacks
init([]) ->
    InitialState = #{ msgs => [], handles => [] },
    {ok, InitialState}.

handle_join(Msg, Session, State) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle,ID]),
    % Add the handle to the player info
    PlayerInfo = #{ handle => Handle },
    % Let everyone know that the Player has joined
    Reply = {'@zone', {join, Msg}},
    {Reply, {ok, Session, PlayerInfo}, State}.

handle_part(Session, State) ->
    ID = ow_session:get_id(Session),
    Player = ow_player_reg:get(ID),
    Handle = maps:get(handle, ow_player_reg:get_info(Player)),
    logger:notice("Player ~p:~p has left the server!", [Handle, ID]),
    % Let everyone know that the Player departed
    Msg = #{ handle => Handle },
    Reply = {'@zone', {part, Msg}},
    {Reply, ok, State}.

handle_rpc(privmsg, Msg, Session, State) ->
    % get the text of the message sent and who sent it
    ID = ow_session:get_id(Session),
    Player = ow_player_reg:get(ID),
    Handle = maps:get(handle, ow_player_reg:get_info(Player)),
    Text = maps:get(text, Msg),
    % Add this new message along with the ID to the buffer
    Msgs0 = maps:get(msgs, State, []),
    Msgs1 = [ #{handle => Handle, text => Text} | Msgs0 ],
    % Update the state
    State1 = State#{ msgs := Msgs1 },
    {noreply, ok, State1};
handle_rpc(list, _Msg, Session, State) ->
    Players = ow_player_reg:list(self()),
    Handles = [ maps:get(handle, ow_player_reg:get_info(P)) || P <- Players ],
    logger:notice("players are: ~p", [Handles]),
    Msg = #{ handles => Handles },
    P = ow_session:get_id(Session),
    Reply = {{'@', [P]}, {plist, Msg}},
    {Reply, ok, State}.

handle_tick(_ZoneConfig, State = #{ msgs := [] }) ->
    % If the state is empty, there's nothing to do.
    {noreply,State};
handle_tick(_ZoneConfig, State) ->
    Reply = {'@zone', {state_transfer, State}},
    % Empty the buffer after sending it to everyone.
    State1 = State#{ msgs => []},
    {Reply, State1}.
