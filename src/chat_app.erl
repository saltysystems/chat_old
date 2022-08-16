%%%-------------------------------------------------------------------
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ow_protocol:register_app(chat),
    chat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
