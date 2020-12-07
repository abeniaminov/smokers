%%%-------------------------------------------------------------------
%% @doc smokers public API
%% @end
%%%-------------------------------------------------------------------

-module(smokers_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    R = smokers_sup:start_link(),
    table:start(),
    R.

stop(_State) ->
    ok.

%% internal functions
