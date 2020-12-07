%%%-------------------------------------------------------------------
%% @doc smokers top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(smokers_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(N),{N, {N, start_link, []}, permanent, 2000, worker,[N]} ).
-define(CHILD(N, T),{T, {N, start_link, [T]}, permanent, 2000, worker,[N]} ).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  SupFlags = #{strategy => one_for_all,
    intensity => 5,
    period => 10},

  ChildSpecs =
    [
      ?CHILD(barmen),
      ?CHILD(smoker, match),
      ?CHILD(smoker, tobacco),
      ?CHILD(smoker, paper),
      ?CHILD(table)
      ],

  {ok, {SupFlags, ChildSpecs}}.



%% internal functions
