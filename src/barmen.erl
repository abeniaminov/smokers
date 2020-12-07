%%%-------------------------------------------------------------------
%%% @author abenniaminov@gmail.com
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. нояб. 2020 20:50
%%%-------------------------------------------------------------------
-module(barmen).
-author("abenniaminov@gmail.com").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1,  waiting/3,  terminate/3,
  code_change/4, callback_mode/0]).

-export([empty_table/0]).

-define(SERVER, ?MODULE).
-define(smokers ,[match, paper, tobacco]).

-record(barmen_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec empty_table() -> ok.
empty_table() ->
  gen_statem:cast(barmen, empty_table).


-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
  {ok, waiting, #barmen_state{}}.

callback_mode() ->
  state_functions.


waiting(cast, empty_table, State) ->
  NextStateName = waiting,
  Ingredients = get_ingredients(),
  ok = put_ingredients_on_the_table(Ingredients),
  {next_state, NextStateName, State}.



terminate(_Reason, _StateName, _State = #barmen_state{}) ->
  ok.

code_change(_OldVsn, StateName, State = #barmen_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_ingredients() ->
  L = ?smokers --  [lists:nth(rand:uniform(length(?smokers)),?smokers)],
  lists:map(fun smoker:get/1, L).

put_ingredients_on_the_table(Ingredients) ->
  table:put([I || {ingredient, I} <- Ingredients]),
  ok.
