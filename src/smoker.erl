%%%-------------------------------------------------------------------
%%% @author abenniaminov@gmail.com
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. нояб. 2020 20:52
%%%-------------------------------------------------------------------
-module(smoker).
-author("abenniaminov@gmail.com").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, waiting/3, smoking/3, terminate/3,
  code_change/4, callback_mode/0]).

-export([get/1, look_at/2 ]).

-define(SERVER, ?MODULE).
-define(smoke_ingredients ,[match, paper, tobacco]).
-define(smoking_time, 500).

-type smoker() :: match | paper | tobacco.
-type ingredients() :: [smoker()].

-record(smoker_state,
    {
      smoker_type :: smoker()
    }
  ).

%%%===================================================================
%%% API
%%%===================================================================

-spec get(smoker()) -> {ingredient, smoker()}.
get(Smoker) ->
  gen_statem:call(Smoker, get).

-spec look_at(smoker(), ingredients()) -> ok.
look_at(Smoker, Ingredients) ->
  gen_statem:cast(Smoker, {look_at, Ingredients}).

-spec start_link(smoker()) -> {ok, pid()}.
start_link(Smoker) ->
  gen_statem:start_link({local, Smoker}, ?MODULE, [Smoker], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================


init([Smoker]) ->
  {ok, waiting, #smoker_state{smoker_type = Smoker}}.

callback_mode() ->
  state_functions.


waiting({call, From}, get, #smoker_state{smoker_type = Ingredient}) ->
  logger:info("Mr. ~s gets ~p~n", [upper_atom(Ingredient),Ingredient]),
  {keep_state_and_data, [{reply, From, {ingredient, Ingredient}}]};

waiting(cast, {look_at, Ingredients}, #smoker_state{smoker_type = Ingredient} = State) ->
  case lists:usort([Ingredient|Ingredients]) of
    ?smoke_ingredients ->
      table:get(),
      logger:info("Mr. ~s takes ~p and starts smoking~n", [upper_atom(Ingredient),Ingredients]),
      {next_state, smoking, State, [{state_timeout, ?smoking_time, stop_smoking}]};
    _ ->
      logger:info("These are the wrong ingredients ~p for Mr. ~s to start smoking~n", [Ingredients, upper_atom(Ingredient)]),
      keep_state_and_data
  end.


smoking(state_timeout, stop_smoking, #smoker_state{smoker_type = Ingredient} = State) ->
  NextStateName = waiting,
  logger:info("Mr. ~s jast stopped smoking.~n", [upper_atom(Ingredient)]),
  {next_state, NextStateName, State};

smoking(_, Ctx, #smoker_state{smoker_type = Ingredient}) ->
  logger:info("Mr. ~s is smoking now. Message \"~p\" postponed ~n", [upper_atom(Ingredient), Ctx]),
  {keep_state_and_data, [postpone]}.




terminate(_Reason, _StateName, _State = #smoker_state{}) ->
  ok.

code_change(_OldVsn, StateName, State = #smoker_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

upper_atom(A) ->
  string:uppercase(atom_to_list(A)).