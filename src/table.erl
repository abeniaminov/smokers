%%%-------------------------------------------------------------------
%%% @author abenniaminov@gmail.com
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. нояб. 2020 20:51
%%%-------------------------------------------------------------------
-module(table).
-author("abenniaminov@gmail.com").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1,  empty/3, not_empty/3, stopped/3,  terminate/3,
  code_change/4, callback_mode/0]).
-export([put/1, get/0, start/0, stop/0]).

-type ingredient() :: match | paper | tobacco.
-type smoker() :: match | paper | tobacco.
-type ingredients() :: [smoker()].

-define(SERVER, ?MODULE).
-define(smokers ,[match, paper, tobacco]).

-record(table_state, {ingredients = [] :: [ingredient()]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec put(ingredients()) -> ok.
put(Ingredients) ->
  gen_statem:cast(?SERVER, {ingredients, Ingredients}).

-spec get() ->  {ingredients, ingredients()}.
get() ->
  gen_statem:call(?SERVER, get).

-spec start() -> ok.
start() ->
  gen_statem:cast(?SERVER, start).

-spec stop() -> ok.
stop() ->
  gen_statem:cast(?SERVER, stop).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================


init([]) ->
  {ok, stopped, #table_state{}}.

callback_mode() ->
  state_functions.


stopped(cast, start, State) ->
  barmen:empty_table(),
  {next_state, empty, State};
stopped(_, _, State) ->
  {next_state, stopped, State#table_state{ingredients = []}}.

empty(cast, {ingredients, Ingredients}, State) ->
  lists:foreach(fun(X) -> smoker:look_at(X, Ingredients) end , ?smokers),
  {next_state, not_empty, State#table_state{ingredients = Ingredients}};
empty(cast, stop, State) ->
  {next_state, stopped, State#table_state{ingredients = []}}.


not_empty({call, From}, get, State = #table_state{ingredients = Ingredients}) ->
  barmen:empty_table(),
  {next_state, empty, State#table_state{ingredients = []}, [{reply, From, {ingredients, Ingredients}}]};
not_empty(cast, stop, State) ->
  {next_state, stopped, State#table_state{ingredients = []}, [postpone]}.

terminate(_Reason, _StateName, _State = #table_state{}) ->
  ok.

code_change(_OldVsn, StateName, State = #table_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
