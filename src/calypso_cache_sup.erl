-module(calypso_cache_sup).
-author("Sergey Loguntsov").

-behaviour(supervisor).

%% API
-export([start_link/0,
  add_child/2, delete_child/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).

add_child(Name, Tick) when is_atom(Name), is_integer(Tick) ->
  supervisor:start_child(?SERVER,  { Name, { cl_cache_server, start_link, [ Name, Tick ]},
   permanent, 2000, worker, [ cl_cache_server ]}).

delete_child(Name) when is_atom(Name) ->
  supervisor:terminate_child(?SERVER, Name),
  supervisor:delete_child(?SERVER, Name),
  ok.

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
