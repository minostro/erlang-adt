-module(store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupervisorFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5},
  Store = #{
    id => store,
    start => {store, start_link, []},
    restart => permanent,
    shutdown => brutal_kill},
  Postgresql = #{
    id => postgresql,
    start => {postgresql, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor},
  Children = [Store, Postgresql],
  {ok, {SupervisorFlags, Children} }.
