-module(process_merchant_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
  supervisor:start_child(?SERVER, Args).

init([]) ->
  SupervisorFlags = #{
    strategy => simple_one_for_one,
    intensity => 1,
    period => 5},
  Child = #{
    id => process_merchant,
    start => {process_merchant, start_link, []},
    restart => temporary,
    shutdown => 5000,
    type => worker},
  {ok, {SupervisorFlags, [Child]}}.
