-module(invoicing_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  %% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
  {ok, { {one_for_all, 0, 1}, []} }.
