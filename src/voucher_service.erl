-module(voucher_service).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 voucher_summary/1,
	 async_voucher_summary/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

voucher_summary(Contract) ->
  gen_server:call(?SERVER, {voucher_summary, Contract}).

async_voucher_summary(Contract) ->
  gen_server:cast(?SERVER, {voucher_summary, self(), Contract}).

init([]) ->
  {ok, undefined}.

handle_call({voucher_summary, Contract}, _From, State) ->
  VoucherSummary = get_voucher_summary(Contract),
  {reply, {ok, VoucherSummary}, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({voucher_summary, From, Contract}, State) ->
  VoucherSummary = get_voucher_summary(Contract),
  From ! {ok, {voucher_service, VoucherSummary}},
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_voucher_summary(_Contract) ->
  lists:map(
    fun build_voucher/1,
    ["bought", "bought", "bought", "redeemed"]).

build_voucher(State) ->
  {voucher, [{count, 1}, {amount, 10}, {state, State}]}.

