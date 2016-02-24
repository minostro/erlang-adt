-module(create_invoices).

-behaviour(gen_fsm).

%% API
-export([call/2, perform/2]).

%% gen_fsm callbacks
-export([init/1,
	 idle/2,
	 creating_invoices/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {subsidiary, merchant, working_count = 0}).

call(Subsidiary, Merchant) ->
  gen_fsm:start_link(?MODULE, [Subsidiary, Merchant], []).

init([Subsidiary, Merchant]) ->
  State = #state{
	     subsidiary = Subsidiary,
	     merchant = Merchant
	    },
  {ok, idle, State, 0}.

idle(timeout, #state{subsidiary = Subsidiary, merchant = Merchant} = State) ->
  Contracts = merchants:get(contracts, Merchant),
  VoucherSummaries = lists:map(fun voucher_summary/1, Contracts),
  lists:foreach(fun({Contract, VoucherSummary}) ->
		    {ok, _WorkerPid} = create_invoice:call(Subsidiary, Contract, VoucherSummary)
		end,
		VoucherSummaries),
  {next_state, creating_invoices, State#state{working_count = length(Contracts)}}.

creating_invoices({invoice_created, _Invoice}, #state{working_count = WorkingCount} = State) ->
  NewWorkingCount = WorkingCount - 1,
  NewState = State#state{working_count = NewWorkingCount},
  case NewWorkingCount == 0 of
    true  -> {stop, normal, NewState};
    false -> {next_state, creating_invoices, NewState}
  end.

handle_event(status, StateName, State) ->
  {reply, State, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({ok, {create_invoice, Invoice}}, creating_invoices, State) ->
  gen_fsm:send_event(self(), {invoice_created, Invoice}),
  {next_state, creating_invoices, State}.


terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
perform(Subsidiary, Merchant) ->
  Contracts = merchants:get(contracts, Merchant),
  VoucherSummaries = lists:map(fun voucher_summary/1, Contracts),
  lists:foreach(fun({Contract, VoucherSummary}) ->
		    create_invoice:perform(Subsidiary, Contract, VoucherSummary)
		end,
		VoucherSummaries).

voucher_summary(Contract) ->
  {ok, VoucherSummary} = voucher_service:voucher_summary(Contract),
  {Contract, VoucherSummary}.
