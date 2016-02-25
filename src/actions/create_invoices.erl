-module(create_invoices).
-behaviour(gen_server).

%%% API Module declaration
-export([start_link/4, perform/3]).

%%% API Gen Server declaration
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(From, Subsidiary, Contract, VoucherSummaries) ->
  gen_server:start_link(?MODULE, [From, Subsidiary, Contract, VoucherSummaries], []).

perform(Subsidiary, Contract, VoucherSummaries) ->
  Invoice = build_invoice(Subsidiary, Contract, VoucherSummaries),
  db:save(invoice, Invoice).

-spec build_invoice(subsidiaries:subsidiary(), contracts:contract(), list()) -> invoices:invoice().
build_invoice(Subsidiary, Contract, VoucherSummaries) ->
  Merchant = contracts:get(merchant, Contract),
  InvoiceDetails = lists:map(fun build_invoice_detail/1, VoucherSummaries),
  InvoiceTotal = invoice_total_amount(InvoiceDetails),
  Invoice = invoices:new(InvoiceTotal, "memo", Subsidiary, Merchant, #{}),
  invoices:set(invoice_details, InvoiceDetails, Invoice).

-spec build_invoice_detail(tuple()) -> invoice_details:invoice_detail().
build_invoice_detail({voucher, VoucherInfo}) ->
  Description = "voucher -" ++ proplists:get_value(state, VoucherInfo),
  VoucherCount = proplists:get_value(count, VoucherInfo),
  VoucherAmount = proplists:get_value(amount, VoucherInfo),
  invoice_details:new(Description, VoucherCount * VoucherAmount, #{}).

invoice_total_amount(InvoiceDetails) ->
  lists:foldl(fun(InvoiceDetail, Sum) ->
		  Sum + invoice_details:get(amount, InvoiceDetail)
	      end,
	      0,
	      InvoiceDetails).

%%%===================================================================
%%% Gen Server Implementation
%%%===================================================================
init(State) ->
  {ok, State, 0}.

handle_info(timeout, [From, Subsidiary, Contract, VoucherSummaries] = State) ->
  Invoice = perform(Subsidiary, Contract, VoucherSummaries),
  From ! {ok, {create_invoice, Invoice}},
  {stop, normal, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
