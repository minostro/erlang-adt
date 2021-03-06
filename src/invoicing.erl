-module(invoicing).

-export([start/0,
	 process_merchant/2,
	 create_invoices/3]).

start() ->
  application:start(invoicing).

process_merchant(Subsidiary, Merchant) ->
  process_merchant_sup:start_child([Subsidiary, Merchant]).

create_invoices(Subsidiary, Contract, VoucherSummaries) ->
  create_invoices_sup:start_child([self(), Subsidiary, Contract, VoucherSummaries]).
