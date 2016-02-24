-module(create_invoices).

-export([perform/2]).

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
