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
  VoucherSummary = voucher_service:vouchers(Contract),
  {Contract, VoucherSummary}.
