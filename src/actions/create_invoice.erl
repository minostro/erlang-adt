-module(create_invoice).

-export([perform/3]).

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
