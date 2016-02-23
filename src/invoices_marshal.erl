-module(invoices_marshal).
-define(FIELDS, [id, title, memo, amount, merchant, subsidiary]).

-export([load/2, dump/2]).

-spec dump(invoices:invoice(), json) -> jsx:json_text().
dump(Invoice, json) ->
  Attrs = to_proplist(Invoice),
  jsx:encode(Attrs);
dump(Invoice, postgresql) ->
  Attrs = to_proplist(Invoice, [merchant, subsidiary]),
  BelongsTo = [
    {merchant_id, merchants:get(id, invoices:get(merchant, Invoice))},
    {subsidiary_id, subsidiaries:get(id, invoices:get(subsidiary, Invoice))}
  ],
  lists:append(Attrs, BelongsTo).

-spec load(list(), postgresql) -> invoices:invoice().
load(InvoiceAttrs, postgresql) ->
  Amount = proplists:get_value(amount, InvoiceAttrs),
  Memo = proplists:get_value(memo, InvoiceAttrs),
  MerchantId = proplists:get_value(merchant_id, InvoiceAttrs),
  SubsidiaryId = proplists:get_value(subsidiary_id, InvoiceAttrs),
  Subsidiary = db:find(subsidiary, {id, '=', SubsidiaryId}),
  Merchant = db:find(merchant, {id, '=', MerchantId}),
  invoices:new(Amount, Memo, Subsidiary, Merchant, maps:from_list(InvoiceAttrs)).

to_proplist(Invoice) ->
  to_proplist(Invoice, []).

to_proplist(Invoice, ExcludedFields) ->
  Fields = lists:subtract(?FIELDS, ExcludedFields),
  lists:flatmap(fun(Field)-> [{Field, invoices:get(Field, Invoice)}] end, Fields).
