-module(invoices_marshal).
-define(FIELDS, [id, title, memo, amount, merchant, subsidiary]).

-export([load/4, dump/2]).

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
  InvoiceAttrs = lists:append(Attrs, BelongsTo),
  InvoiceDetails = lists:map(fun(InvoiceDetail) ->
				 {invoice_detail, marshal:dump(invoice_detail, InvoiceDetail, postgresql)}
			     end,
			     invoices:get(invoice_details, Invoice)),
  [InvoiceAttrs, InvoiceDetails].

-spec load(list(), list(), list(), postgresql) -> invoices:invoice().
load(Attributes, BelongsToAttrs, _HasManyAttrs, postgresql) ->
  InvoiceId = proplists:get_value(id, Attributes),
  Amount = proplists:get_value(amount, Attributes),
  Memo = proplists:get_value(memo, Attributes),
  BelongsTo = load_belongs_to(BelongsToAttrs, postgresql),
  %InvoiceDetails = store:where(postgresql, invoice_detail, {invoice_id, '=', InvoiceId}),
  Args = [Amount, Memo] ++ BelongsTo ++ [maps:from_list(Attributes)],
  apply(invoices, new, Args).
  %invoices:set(invoice_details, InvoiceDetails, apply(invoices, new, Args)).

to_proplist(Invoice) ->
  to_proplist(Invoice, []).

to_proplist(Invoice, ExcludedFields) ->
  Fields = lists:subtract(?FIELDS, ExcludedFields),
  lists:flatmap(fun(Field)-> [{Field, invoices:get(Field, Invoice)}] end, Fields).

load_belongs_to(BelongsToAttrs, Backend) ->
  lists:map(fun({Type, Attrs, BelongsTo, HasMany}) ->
		{Type, marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)}
	    end,
	    BelongsToAttrs).
