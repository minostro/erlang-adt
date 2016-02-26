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
load(Attributes, BelongsToAttrs, HasManyAttrs, postgresql) ->
  Amount = proplists:get_value(amount, Attributes),
  Memo = proplists:get_value(memo, Attributes),
  BelongsTo = load_belongs_to(BelongsToAttrs, postgresql),
  Args = [Amount, Memo] ++ BelongsTo ++ [maps:from_list(Attributes)],
  Invoice = apply(invoices, new, Args),

  lists:foldl(fun({Type, HasManyValue}, NewInvoice) ->
		 invoices:set(attr(Type), HasManyValue, NewInvoice)
	     end,
	     Invoice,
	     load_has_many(HasManyAttrs, postgresql)).

to_proplist(Invoice) ->
  to_proplist(Invoice, []).

to_proplist(Invoice, ExcludedFields) ->
  Fields = lists:subtract(?FIELDS, ExcludedFields),
  lists:flatmap(fun(Field)-> [{Field, invoices:get(Field, Invoice)}] end, Fields).

load_belongs_to(BelongsToAttrs, Backend) ->
  lists:map(fun({Type, Attrs, BelongsTo, HasMany}) ->
		marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)
	    end,
	    BelongsToAttrs).

load_has_many(HasManyAttrs, Backend) ->
  lists:map(fun({Type, Values}) ->
		HasManyValues = lists:map(fun({Attrs, BelongsTo, HasMany}) ->
					      marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)
					  end,
					  Values),
		{Type, HasManyValues}
	    end,
	    HasManyAttrs).

attr(invoice_detail) ->
  invoice_details.
