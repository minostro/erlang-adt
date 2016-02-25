-module(invoice_details_marshal).
-define(FIELDS, [id, description, amount]).

-export([load/2, dump/2]).

-spec dump(invoice_details:invoice_detail(), json) -> jsx:json_text()
       ;  (invoice_details:invoice_detail(), postgresql) -> list().
dump(InvoiceDetail, json) ->
  Attrs = to_proplist(InvoiceDetail),
  jsx:encode(Attrs);
dump(InvoiceDetail, postgresql) ->
  InvoiceDetailAttrs = to_proplist(InvoiceDetail),
  Descendants = [],
  [InvoiceDetailAttrs, Descendants].

-spec load(list(), postgresql) -> invoice_details:invoice_detail().
load(InvoiceDetailAttrs, postgresql) ->
  Description = proplists:get_value(description, InvoiceDetailAttrs),
  Amount = proplists:get_value(amount, InvoiceDetailAttrs),
  invoice_details:new(Description, Amount, maps:from_list(InvoiceDetailAttrs)).

to_proplist(InvoiceDetail) ->
  lists:flatmap(fun(Field)-> [{Field, invoice_details:get(Field, InvoiceDetail)}] end, ?FIELDS).
