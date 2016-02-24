-module(invoices).

-record(invoice, {id, amount, memo, title, merchant, subsidiary, invoice_details}).
-belongs_to([merchant, subsidiary]).
-has_many([invoice_detail]).

%%% Types
-opaque invoice() :: #invoice{
			id         :: integer(),
			amount     :: integer(),
			memo       :: string(),
			title      :: string(),
			merchant   :: merchants:merchant(),
			subsidiary :: subsidiaries:subsidiary(),
			invoice_details :: list(invoice_details:invoice_detail())
		       }.
-export_type([invoice/0]).

%%% API
-export([new/5, get/2, add_invoice_detail/2, add_invoice_details/2]).

-spec new(integer(), string(), subsidiaries:subsidiary(), merchants:merchant(), map()) -> invoice().
new(Amount, Memo, Subsidiary, Merchant, Options) ->
  #invoice{
     subsidiary = Subsidiary,
     merchant   = Merchant,
     amount     = Amount,
     memo       = Memo,
     title      = maps:get(title, Options, ""),
     id         = maps:get(id, Options, undefined),
     invoice_details = maps:get(invoice_details, Options, [])
  }.


-spec get(merchant, invoice()) -> merchants:merchant()
       ; (subsidiary, invoice()) -> subsidiaries:subsidiary().
get(merchant, #invoice{merchant = Value}) ->
  Value;
get(subsidiary, #invoice{subsidiary = Value}) ->
  Value;
get(id, #invoice{id = Value}) ->
  Value;
get(amount, #invoice{amount = Value}) ->
  Value;
get(memo, #invoice{memo = Value}) ->
  Value;
get(title, #invoice{title = Value}) ->
  Value;
get(invoice_details, #invoice{invoice_details = Value}) ->
  Value.


add_invoice_details([], Invoice) ->
  Invoice;
add_invoice_details([InvoiceDetail | Rest], #invoice{invoice_details = Value} = Invoice) ->
  add_invoice_details(Rest, Invoice#invoice{invoice_details = [InvoiceDetail | Value]}).

-spec add_invoice_detail(invoice_details:invoice_detail(), invoice()) -> invoice().
add_invoice_detail(InvoiceDetail, #invoice{invoice_details = Value} = Invoice) ->
  Invoice#invoice{invoice_details = [InvoiceDetail | Value]}.
