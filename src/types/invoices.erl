-module(invoices).

-belongs_to([{subsidiary, [{foreign_key, subsidiary_id}]},
	     {merchant, [{foreign_key, merchant_id}]}]).
-has_many([invoice_detail]).

-record(invoice, {id, amount, memo, title, merchant, subsidiary, invoice_details}).

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
-export([new/5, get/2, set/3]).

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

set(invoice_details, InvoiceDetails, #invoice{invoice_details = Value} = Invoice) ->
  Invoice#invoice{invoice_details = lists:append(InvoiceDetails, Value)}.
