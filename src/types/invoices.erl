-module(invoices).

-record(invoice, {id, amount, memo, title, merchant, subsidiary}).

%%% Types
-opaque invoice() :: #invoice{
			id         :: integer(),
			amount     :: integer(),
			memo       :: string(),
			title      :: string(),
			merchant   :: merchants:merchant(),
			subsidiary :: subsidiaries:subsidiary()
		       }.
-export_type([invoice/0]).

%%% API
-export([new/5, get/2]).

-spec new(integer(), string(), subsidiaries:subsidiary(), merchants:merchant(), map()) -> invoice().
new(Amount, Memo, Subsidiary, Merchant, Options) ->
  #invoice{
     subsidiary = Subsidiary,
     merchant   = Merchant,
     amount     = Amount,
     memo       = Memo,
     title      = maps:get(title, Options, ""),
     id         = maps:get(id, Options, undefined)
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
  Value.
