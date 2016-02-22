-module(invoices).
-define(ATTRIBUTES, [amount, memo, title, merchant]).

%%% Types
-type invoice() :: map().
-export_type([invoice/0]).

%%% API
-export([new/2]).

-spec new(map(), merchants:merchant()) -> invoice().
new(Attributes, Merchant) ->
  InvoiceAttrs = maps:with(?ATTRIBUTES, Attributes),
  maps:put(merchant, Merchant, InvoiceAttrs).
