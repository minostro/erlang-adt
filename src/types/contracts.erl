-module(contracts).

-belongs_to([{merchant, merchant_id}]).
-has_many([{invoice, invoices}]).

-record(contract, {id, number, merchant}).
%%% Types
-opaque contract() :: #contract{
			 id     :: integer(),
			 number :: integer(),
			 merchant :: merchants:merchant()
			}.
-export_type([contract/0]).

%%% API declaration
-export([new/3, get/2]).

-spec new(integer(), merchants:merchant(), map()) -> contract().
new(Number, Merchant, Options) ->
  #contract{
     number = Number,
     merchant = Merchant,
     id       = maps:get(id, Options, undefined)
    }.

get(id, #contract{id = Value}) ->
  Value;
get(number, #contract{number = Value}) ->
  Value;
get(merchant, #contract{merchant = Value}) ->
  Value.
