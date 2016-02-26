-module(invoice_details).

-belongs_to([]).

-has_many([]).

-record(invoice_detail, {id, description, amount}).
%%% Types
-opaque invoice_detail() :: #invoice_detail{
			   id          :: integer(),
			   description :: string(),
			   amount      :: float()
			  }.
-export_type([invoice_detail/0]).

%%% API
-export([new/3]).

%%% API Data accessors
-export([get/2]).

-spec new(string(), atom(), map()) -> invoice_detail().
new(Description, Amount, Options) ->
  #invoice_detail{
     description = Description,
     amount = Amount,
     id = maps:get(id, Options, undefined)
    }.

get(id, #invoice_detail{id = Value}) ->
  Value;
get(description, #invoice_detail{description = Value}) ->
  Value;
get(amount, #invoice_detail{amount = Value}) ->
  Value.

