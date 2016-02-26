-module(merchants).

-belongs_to([{subsidiary, [{foreign_key, subsidiary_id}]}]).

-has_many([{contract, [{foreign_key, merchant_id}]}]).

-record(merchant, {id, legal_entity_id, company_name, subsidiary, contracts = []}).
%%% Types
-opaque merchant() :: #merchant{
			 id               :: integer(),
			 legal_entity_id  :: string(),
			 company_name     :: string(),
			 subsidiary       :: subsidiaries:subsidiary(),
			 contracts        :: list(contracts:contract())
			}.
-export_type([merchant/0]).

%%% API
-export([new/4]).

%%% API Data accessors
-export([get/2, set/3]).

-spec new(string(), string(), subsidiaries:subsidiary(), map()) -> merchant().
new(LegalEntityId, CompanyName, Subsidiary, Options) ->
  #merchant{
     legal_entity_id = LegalEntityId,
     company_name    = CompanyName,
     subsidiary      = Subsidiary,
     id              = maps:get(id, Options, undefined)
  }.

-spec get(id, merchant()) -> integer()
       ; (legal_entity_id, merchant()) -> string()
       ; (company_name, merchant()) -> string().
get(id, #merchant{id = Value}) ->
  Value;
get(legal_entity_id, #merchant{legal_entity_id = Value}) ->
  Value;
get(company_name, #merchant{company_name = Value}) ->
  Value;
get(subsidiary, #merchant{subsidiary = Value}) ->
  Value;
get(contracts, #merchant{contracts = Value}) ->
  Value.

set(contracts, Contracts, #merchant{contracts = Value} = Merchant) ->
  Merchant#merchant{contracts = lists:append(Contracts, Value)}.
