-module(merchants).

-record(merchant, {id, legal_entity_id, company_name}).
%%% Types
-opaque merchant() :: #merchant{
			 id               :: integer(),
			 legal_entity_id  :: string(),
			 company_name     :: string()
			}.
-export_type([merchant/0]).

%%% API
-export([new/3]).

%%% API Data accessors
-export([get/2]).

-spec new(string(), string(), map()) -> merchant().
new(LegalEntityId, CompanyName, Options) ->
  #merchant{
     legal_entity_id = LegalEntityId,
     company_name    = CompanyName,
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
  Value.
