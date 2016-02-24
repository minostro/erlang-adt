-module(merchants_marshal).
-define(FIELDS, [id, legal_entity_id, company_name]).

%%% API definition
-export([load/2, dump/2]).

-spec dump(merchants:merchant(), json) -> jsx:json_text().
dump(Merchant, json) ->
  Attrs = to_proplist(Merchant),
  jsx:encode(Attrs);
dump(Merchant, postgresql) ->
  Attrs = to_proplist(Merchant),
  Descendants = [],
  [Attrs, Descendants].

-spec load(list(), postgresql) -> merchants:merchant().
load(Attributes, postgresql) ->
  LegalEntityId = proplists:get_value(legal_entity_id, Attributes),
  CompanyName = proplists:get_value(company_name, Attributes),
  merchants:new(LegalEntityId, CompanyName, maps:from_list(Attributes)).

to_proplist(Merchant) ->
  lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, ?FIELDS).
