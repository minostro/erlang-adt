-module(merchants_marshal).

-export([load/2, dump/2]).

-spec dump(merchants:merchant(), json) -> jsx:json_text().
dump(Merchant, json) ->
  Fields = [id, legal_entity_id, company_name],
  Attrs = lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, Fields),
  jsx:encode(Attrs).

-spec load(list(), postgresql) -> merchants:merchant().
load(Attributes, postgresql) ->
  LegalEntityId = proplists:get_value(legal_entity_id, Attributes),
  CompanyName = proplists:get_value(company_name, Attributes),
  merchants:new(LegalEntityId, CompanyName, maps:from_list(Attributes)).
