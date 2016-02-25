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
  BelongsTo = load_belongs_to(Attributes),
  Args = [LegalEntityId, CompanyName] ++ BelongsTo ++ [maps:from_list(Attributes)],
  apply(merchants, new, Args).

to_proplist(Merchant) ->
  lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, ?FIELDS).

load_belongs_to(Attributes) ->
  BelongsTo = proplists:get_value(belongs_to, merchants:module_info(attributes)),
  lists:map(fun({Type, FKey}) ->
		ForeignKeyValue = proplists:get_value(FKey, Attributes),
		store:find(postgresql, Type, {id, '=', ForeignKeyValue})
	    end,
	    BelongsTo).
