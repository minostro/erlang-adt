-module(merchants_marshal).
-define(FIELDS, [id, legal_entity_id, company_name]).

-type attrs() :: proplists:proplist().

%%% API definition
-export([load/4, dump/2]).

-spec dump(merchants:merchant(), json) -> jsx:json_text().
dump(Merchant, json) ->
  Attrs = to_proplist(Merchant),
  jsx:encode(Attrs);
dump(Merchant, postgresql) ->
  Attrs = to_proplist(Merchant),
  Descendants = [],
  [Attrs, Descendants].

-spec load(attrs(), list(attrs()), list(attrs()), postgresql) -> merchants:merchant().
load(Attributes, BelongsToAttrs, _HasManyAttrs, postgresql) ->
  LegalEntityId = proplists:get_value(legal_entity_id, Attributes),
  CompanyName = proplists:get_value(company_name, Attributes),
  BelongsTo = load_belongs_to(BelongsToAttrs, postgresql),
  Args = [LegalEntityId, CompanyName] ++ BelongsTo ++ [maps:from_list(Attributes)],
  apply(merchants, new, Args).

to_proplist(Merchant) ->
  lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, ?FIELDS).

load_belongs_to(BelongsToAttrs, Backend) ->
  lists:map(fun({Type, Attrs, BelongsTo, HasMany}) ->
		{Type, marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)}
	    end,
	    BelongsToAttrs).
