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
load(Attributes, BelongsToAttrs, HasManyAttrs, postgresql) ->
  LegalEntityId = proplists:get_value(legal_entity_id, Attributes),
  CompanyName = proplists:get_value(company_name, Attributes),
  BelongsTo = load_belongs_to(BelongsToAttrs, postgresql),
  Args = [LegalEntityId, CompanyName] ++ BelongsTo ++ [maps:from_list(Attributes)],
  Merchant = apply(merchants, new, Args),
  lists:foldl(fun({Type, HasManyValue}, NewMerchant) ->
		  merchants:set(attr(Type), HasManyValue, NewMerchant)
	      end,
	      Merchant,
	      load_has_many(HasManyAttrs, postgresql)).

to_proplist(Merchant) ->
  lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, ?FIELDS).

load_belongs_to(BelongsToAttrs, Backend) ->
  lists:map(fun({Type, Attrs, BelongsTo, HasMany}) ->
		marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)
	    end,
	    BelongsToAttrs).

load_has_many(HasManyAttrs, Backend) ->
  lists:map(fun({Type, Values}) ->
		HasManyValues = lists:map(fun({Attrs, BelongsTo, HasMany}) ->
					      marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)
					  end,
					  Values),
		{Type, HasManyValues}
	    end,
	    HasManyAttrs).

attr(contract) ->
  contracts.
