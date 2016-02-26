-module(contracts_marshal).
-define(FIELDS, [id, number, merchant]).

%%% API definition
-export([load/4, dump/2]).

-spec dump(contracts:contract(), json) -> jsx:json_text().
dump(Contract, json) ->
  Attrs = to_proplist(Contract),
  jsx:encode(Attrs);
dump(Contract, postgresql) ->
  Attrs = to_proplist(Contract, belongs_to()),
  BelongsToAttrs = lists:map(fun({Type, FKey}) ->
				 ModuleType = pluralize(Type),
				 {FKey, ModuleType:get(id, (contracts:get(Type, Contract)))}
			     end,
			     belongs_to()),
  ContractAttrs = lists:append(Attrs, BelongsToAttrs),
  Descendants = [],
  [ContractAttrs, Descendants].

-spec load(list(), list(), list(), postgresql) -> contracts:contract().
load(Attributes, BelongsToAttrs, _HasManyAttrs, postgresql) ->
  Number = proplists:get_value(legal_entity_id, Attributes),
  BelongsTo = load_belongs_to(BelongsToAttrs, postgresql),
  Args = [Number] ++ BelongsTo ++ [maps:from_list(Attributes)],
  apply(contracts, new, Args).

to_proplist(Contract) ->
  to_proplist(Contract, []).

to_proplist(Contract, ExcludedFields) ->
  Fields = lists:subtract(?FIELDS, ExcludedFields),
  lists:flatmap(fun(Field)-> [{Field, contracts:get(Field, Contract)}] end, Fields).

belongs_to() ->
  ModuleAttrs = contracts:module_info(attributes),
  proplists:get_value(belongs_to, ModuleAttrs).

pluralize(Type) ->
  list_to_atom(atom_to_list(Type) ++ "s").

load_belongs_to(BelongsToAttrs, Backend) ->
  lists:map(fun({Type, Attrs, BelongsTo, HasMany}) ->
		marshal:load(Type, Attrs, BelongsTo, HasMany, Backend)
	    end,
	    BelongsToAttrs).
