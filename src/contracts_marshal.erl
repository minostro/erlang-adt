-module(contracts_marshal).
-define(FIELDS, [id, number, merchant]).

%%% API definition
-export([load/2, dump/2]).

-spec dump(contracts:contract(), json) -> jsx:json_text().
dump(Contract, json) ->
  Attrs = to_proplist(Contract),
  jsx:encode(Attrs);
dump(Contract, postgresql) ->
  Attrs = to_proplist(Contract, belongs_to()),
  BelongsToAttrs = lists:map(fun(Type) ->
				 ModuleType = pluralize(Type),
				 {belongs_to_name(Type), ModuleType:get(id, (contracts:get(Type, Contract)))}
			     end,
			     belongs_to()),
  ContractAttrs = lists:append(Attrs, BelongsToAttrs),
  Descendants = [],
  [ContractAttrs, Descendants].

-spec load(list(), postgresql) -> contracts:contract().
load(ContractAttributes, postgresql) ->
  Number = proplists:get_value(legal_entity_id, ContractAttributes),
  %HINT: it's interesting that I will need to make constructors,
  %more flexibles in order to support generic initialization of
  %the types here.
  MerchantId = proplists:get_value(merchant_id, ContractAttributes),
  Merchant = db:find(merchant, {id, '=', MerchantId}),
  contracts:new(Number, Merchant, maps:from_list(ContractAttributes)).

to_proplist(Contract) ->
  to_proplist(Contract, []).

to_proplist(Contract, ExcludedFields) ->
  Fields = lists:subtract(?FIELDS, ExcludedFields),
  lists:flatmap(fun(Field)-> [{Field, contracts:get(Field, Contract)}] end, Fields).

belongs_to() ->
  ModuleAttrs = contracts:module_info(attributes),
  proplists:get_value(belongs_to, ModuleAttrs).

belongs_to_name(Type) when is_atom(Type) ->
  list_to_atom(atom_to_list(Type) ++ "_id").

pluralize(Type) ->
  list_to_atom(atom_to_list(Type) ++ "s").
