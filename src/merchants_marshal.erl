-module(merchants_marshal).

-export([load/3, dump/2]).

-spec dump(merchants:merchant(), json) -> jsx:json_text().
dump(Merchant, json) ->
  Fields = [id, legal_entity_id, company_name],
  Attrs = lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, Fields),
  jsx:encode(Attrs).

-spec load(tuple(), list(), postgresql) -> merchants:merchant().
load(Row, Columns, postgresql) ->
  lists:map(fun({Column, Value}) -> build_attribute(Column, Value) end, lists:zip(Columns, Row)).

build_attribute({column, Name, Type}, Value) ->
  {binary_to_atom(Name, utf8), convert_value_to(Value, Type)}.

convert_value_to(Value, int4) ->
  binary_to_integer(Value);
convert_value_to(Value, varchar) ->
  binary_to_list(Value).

