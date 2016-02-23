-module(merchants_transform).

-export([marshal/2]).

-spec marshal(merchants:merchant(), json | postgresql) -> jsx:json_text() | iodata().
marshal(Merchant, json) ->
  Fields = [id, legal_entity_id, company_name],
  Attrs = lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, Fields),
  jsx:encode(Attrs);
marshal(Merchant, postgresql) ->
  Id = merchants:get(id, Merchant),
  marshal(Merchant, postgresql, Id).

-spec marshal(merchants:merchant(), postgresql, undefined | integer()) -> iodata().
marshal(Merchant, postgresql, undefined) ->
  Attrs = merchant_attrs(Merchant),
  sqerl:sql({insert, invoices, Attrs}, true);
marshal(Merchant, postgresql, Id) ->
  Attrs = merchant_attrs(Merchant),
  Condition = {where, {id, '=', Id}},
  sqerl:sql({update, invoices, Attrs, Condition}, true).

-spec merchant_attrs(merchants:merchant()) -> list().
merchant_attrs(Merchant) ->
  Fields = [legal_entity_id, company_name],
  lists:flatmap(fun(Field)-> [{Field, merchants:get(Field, Merchant)}] end, Fields).



