-module(subsidiaries_marshal).
-define(FIELDS, [id, name, country, last_assigned_number]).

-export([load/2, dump/2]).

-spec dump(subsidiaries:subsidiary(), json) -> jsx:json_text()
       ;  (subsidiaries:subsidiary(), postgresql) -> list().
dump(Subsidiary, json) ->
  Attrs = to_proplist(Subsidiary),
  jsx:encode(Attrs);
dump(Subsidiary, postgresql) ->
  SubsidiaryAttrs = to_proplist(Subsidiary),
  Descendants = [],
  [SubsidiaryAttrs, Descendants].

-spec load(list(), postgresql) -> invoices:invoice().
load(SubsidiaryAttrs, postgresql) ->
  Name = proplists:get_value(name, SubsidiaryAttrs),
  Country = proplists:get_value(country, SubsidiaryAttrs),
  subsidiaries:new(Name, Country, maps:from_list(SubsidiaryAttrs)).

to_proplist(Subsidiary) ->
  lists:flatmap(fun(Field)-> [{Field, subsidiaries:get(Field, Subsidiary)}] end, ?FIELDS).
