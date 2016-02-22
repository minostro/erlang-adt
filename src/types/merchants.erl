-module(merchants).
-define(ATTRIBUTES, [legal_entity_id, company_name]).

%%% Types
-opaque merchant() :: map().
-export_type([merchant/0]).

%%% API
-export([new/1, company_name/1]).

-spec new(map()) -> merchant().
new(Attributes) ->
  maps:with(?ATTRIBUTES, Attributes).

-spec company_name(merchant()) -> any().
company_name(#{company_name := CompanyName}) ->
  CompanyName.
