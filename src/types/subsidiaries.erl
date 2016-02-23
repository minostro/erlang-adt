-module(subsidiaries).

-record(subsidiary, {id, name, country, last_assigned_number}).
%%% Types
-opaque subsidiary() :: #subsidiary{
			   id                   :: integer(),
			   name                 :: string(),
			   country              :: atom(),
			   last_assigned_number :: integer()
			  }.
-export_type([subsidiary/0]).

%%% API
-export([new/3]).

%%% API Data accessors
-export([get/2]).

-spec new(string(), atom(), map()) -> subsidiary().
new(Name, Country, Options) ->
  #subsidiary{
     name = Name,
     country = Country,
     id = maps:get(id, Options, undefined),
     last_assigned_number = maps:get(last_assigned_number, Options, 0)
    }.

-spec get(id, subsidiary()) -> integer()
       ; (name, subsidiary()) -> string()
       ; (country, subsidiary()) -> atom()
       ; (last_assigned_number, subsidiary()) -> integer().
get(id, #subsidiary{id = Value}) ->
  Value;
get(name, #subsidiary{name = Value}) ->
  Value;
get(country, #subsidiary{country = Value}) ->
  Value;
get(last_assigned_number, #subsidiary{last_assigned_number = Value}) ->
  Value.



