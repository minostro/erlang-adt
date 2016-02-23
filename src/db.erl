-module(db).

-export([find/2]).

-spec find(merchant, tuple()) -> merchants:merchant().
find(merchant, Condition) ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  Query = sqerl:sql({select, '*', {from, merchants}, {where, Condition}}, true),
  {ok, Columns, [Row | _]} = squery(C, Query),
  merchants_marshal:load(Row, Columns, postgresql).

squery(Connection, Query) ->
  {ok, Columns, Rows} = epgsql:squery(Connection, Query),
  {ok, normalize_columns(Columns), normalize_rows(Rows)}.

normalize_columns(Columns) ->
  [{column, element(2, Column), element(3, Column)} || Column <- Columns].

normalize_rows(Rows) ->
  [tuple_to_list(Row) || Row <- Rows].
