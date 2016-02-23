-module(db).

-export([find/2, save/2]).

-spec find(subsidiary, tuple()) -> subsidiaries:subsidiary()
        ; (merchant, tuple()) -> merchants:merchant()
        ; (invoice, tuple()) -> invoices:invoice().
find(subsidiary, Condition) ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  Query = sqerl:sql({select, '*', {from, subsidiaries}, {where, Condition}}, true),
  {ok, [Row | _]} = squery(C, Query),
  subsidiaries_marshal:load(Row, postgresql);
find(merchant, Condition) ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  Query = sqerl:sql({select, '*', {from, merchants}, {where, Condition}}, true),
  {ok, [Row | _]} = squery(C, Query),
  merchants_marshal:load(Row, postgresql);
find(invoice, Condition) ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  InvoiceQuery = sqerl:sql({select, '*', {from, invoices}, {where, Condition}}, true),
  {ok, [InvoiceRow | _]} = squery(C, InvoiceQuery),
  invoices_marshal:load(InvoiceRow, postgresql).

-spec save(subsidiary, subsidiaries:subsidiary()) -> subsidiaries:subsidiary()
       ;  (invoice, invoices:invoice()) -> invoices:invoice()
       ;  (atom(), list()) -> integer().
save(subsidiary, Subsidiary) ->
  NewId = save(subsidiaries, subsidiaries_marshal:dump(Subsidiary, postgresql)),
  find(subsidiary, {id, '=', NewId});
save(invoice, Invoice) ->
  NewId = save(invoices, invoices_marshal:dump(Invoice, postgresql)),
  find(invoice, {id, '=', NewId});
save(TableName, Attrs) ->
  Id = proplists:get_value(id, Attrs),
  case Id =:= undefined of
    true -> insert(TableName, proplists:delete(id, Attrs));
    false ->
      update(TableName, Id, proplists:delete(id, Attrs)),
      Id
  end.

insert(TableName, Attrs) ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  Query = sqerl:sql({insert, TableName, Attrs, {returning, id}}, true),
  {ok, [Result]} = iquery(C, Query),
  proplists:get_value(id, Result).

update(TableName, Id, Attrs) ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  Query = sqerl:sql({update, TableName, Attrs, {where, {'id', '=', Id}}}, true),
  {ok, 1} = uquery(C, Query).

uquery(Connection, Query) ->
  {ok, RowsAffected} = epgsql:squery(Connection, Query),
  {ok, RowsAffected}.

iquery(Connection, Query) ->
  {ok, _RowsAffected, RawColumns, RawRows} = epgsql:squery(Connection, Query),
  Columns = normalize_columns(RawColumns),
  Rows = normalize_rows(RawRows),
  Results = build_results(Columns, Rows),
  {ok, Results}.

squery(Connection, Query) ->
  {ok, RawColumns, RawRows} = epgsql:squery(Connection, Query),
  Columns = normalize_columns(RawColumns),
  Rows = normalize_rows(RawRows),
  Results = build_results(Columns, Rows),
  {ok, Results}.

normalize_columns(Columns) ->
  [{element(2, Column), element(3, Column)} || Column <- Columns].

normalize_rows(Rows) ->
  [tuple_to_list(Row) || Row <- Rows].

build_results(Columns, Rows) ->
  [build_result(Columns, Row) || Row <- Rows].

build_result(Columns, Row) ->
  lists:map(fun({Column, Value}) -> build_attribute(Column, Value) end, lists:zip(Columns, Row)).

build_attribute({Name, Type}, Value) ->
  {binary_to_atom(Name, utf8), convert_value_to(Value, Type)}.

convert_value_to(Value, int4) ->
  binary_to_integer(Value);
convert_value_to(Value, varchar) ->
  binary_to_list(Value);
convert_value_to(Value, numeric) ->
  binary_to_float(Value).
