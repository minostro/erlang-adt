-module(db).

%%% API definition
-export([find/2, save/2, where/2]).

where(Type, Condition) ->
  Connection = get_connection(),
  Query = build_select(Type, Condition),
  {ok, Rows} = squery(Connection, Query),
  lists:map(fun(Row) ->
		marshal:load(Type, Row, postgresql)
	    end,
	    Rows).

-spec find(subsidiary, tuple()) -> subsidiaries:subsidiary()
        ; (merchant, tuple()) -> merchants:merchant()
        ; (invoice, tuple()) -> invoices:invoice().
find(Type, Condition) ->
  Connection = get_connection(),
  Query = build_select(Type, Condition),
  {ok, [Row | _]} = squery(Connection, Query),
  marshal:load(Type, Row, postgresql).

-spec save(subsidiary, subsidiaries:subsidiary()) -> subsidiaries:subsidiary()
       ;  (invoice, invoices:invoice()) -> invoices:invoice().
save(Type, Value) ->
  [TypeAttrs, Descendants] = marshal:dump(Type, Value, postgresql),
  Id = save(Type, TypeAttrs, Descendants, proplists:get_value(id, TypeAttrs)),
  find(Type, {id, '=', Id}).

save(Type, Attrs, Descendants, undefined) ->
  insert(Type, proplists:delete(id, Attrs), Descendants);
save(Type, Attrs, Descendants, Id) ->
  update(Type, Id, proplists:delete(id, Attrs), Descendants),
  Id.

%%% Internal Functions
table_name(invoice) ->
  invoices;
table_name(merchant) ->
  merchants;
table_name(subsidiary) ->
  subsidiaries;
table_name(invoice_detail) ->
  invoice_details;
table_name(contract) ->
  contracts.

get_connection() ->
  {ok, C} = epgsql:connect("localhost", "test", "", [{database, "test"}]),
  C.

parent_attr(invoice) ->
  invoice_id.

insert(Type, TypeAttrs, Descendants) ->
  Connection = get_connection(),
  Query = build_insert(Type, TypeAttrs),
  {ok, [Result]} = iquery(Connection, Query),
  NewId = proplists:get_value(id, Result),
  save_descendants(Type, NewId, Descendants),
  NewId.

save_descendants(_, _, []) ->
  [];
save_descendants(Type, ParentId, Descendants) ->
  ParentAttr = [{parent_attr(Type), ParentId}],
  lists:map(fun({DescType, [Attrs, DescAttrs]}) ->
		Id = proplists:get_value(id, Attrs),
		save(DescType, Attrs ++ ParentAttr, DescAttrs, Id)
	    end,
	    Descendants).

update(Type, Id, TypeAttrs, Descendants) ->
  Connection = get_connection(),
  Query = build_update(Type, TypeAttrs, {id, '=', Id}),
  {ok, 1} = uquery(Connection, Query),
  save_descendants(Type, Id, Descendants).


build_select(Type, Condition) ->
  From = {from, table_name(Type)},
  Where = {where, Condition},
  sqerl:sql({select, '*', From, Where}, true).

build_insert(Type, Attrs) ->
  TableName = table_name(Type),
  Returning = {returning, id},
  sqerl:sql({insert, TableName, Attrs, Returning}, true).

build_update(Type, Attrs, Condition) ->
  TableName = table_name(Type),
  Where = {where, Condition},
  sqerl:sql({update, TableName, Attrs, Where}, true).

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
