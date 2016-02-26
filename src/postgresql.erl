-module(postgresql).

-behaviour(gen_server).

%%% API definition
-export([start_link/0,
	 find/2,
	 save/2,
	 where/2]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

where(Type, Condition) ->
  gen_server:call(?SERVER, {where, Type, Condition}).

find(Type, Condition) ->
  gen_server:call(?SERVER, {find, Type, Condition}).

save(Type, Value) ->
  gen_server:call(?SERVER, {save, Type, Value}).

init([]) ->
  {ok, #state{connection = get_connection()}}.

handle_call({where, Type, Condition}, _From, #state{connection = Connection} = State) ->
  Reply = handle_where(Type, Condition, Connection),
  {reply, Reply, State};
handle_call({find, Type, Condition}, _From, #state{connection = Connection} = State) ->
  Reply = handle_find(Type, Condition, Connection),
  {reply, Reply, State};
handle_call({save, Type, Value}, _From, #state{connection = Connection} = State) ->
  Reply = handle_save(Type, Value, Connection),
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec handle_find(subsidiary | merchant | invoice, tuple(), any()) -> {ok, proplists:proplist()}.
handle_find(Type, Condition, Connection) ->
  {ok, [Row | _]} = handle_where(Type, Condition, Connection),
  {ok, Row}.

-spec handle_where(subsdiary | merchant | invoice, tuple(), any()) -> {ok, list(proplists:proplist())}.
handle_where(Type, Condition, Connection) ->
  Query = build_select(Type, Condition),
  {ok, Rows} = squery(Connection, Query),
  {ok, Rows}.

-spec handle_save(subsidiary, subsidiaries:subsidiary(), any()) -> {ok, integer()}
       ;  (invoice, invoices:invoice(), any()) -> {ok, integer()}.
handle_save(Type, Value, Connection) ->
  [TypeAttrs, Descendants] = marshal:dump(Type, Value, postgresql),
  save(Type, TypeAttrs, Descendants, Connection, proplists:get_value(id, TypeAttrs)).

save(Type, Attrs, Descendants, Connection, undefined) ->
  insert(Type, proplists:delete(id, Attrs), Descendants, Connection);
save(Type, Attrs, Descendants, Connection, Id) ->
  update(Type, Id, proplists:delete(id, Attrs), Descendants, Connection).

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

insert(Type, TypeAttrs, Descendants, Connection) ->
  Query = build_insert(Type, TypeAttrs),
  {ok, [Result]} = iquery(Connection, Query),
  NewId = proplists:get_value(id, Result),
  save_descendants(Type, NewId, Connection, Descendants),
  {ok, NewId}.

save_descendants(_, _, _, []) ->
  [];
save_descendants(Type, ParentId, Connection, Descendants) ->
  ParentAttr = [{parent_attr(Type), ParentId}],
  lists:map(fun({DescType, [Attrs, DescAttrs]}) ->
		Id = proplists:get_value(id, Attrs),
		save(DescType, Attrs ++ ParentAttr, DescAttrs, Connection, Id)
	    end,
	    Descendants).

update(Type, Id, TypeAttrs, Descendants, Connection) ->
  Query = build_update(Type, TypeAttrs, {id, '=', Id}),
  {ok, 1} = uquery(Connection, Query),
  save_descendants(Type, Id, Connection, Descendants),
  {ok, Id}.


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
