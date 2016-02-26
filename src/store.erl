-module(store).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 where/2,
	 find/2,
	 save/2]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {backend}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [postgresql], []).

where(Type, Condition) ->
  gen_server:call(?SERVER, {where, Type, Condition}).

find(Type, Condition) ->
  gen_server:call(?SERVER, {find, Type, Condition}).

save(Type, Value) ->
  gen_server:call(?SERVER, {save, Type, Value}).

init([Backend]) ->
  {ok, #state{backend = Backend}}.

handle_call({where, Type, Condition}, _From, #state{backend = Backend} = State) ->
  Reply = handle_where(Type, Condition, Backend),
  {reply, Reply, State};
handle_call({find, Type, Condition}, _From, #state{backend = Backend} = State) ->
  Reply = handle_find(Type, Condition, Backend),
  {reply, Reply, State};
handle_call({save, Type, Value}, _From, #state{backend = Backend} = State) ->
  Reply = handle_save(Type, Value, Backend),
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_where(Type, Condition, Backend) ->
  {ok, Rows} = Backend:where(Type, Condition),
  {ok, lists:map(fun(Row) ->
		marshal:load(Type, Row, postgresql)
	    end,
	    Rows)}.

handle_find(Type, Condition, Backend) ->
  {Type, Row, BelongsTo, HasMany} = find(Type, Condition, Backend),
  {ok, marshal:load(Type, Row, BelongsTo, HasMany, Backend)}.

handle_save(Type, Value, Backend) ->
  {ok, Id} = Backend:save(Type, Value),
  handle_find(Type, {id, '=', Id}, Backend).

module_name(merchant) ->
  merchants;
module_name(subsidiary) ->
  subsidiaries;
module_name(contract) ->
  contracts;
module_name(invoice) ->
  invoices.

find(Type, Condition, Backend) ->
  {ok, Row} = Backend:find(Type, Condition),
  BelongsTo = find_belongs_to(Type, Row, Backend),
  HasMany = [],
  {Type, Row, BelongsTo, HasMany}.

find_belongs_to(Type, Attributes, Backend) ->
  ModuleName = module_name(Type),
  BelongsTo = proplists:get_value(belongs_to, ModuleName:module_info(attributes)),
  lists:map(fun(Value) -> find_belong_to(Value, Attributes, Backend) end, BelongsTo).

find_belong_to({Type, RelationAttrs}, Attributes, Backend) ->
  ForeignKey = proplists:get_value(foreign_key, RelationAttrs),
  ForeignKeyValue = proplists:get_value(ForeignKey, Attributes),
  find(Type, {id, '=', ForeignKeyValue}, Backend).
