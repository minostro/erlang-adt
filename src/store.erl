-module(store).

-export([where/3,
	 find/3,
	 save/3]).

where(postgresql, Type, Condition) ->
  {ok, Rows} = db:where(Type, Condition),
  lists:map(fun(Row) ->
		marshal:load(Type, Row, postgresql)
	    end,
	    Rows).

find(postgresql, Type, Condition) ->
  {ok, Row} = db:find(Type, Condition),
  marshal:load(Type, Row, postgresql).

save(postgresql, Type, Value) ->
  {ok, Id} = db:save(Type, Value),
  find(postgresql, Type, {id, '=', Id}).
