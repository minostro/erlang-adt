-module(voucher_service).

%%% API declaration
-export([vouchers/1]).

%%% API definition
vouchers(_ContractId) ->
  lists:map(
    fun build_voucher/1,
    ["bought", "bought", "bought", "redeemed"]).

%%% Internal functions
build_voucher(State) ->
  {voucher, [{count, 1}, {amount, 10}, {state, State}]}.

