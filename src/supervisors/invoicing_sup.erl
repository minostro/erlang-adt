-module(invoicing_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupervisorFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5},
  VoucherService = #{
    id => voucher_service,
    start => {voucher_service, start_link, []},
    restart => permanent,
    shutdown => brutal_kill},
  ProcessMerchantSup = #{
    id => process_merchant_sup,
    start => {process_merchant_sup, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor},
  CreateInvoicesSup = #{
    id => create_invoices_sup,
    start => {create_invoices_sup, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor},
  StoreSupervisor = #{
    id => store_sup,
    start => {store_sup, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor},
  Children = [VoucherService, ProcessMerchantSup, CreateInvoicesSup, StoreSupervisor],
  {ok, {SupervisorFlags, Children} }.
