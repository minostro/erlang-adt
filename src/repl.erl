-module(repl).
-compile(export_all).

-spec new_merchant() -> merchants:merchant().
new_merchant() ->
  merchants:new(#{id => 1, legal_entity_id => abc1}).

-spec new_invoice(merchants:merchant()) -> invoices:invoice().
new_invoice(Merchant) ->
  invoices:new(10, "my memo", Merchant, #{title => "my title"}).
