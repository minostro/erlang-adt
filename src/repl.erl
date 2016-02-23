-module(repl).
-compile(export_all).

-spec new_merchant() -> merchants:merchant().
new_merchant() ->
  merchants:new(legal_id, "My Company", #{}).

-spec new_subsidiary() -> subsidiaries:subsidiary().
new_subsidiary() ->
  subsidiaries:new("Intl-Travel", ch, #{}).

-spec new_invoice(subsidiaries:subsdiary(), merchants:merchant()) -> invoices:invoice().
new_invoice(Subsidiary, Merchant) ->
  invoices:new(10, "my memo", Subsidiary, Merchant, #{title => "my title"}).
