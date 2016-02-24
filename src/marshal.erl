-module(marshal).

-export([load/3, dump/3]).

load(invoice, Data, Context) ->
  invoices_marshal:load(Data, Context);
load(subsidiary, Data, Context) ->
  subsidiaries_marshal:load(Data, Context);
load(merchant, Data, Context) ->
  merchants_marshal:load(Data, Context);
load(invoice_detail, Data, Context) ->
  invoice_details_marshal:load(Data, Context).

dump(invoice, Invoice, Context) ->
  invoices_marshal:dump(Invoice, Context);
dump(subsidiary, Subsidiary, Context) ->
  subsidiaries_marshal:dump(Subsidiary, Context);
dump(merchant, Merchant, Context) ->
  merchants_marshal:dump(Merchant, Context);
dump(invoice_detail, InvoiceDetail, Context) ->
  invoice_details_marshal:dump(InvoiceDetail, Context).


