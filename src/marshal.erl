-module(marshal).

-export([load/5, dump/3]).

load(invoice, Data, BelongsToData, HasManyData, Context) ->
  invoices_marshal:load(Data, BelongsToData, HasManyData, Context);
load(subsidiary, Data, BelongsToData, HasManyData, Context) ->
  subsidiaries_marshal:load(Data, BelongsToData, HasManyData, Context);
load(merchant, Data, BelongsToData, HasManyData, Context) ->
  merchants_marshal:load(Data, BelongsToData, HasManyData, Context);
load(invoice_detail, Data, BelongsToData, HasManyData, Context) ->
  invoice_details_marshal:load(Data, BelongsToData, HasManyData, Context);
load(contract, Data, BelongsToData, HasManyData, Context) ->
  contracts_marshal:load(Data, BelongsToData, HasManyData, Context).

dump(invoice, Invoice, Context) ->
  invoices_marshal:dump(Invoice, Context);
dump(subsidiary, Subsidiary, Context) ->
  subsidiaries_marshal:dump(Subsidiary, Context);
dump(merchant, Merchant, Context) ->
  merchants_marshal:dump(Merchant, Context);
dump(invoice_detail, InvoiceDetail, Context) ->
  invoice_details_marshal:dump(InvoiceDetail, Context);
dump(contract, Contract, Context) ->
  contracts_marshal:dump(Contract, Context).



