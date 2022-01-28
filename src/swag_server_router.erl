-module(swag_server_router).

-export([get_paths/1]).
-export([get_paths/2]).
-export([get_operation/1]).
-export([get_operations/0]).

-type operations() :: #{
    Method :: binary() => OperationID :: swag_server:operation_id()
}.

-type logic_handler(T) :: swag_server:logic_handler(T).

-type swagger_handler_opts() :: #{
    validation_opts => swag_server_validation:validation_opts()
}.

-type init_opts() :: {
    Operations      :: operations(),
    LogicHandler    :: logic_handler(_),
    SwaggerHandlerOpts :: swagger_handler_opts()
}.

-type operation_spec() :: #{
    path    := '_' | iodata(),
    method  := binary(),
    handler := module()
}.

-export_type([swagger_handler_opts/0]).
-export_type([init_opts/0]).
-export_type([operation_spec/0]).

-spec get_paths(LogicHandler :: logic_handler(_)) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    get_paths(LogicHandler, #{}).

-spec get_paths(LogicHandler :: logic_handler(_), SwaggerHandlerOpts :: swagger_handler_opts()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler, SwaggerHandlerOpts) ->
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, SwaggerHandlerOpts}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

-spec get_operation(swag_server:operation_id()) ->
   operation_spec().

get_operation(OperationId) ->
    maps:get(OperationId, get_operations()).

-spec get_operations() -> #{
    swag_server:operation_id() := operation_spec()
}.

get_operations() ->
    #{ 
        'GetAccountByID' => #{
            path => "/v2/processing/accounts/:accountID",
            method => <<"GET">>,
            handler => 'swag_server_accounts_handler'
        },
        'GetPaymentConversionStats' => #{
            path => "/v2/analytics/shops/:shopID/payments/stats/conversion",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentGeoStats' => #{
            path => "/v2/analytics/shops/:shopID/payments/stats/geo",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentMethodStats' => #{
            path => "/v2/analytics/shops/:shopID/customers/stats/payment_method",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentRateStats' => #{
            path => "/v2/analytics/shops/:shopID/customers/stats/rate",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentRevenueStats' => #{
            path => "/v2/analytics/shops/:shopID/payments/stats/revenue",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetCategories' => #{
            path => "/v2/processing/categories",
            method => <<"GET">>,
            handler => 'swag_server_categories_handler'
        },
        'GetCategoryByRef' => #{
            path => "/v2/processing/categories/:categoryID",
            method => <<"GET">>,
            handler => 'swag_server_categories_handler'
        },
        'CreateClaim' => #{
            path => "/v2/processing/claims",
            method => <<"POST">>,
            handler => 'swag_server_claims_handler'
        },
        'GetClaimByID' => #{
            path => "/v2/processing/claims/:claimID",
            method => <<"GET">>,
            handler => 'swag_server_claims_handler'
        },
        'GetClaims' => #{
            path => "/v2/processing/claims",
            method => <<"GET">>,
            handler => 'swag_server_claims_handler'
        },
        'RevokeClaimByID' => #{
            path => "/v2/processing/claims/:claimID/revoke",
            method => <<"PUT">>,
            handler => 'swag_server_claims_handler'
        },
        'GetContractAdjustmentByID' => #{
            path => "/v2/processing/contracts/:contractID/adjustments/:adjustmentID",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContractAdjustmentByIDForParty' => #{
            path => "/v2/processing/parties/:partyID/contracts/:contractID/adjustments/:adjustmentID",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContractAdjustments' => #{
            path => "/v2/processing/contracts/:contractID/adjustments",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContractAdjustmentsForParty' => #{
            path => "/v2/processing/parties/:partyID/contracts/:contractID/adjustments",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContractByID' => #{
            path => "/v2/processing/contracts/:contractID",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContractByIDForParty' => #{
            path => "/v2/processing/parties/:partyID/contracts/:contractID",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContracts' => #{
            path => "/v2/processing/contracts",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetContractsForParty' => #{
            path => "/v2/processing/parties/:partyID/contracts",
            method => <<"GET">>,
            handler => 'swag_server_contracts_handler'
        },
        'GetCountries' => #{
            path => "/v2/processing/countries",
            method => <<"GET">>,
            handler => 'swag_server_countries_handler'
        },
        'GetCountryByID' => #{
            path => "/v2/processing/countries/:countryID",
            method => <<"GET">>,
            handler => 'swag_server_countries_handler'
        },
        'CreateBinding' => #{
            path => "/v2/processing/customers/:customerID/bindings",
            method => <<"POST">>,
            handler => 'swag_server_customers_handler'
        },
        'CreateCustomer' => #{
            path => "/v2/processing/customers",
            method => <<"POST">>,
            handler => 'swag_server_customers_handler'
        },
        'CreateCustomerAccessToken' => #{
            path => "/v2/processing/customers/:customerID/access-tokens",
            method => <<"POST">>,
            handler => 'swag_server_customers_handler'
        },
        'DeleteCustomer' => #{
            path => "/v2/processing/customers/:customerID",
            method => <<"DELETE">>,
            handler => 'swag_server_customers_handler'
        },
        'GetBinding' => #{
            path => "/v2/processing/customers/:customerID/bindings/:customerBindingID",
            method => <<"GET">>,
            handler => 'swag_server_customers_handler'
        },
        'GetBindings' => #{
            path => "/v2/processing/customers/:customerID/bindings",
            method => <<"GET">>,
            handler => 'swag_server_customers_handler'
        },
        'GetCustomerById' => #{
            path => "/v2/processing/customers/:customerID",
            method => <<"GET">>,
            handler => 'swag_server_customers_handler'
        },
        'GetCustomerEvents' => #{
            path => "/v2/processing/customers/:customerID/events",
            method => <<"GET">>,
            handler => 'swag_server_customers_handler'
        },
        'GetCustomerPaymentMethods' => #{
            path => "/v2/processing/customers/:customerID/payment-methods",
            method => <<"GET">>,
            handler => 'swag_server_customers_handler'
        },
        'GetLocationsNames' => #{
            path => "/v2/reference/geo/location/names",
            method => <<"GET">>,
            handler => 'swag_server_geo_handler'
        },
        'CreateInvoiceTemplate' => #{
            path => "/v2/processing/invoice-templates",
            method => <<"POST">>,
            handler => 'swag_server_invoice_templates_handler'
        },
        'CreateInvoiceWithTemplate' => #{
            path => "/v2/processing/invoice-templates/:invoiceTemplateID/invoices",
            method => <<"POST">>,
            handler => 'swag_server_invoice_templates_handler'
        },
        'DeleteInvoiceTemplate' => #{
            path => "/v2/processing/invoice-templates/:invoiceTemplateID",
            method => <<"DELETE">>,
            handler => 'swag_server_invoice_templates_handler'
        },
        'GetInvoicePaymentMethodsByTemplateID' => #{
            path => "/v2/processing/invoice-templates/:invoiceTemplateID/payment-methods",
            method => <<"GET">>,
            handler => 'swag_server_invoice_templates_handler'
        },
        'GetInvoiceTemplateByID' => #{
            path => "/v2/processing/invoice-templates/:invoiceTemplateID",
            method => <<"GET">>,
            handler => 'swag_server_invoice_templates_handler'
        },
        'UpdateInvoiceTemplate' => #{
            path => "/v2/processing/invoice-templates/:invoiceTemplateID",
            method => <<"PUT">>,
            handler => 'swag_server_invoice_templates_handler'
        },
        'CreateInvoice' => #{
            path => "/v2/processing/invoices",
            method => <<"POST">>,
            handler => 'swag_server_invoices_handler'
        },
        'CreateInvoiceAccessToken' => #{
            path => "/v2/processing/invoices/:invoiceID/access-tokens",
            method => <<"POST">>,
            handler => 'swag_server_invoices_handler'
        },
        'FulfillInvoice' => #{
            path => "/v2/processing/invoices/:invoiceID/fulfill",
            method => <<"POST">>,
            handler => 'swag_server_invoices_handler'
        },
        'GetInvoiceByExternalID' => #{
            path => "/v2/processing/invoices",
            method => <<"GET">>,
            handler => 'swag_server_invoices_handler'
        },
        'GetInvoiceByID' => #{
            path => "/v2/processing/invoices/:invoiceID",
            method => <<"GET">>,
            handler => 'swag_server_invoices_handler'
        },
        'GetInvoiceEvents' => #{
            path => "/v2/processing/invoices/:invoiceID/events",
            method => <<"GET">>,
            handler => 'swag_server_invoices_handler'
        },
        'GetInvoicePaymentMethods' => #{
            path => "/v2/processing/invoices/:invoiceID/payment-methods",
            method => <<"GET">>,
            handler => 'swag_server_invoices_handler'
        },
        'RescindInvoice' => #{
            path => "/v2/processing/invoices/:invoiceID/rescind",
            method => <<"POST">>,
            handler => 'swag_server_invoices_handler'
        },
        'ActivateMyParty' => #{
            path => "/v2/processing/me/activate",
            method => <<"PUT">>,
            handler => 'swag_server_parties_handler'
        },
        'ActivatePartyByID' => #{
            path => "/v2/processing/parties/:partyID/activate",
            method => <<"PUT">>,
            handler => 'swag_server_parties_handler'
        },
        'GetMyParty' => #{
            path => "/v2/processing/me",
            method => <<"GET">>,
            handler => 'swag_server_parties_handler'
        },
        'GetPartyByID' => #{
            path => "/v2/processing/parties/:partyID",
            method => <<"GET">>,
            handler => 'swag_server_parties_handler'
        },
        'SuspendMyParty' => #{
            path => "/v2/processing/me/suspend",
            method => <<"PUT">>,
            handler => 'swag_server_parties_handler'
        },
        'SuspendPartyByID' => #{
            path => "/v2/processing/parties/:partyID/suspend",
            method => <<"PUT">>,
            handler => 'swag_server_parties_handler'
        },
        'GetPaymentInstitutionByRef' => #{
            path => "/v2/processing/payment-institutions/:paymentInstitutionID",
            method => <<"GET">>,
            handler => 'swag_server_payment_institutions_handler'
        },
        'GetPaymentInstitutionPaymentTerms' => #{
            path => "/v2/processing/payment-institutions/:paymentInstitutionID/terms/payments",
            method => <<"GET">>,
            handler => 'swag_server_payment_institutions_handler'
        },
        'GetPaymentInstitutionPayoutMethods' => #{
            path => "/v2/processing/payment-institutions/:paymentInstitutionID/terms/payouts/methods",
            method => <<"GET">>,
            handler => 'swag_server_payment_institutions_handler'
        },
        'GetPaymentInstitutionPayoutSchedules' => #{
            path => "/v2/processing/payment-institutions/:paymentInstitutionID/terms/payouts/schedules",
            method => <<"GET">>,
            handler => 'swag_server_payment_institutions_handler'
        },
        'GetPaymentInstitutions' => #{
            path => "/v2/processing/payment-institutions",
            method => <<"GET">>,
            handler => 'swag_server_payment_institutions_handler'
        },
        'GetServiceProviderByID' => #{
            path => "/v2/processing/service-providers/:serviceProviderID",
            method => <<"GET">>,
            handler => 'swag_server_payment_institutions_handler'
        },
        'CancelPayment' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/cancel",
            method => <<"POST">>,
            handler => 'swag_server_payments_handler'
        },
        'CapturePayment' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/capture",
            method => <<"POST">>,
            handler => 'swag_server_payments_handler'
        },
        'CreatePayment' => #{
            path => "/v2/processing/invoices/:invoiceID/payments",
            method => <<"POST">>,
            handler => 'swag_server_payments_handler'
        },
        'CreateRefund' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/refunds",
            method => <<"POST">>,
            handler => 'swag_server_payments_handler'
        },
        'GetChargebackByID' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/chargebacks/:chargebackID",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetChargebacks' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/chargebacks",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetPaymentByExternalID' => #{
            path => "/v2/processing/payments",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetPaymentByID' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetPayments' => #{
            path => "/v2/processing/invoices/:invoiceID/payments",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetRefundByExternalID' => #{
            path => "/v2/processing/refunds",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetRefundByID' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/refunds/:refundID",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'GetRefunds' => #{
            path => "/v2/processing/invoices/:invoiceID/payments/:paymentID/refunds",
            method => <<"GET">>,
            handler => 'swag_server_payments_handler'
        },
        'CreatePayout' => #{
            path => "/v2/processing/payouts",
            method => <<"POST">>,
            handler => 'swag_server_payouts_handler'
        },
        'GetPayout' => #{
            path => "/v2/processing/payouts/:payoutID",
            method => <<"GET">>,
            handler => 'swag_server_payouts_handler'
        },
        'GetPayoutToolByID' => #{
            path => "/v2/processing/contracts/:contractID/payout_tools/:payoutToolID",
            method => <<"GET">>,
            handler => 'swag_server_payouts_handler'
        },
        'GetPayoutToolByIDForParty' => #{
            path => "/v2/processing/parties/:partyID/contracts/:contractID/payout_tools/:payoutToolID",
            method => <<"GET">>,
            handler => 'swag_server_payouts_handler'
        },
        'GetPayoutTools' => #{
            path => "/v2/processing/contracts/:contractID/payout_tools",
            method => <<"GET">>,
            handler => 'swag_server_payouts_handler'
        },
        'GetPayoutToolsForParty' => #{
            path => "/v2/processing/parties/:partyID/contracts/:contractID/payout_tools",
            method => <<"GET">>,
            handler => 'swag_server_payouts_handler'
        },
        'GetScheduleByRef' => #{
            path => "/v2/processing/schedules/:scheduleID",
            method => <<"GET">>,
            handler => 'swag_server_payouts_handler'
        },
        'CreateReport' => #{
            path => "/v2/shops/:shopID/reports",
            method => <<"POST">>,
            handler => 'swag_server_reports_handler'
        },
        'CreateReportForParty' => #{
            path => "/v2/parties/:partyID/shops/:shopID/reports",
            method => <<"POST">>,
            handler => 'swag_server_reports_handler'
        },
        'DownloadFile' => #{
            path => "/v2/shops/:shopID/reports/:reportID/files/:fileID/download",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'DownloadFileForParty' => #{
            path => "/v2/parties/:partyID/shops/:shopID/reports/:reportID/files/:fileID/download",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'GetReport' => #{
            path => "/v2/shops/:shopID/reports/:reportID",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'GetReportForParty' => #{
            path => "/v2/parties/:partyID/shops/:shopID/reports/:reportID",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'GetReports' => #{
            path => "/v2/shops/:shopID/reports",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'GetReportsForParty' => #{
            path => "/v2/parties/:partyID/shops/:shopID/reports",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'SearchInvoices' => #{
            path => "/v2/analytics/shops/:shopID/invoices",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchPayments' => #{
            path => "/v2/analytics/shops/:shopID/payments",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchPayouts' => #{
            path => "/v2/analytics/shops/:shopID/payouts",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchRefunds' => #{
            path => "/v2/analytics/shops/:shopID/refunds",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'ActivateShop' => #{
            path => "/v2/processing/shops/:shopID/activate",
            method => <<"PUT">>,
            handler => 'swag_server_shops_handler'
        },
        'ActivateShopForParty' => #{
            path => "/v2/processing/parties/:partyID/shops/:shopID/activate",
            method => <<"PUT">>,
            handler => 'swag_server_shops_handler'
        },
        'GetShopByID' => #{
            path => "/v2/processing/shops/:shopID",
            method => <<"GET">>,
            handler => 'swag_server_shops_handler'
        },
        'GetShopByIDForParty' => #{
            path => "/v2/processing/parties/:partyID/shops/:shopID",
            method => <<"GET">>,
            handler => 'swag_server_shops_handler'
        },
        'GetShops' => #{
            path => "/v2/processing/shops",
            method => <<"GET">>,
            handler => 'swag_server_shops_handler'
        },
        'GetShopsForParty' => #{
            path => "/v2/processing/parties/:partyID/shops",
            method => <<"GET">>,
            handler => 'swag_server_shops_handler'
        },
        'SuspendShop' => #{
            path => "/v2/processing/shops/:shopID/suspend",
            method => <<"PUT">>,
            handler => 'swag_server_shops_handler'
        },
        'SuspendShopForParty' => #{
            path => "/v2/processing/parties/:partyID/shops/:shopID/suspend",
            method => <<"PUT">>,
            handler => 'swag_server_shops_handler'
        },
        'CreatePaymentResource' => #{
            path => "/v2/processing/payment-resources",
            method => <<"POST">>,
            handler => 'swag_server_tokens_handler'
        },
        'GetTradeBlocByID' => #{
            path => "/v2/processing/tradeblocs/:tradeBlocID",
            method => <<"GET">>,
            handler => 'swag_server_trade_blocs_handler'
        },
        'GetTradeBlocs' => #{
            path => "/v2/processing/tradeblocs",
            method => <<"GET">>,
            handler => 'swag_server_trade_blocs_handler'
        },
        'CreateWebhook' => #{
            path => "/v2/processing/webhooks",
            method => <<"POST">>,
            handler => 'swag_server_webhooks_handler'
        },
        'DeleteWebhookByID' => #{
            path => "/v2/processing/webhooks/:webhookID",
            method => <<"DELETE">>,
            handler => 'swag_server_webhooks_handler'
        },
        'GetWebhookByID' => #{
            path => "/v2/processing/webhooks/:webhookID",
            method => <<"GET">>,
            handler => 'swag_server_webhooks_handler'
        },
        'GetWebhooks' => #{
            path => "/v2/processing/webhooks",
            method => <<"GET">>,
            handler => 'swag_server_webhooks_handler'
        }
    }.
