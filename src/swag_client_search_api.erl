%% -*- mode: erlang -*-
-module(swag_client_search_api).

%% generated methods

-export([search_invoices/2]).
-export([search_invoices/3]).

-export([search_payments/2]).
-export([search_payments/3]).

-export([search_refunds/2]).
-export([search_refunds/3]).


-spec search_invoices(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_invoices(Endpoint, Params) ->
    search_invoices(Endpoint, Params, []).

-spec search_invoices(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_invoices(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/invoices"),
        Params,
        get_request_spec(search_invoices),
        Opts
    ), search_invoices).

-spec search_payments(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_payments(Endpoint, Params) ->
    search_payments(Endpoint, Params, []).

-spec search_payments(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_payments(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/payments"),
        Params,
        get_request_spec(search_payments),
        Opts
    ), search_payments).

-spec search_refunds(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_refunds(Endpoint, Params) ->
    search_refunds(Endpoint, Params, []).

-spec search_refunds(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_refunds(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/refunds"),
        Params,
        get_request_spec(search_refunds),
        Opts
    ), search_refunds).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: swag_client:operation_id()) ->
    Spec :: swag_client_procession:request_spec() | no_return().


get_request_spec('search_invoices') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shopID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'invoiceStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['unpaid', 'cancelled', 'paid', 'fulfilled']}, true
, {required, false}]
        }},
        {'paymentStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['pending', 'processed', 'captured', 'cancelled', 'refunded', 'failed']}, true
, {required, false}]
        }},
        {'paymentFlow', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['instant', 'hold']}, true
, {required, false}]
        }},
        {'paymentMethod', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['bankCard', 'paymentTerminal']}, true
, {required, false}]
        }},
        {'paymentTerminalProvider', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'payerEmail', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'email'}, {max_length, 100}, true
, {required, false}]
        }},
        {'payerIP', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'ip-address'}, {max_length, 45}, true
, {required, false}]
        }},
        {'payerFingerprint', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 1000}, true
, {required, false}]
        }},
        {'customerID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'bankCardTokenProvider', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }},
        {'bankCardPaymentSystem', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }},
        {'first6', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^\\d{6}$"}, true
, {required, false}]
        }},
        {'last4', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^\\d{0,4}$"}, true
, {required, false}]
        }},
        {'rrn', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[a-zA-Z0-9]{12}$"}, true
, {required, false}]
        }},
        {'paymentAmount', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'invoiceAmount', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('search_payments') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shopID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['pending', 'processed', 'captured', 'cancelled', 'refunded', 'failed']}, true
, {required, false}]
        }},
        {'paymentFlow', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['instant', 'hold']}, true
, {required, false}]
        }},
        {'paymentMethod', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['bankCard', 'paymentTerminal']}, true
, {required, false}]
        }},
        {'paymentTerminalProvider', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'payerEmail', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'email'}, {max_length, 100}, true
, {required, false}]
        }},
        {'payerIP', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'ip-address'}, {max_length, 45}, true
, {required, false}]
        }},
        {'payerFingerprint', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 1000}, true
, {required, false}]
        }},
        {'customerID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'first6', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^\\d{6}$"}, true
, {required, false}]
        }},
        {'last4', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^\\d{0,4}$"}, true
, {required, false}]
        }},
        {'rrn', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[a-zA-Z0-9]{12}$"}, true
, {required, false}]
        }},
        {'approvalCode', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'bankCardTokenProvider', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }},
        {'bankCardPaymentSystem', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }},
        {'paymentAmount', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('search_refunds') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shopID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'offset', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {min, 0, inclusive}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'refundID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'rrn', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[a-zA-Z0-9]{12}$"}, true
, {required, false}]
        }},
        {'approvalCode', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'refundStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['pending', 'succeeded', 'failed']}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('search_invoices', 200) ->
    {'inline_response_200', 'inline_response_200'};

get_response_spec('search_invoices', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_invoices', 401) ->
    undefined;

get_response_spec('search_invoices', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_payments', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('search_payments', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_payments', 401) ->
    undefined;

get_response_spec('search_payments', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_refunds', 200) ->
    {'inline_response_200_2', 'inline_response_200_2'};

get_response_spec('search_refunds', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_refunds', 401) ->
    undefined;

get_response_spec('search_refunds', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
