%% -*- mode: erlang -*-
-module(swag_client_payments_api).

%% generated methods

-export([cancel_payment/2]).
-export([cancel_payment/3]).

-export([capture_payment/2]).
-export([capture_payment/3]).

-export([create_payment/2]).
-export([create_payment/3]).

-export([create_refund/2]).
-export([create_refund/3]).

-export([get_chargeback_by_id/2]).
-export([get_chargeback_by_id/3]).

-export([get_chargebacks/2]).
-export([get_chargebacks/3]).

-export([get_payment_by_external_id/2]).
-export([get_payment_by_external_id/3]).

-export([get_payment_by_id/2]).
-export([get_payment_by_id/3]).

-export([get_payments/2]).
-export([get_payments/3]).

-export([get_refund_by_external_id/2]).
-export([get_refund_by_external_id/3]).

-export([get_refund_by_id/2]).
-export([get_refund_by_id/3]).

-export([get_refunds/2]).
-export([get_refunds/3]).


-spec cancel_payment(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
cancel_payment(Endpoint, Params) ->
    cancel_payment(Endpoint, Params, []).

-spec cancel_payment(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
cancel_payment(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/cancel"),
        Params,
        get_request_spec(cancel_payment),
        Opts
    ), cancel_payment).

-spec capture_payment(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
capture_payment(Endpoint, Params) ->
    capture_payment(Endpoint, Params, []).

-spec capture_payment(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
capture_payment(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/capture"),
        Params,
        get_request_spec(capture_payment),
        Opts
    ), capture_payment).

-spec create_payment(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_payment(Endpoint, Params) ->
    create_payment(Endpoint, Params, []).

-spec create_payment(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_payment(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments"),
        Params,
        get_request_spec(create_payment),
        Opts
    ), create_payment).

-spec create_refund(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_refund(Endpoint, Params) ->
    create_refund(Endpoint, Params, []).

-spec create_refund(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_refund(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/refunds"),
        Params,
        get_request_spec(create_refund),
        Opts
    ), create_refund).

-spec get_chargeback_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_chargeback_by_id(Endpoint, Params) ->
    get_chargeback_by_id(Endpoint, Params, []).

-spec get_chargeback_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_chargeback_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/chargebacks/:chargebackID"),
        Params,
        get_request_spec(get_chargeback_by_id),
        Opts
    ), get_chargeback_by_id).

-spec get_chargebacks(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_chargebacks(Endpoint, Params) ->
    get_chargebacks(Endpoint, Params, []).

-spec get_chargebacks(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_chargebacks(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/chargebacks"),
        Params,
        get_request_spec(get_chargebacks),
        Opts
    ), get_chargebacks).

-spec get_payment_by_external_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_by_external_id(Endpoint, Params) ->
    get_payment_by_external_id(Endpoint, Params, []).

-spec get_payment_by_external_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_by_external_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payments"),
        Params,
        get_request_spec(get_payment_by_external_id),
        Opts
    ), get_payment_by_external_id).

-spec get_payment_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_by_id(Endpoint, Params) ->
    get_payment_by_id(Endpoint, Params, []).

-spec get_payment_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID"),
        Params,
        get_request_spec(get_payment_by_id),
        Opts
    ), get_payment_by_id).

-spec get_payments(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments(Endpoint, Params) ->
    get_payments(Endpoint, Params, []).

-spec get_payments(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments"),
        Params,
        get_request_spec(get_payments),
        Opts
    ), get_payments).

-spec get_refund_by_external_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refund_by_external_id(Endpoint, Params) ->
    get_refund_by_external_id(Endpoint, Params, []).

-spec get_refund_by_external_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refund_by_external_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/refunds"),
        Params,
        get_request_spec(get_refund_by_external_id),
        Opts
    ), get_refund_by_external_id).

-spec get_refund_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refund_by_id(Endpoint, Params) ->
    get_refund_by_id(Endpoint, Params, []).

-spec get_refund_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refund_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/refunds/:refundID"),
        Params,
        get_request_spec(get_refund_by_id),
        Opts
    ), get_refund_by_id).

-spec get_refunds(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refunds(Endpoint, Params) ->
    get_refunds(Endpoint, Params, []).

-spec get_refunds(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refunds(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payments/:paymentID/refunds"),
        Params,
        get_request_spec(get_refunds),
        Opts
    ), get_refunds).

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


get_request_spec('cancel_payment') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'Reason', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('capture_payment') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'CaptureParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_payment') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'PaymentParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_refund') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'RefundParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_chargeback_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'chargebackID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_chargebacks') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_by_external_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_refund_by_external_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_refund_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'refundID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_refunds') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('cancel_payment', 202) ->
    undefined;

get_response_spec('cancel_payment', 400) ->
    {'inline_response_400_9', 'inline_response_400_9'};

get_response_spec('cancel_payment', 401) ->
    undefined;

get_response_spec('cancel_payment', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('capture_payment', 202) ->
    undefined;

get_response_spec('capture_payment', 400) ->
    {'inline_response_400_10', 'inline_response_400_10'};

get_response_spec('capture_payment', 401) ->
    undefined;

get_response_spec('capture_payment', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('create_payment', 201) ->
    {'Payment', 'Payment'};

get_response_spec('create_payment', 400) ->
    {'inline_response_400_8', 'inline_response_400_8'};

get_response_spec('create_payment', 401) ->
    undefined;

get_response_spec('create_payment', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('create_payment', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec('create_refund', 201) ->
    {'Refund', 'Refund'};

get_response_spec('create_refund', 400) ->
    {'inline_response_400_11', 'inline_response_400_11'};

get_response_spec('create_refund', 401) ->
    undefined;

get_response_spec('create_refund', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('create_refund', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec('get_chargeback_by_id', 200) ->
    {'Chargeback', 'Chargeback'};

get_response_spec('get_chargeback_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_chargeback_by_id', 401) ->
    undefined;

get_response_spec('get_chargeback_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_chargebacks', 200) ->
    {'list', 'Chargeback'};

get_response_spec('get_chargebacks', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_chargebacks', 401) ->
    undefined;

get_response_spec('get_chargebacks', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_by_external_id', 200) ->
    {'Payment', 'Payment'};

get_response_spec('get_payment_by_external_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_by_external_id', 401) ->
    undefined;

get_response_spec('get_payment_by_external_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_by_id', 200) ->
    {'Payment', 'Payment'};

get_response_spec('get_payment_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_by_id', 401) ->
    undefined;

get_response_spec('get_payment_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments', 200) ->
    {'list', 'Payment'};

get_response_spec('get_payments', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments', 401) ->
    undefined;

get_response_spec('get_payments', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_refund_by_external_id', 200) ->
    {'Refund', 'Refund'};

get_response_spec('get_refund_by_external_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_refund_by_external_id', 401) ->
    undefined;

get_response_spec('get_refund_by_external_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_refund_by_id', 200) ->
    {'Refund', 'Refund'};

get_response_spec('get_refund_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_refund_by_id', 401) ->
    undefined;

get_response_spec('get_refund_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_refunds', 200) ->
    {'list', 'Refund'};

get_response_spec('get_refunds', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_refunds', 401) ->
    undefined;

get_response_spec('get_refunds', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
