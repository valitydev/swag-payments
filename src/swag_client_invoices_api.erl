%% -*- mode: erlang -*-
-module(swag_client_invoices_api).

%% generated methods

-export([create_invoice/2]).
-export([create_invoice/3]).

-export([create_invoice_access_token/2]).
-export([create_invoice_access_token/3]).

-export([fulfill_invoice/2]).
-export([fulfill_invoice/3]).

-export([get_invoice_by_external_id/2]).
-export([get_invoice_by_external_id/3]).

-export([get_invoice_by_id/2]).
-export([get_invoice_by_id/3]).

-export([get_invoice_events/2]).
-export([get_invoice_events/3]).

-export([get_invoice_payment_methods/2]).
-export([get_invoice_payment_methods/3]).

-export([rescind_invoice/2]).
-export([rescind_invoice/3]).


-spec create_invoice(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_invoice(Endpoint, Params) ->
    create_invoice(Endpoint, Params, []).

-spec create_invoice(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_invoice(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices"),
        Params,
        get_request_spec(create_invoice),
        Opts
    ), create_invoice).

-spec create_invoice_access_token(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_invoice_access_token(Endpoint, Params) ->
    create_invoice_access_token(Endpoint, Params, []).

-spec create_invoice_access_token(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_invoice_access_token(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/access-tokens"),
        Params,
        get_request_spec(create_invoice_access_token),
        Opts
    ), create_invoice_access_token).

-spec fulfill_invoice(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
fulfill_invoice(Endpoint, Params) ->
    fulfill_invoice(Endpoint, Params, []).

-spec fulfill_invoice(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
fulfill_invoice(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/fulfill"),
        Params,
        get_request_spec(fulfill_invoice),
        Opts
    ), fulfill_invoice).

-spec get_invoice_by_external_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_by_external_id(Endpoint, Params) ->
    get_invoice_by_external_id(Endpoint, Params, []).

-spec get_invoice_by_external_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_by_external_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices"),
        Params,
        get_request_spec(get_invoice_by_external_id),
        Opts
    ), get_invoice_by_external_id).

-spec get_invoice_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_by_id(Endpoint, Params) ->
    get_invoice_by_id(Endpoint, Params, []).

-spec get_invoice_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID"),
        Params,
        get_request_spec(get_invoice_by_id),
        Opts
    ), get_invoice_by_id).

-spec get_invoice_events(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_events(Endpoint, Params) ->
    get_invoice_events(Endpoint, Params, []).

-spec get_invoice_events(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_events(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/events"),
        Params,
        get_request_spec(get_invoice_events),
        Opts
    ), get_invoice_events).

-spec get_invoice_payment_methods(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_payment_methods(Endpoint, Params) ->
    get_invoice_payment_methods(Endpoint, Params, []).

-spec get_invoice_payment_methods(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_invoice_payment_methods(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/payment-methods"),
        Params,
        get_request_spec(get_invoice_payment_methods),
        Opts
    ), get_invoice_payment_methods).

-spec rescind_invoice(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
rescind_invoice(Endpoint, Params) ->
    rescind_invoice(Endpoint, Params, []).

-spec rescind_invoice(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
rescind_invoice(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/invoices/:invoiceID/rescind"),
        Params,
        get_request_spec(rescind_invoice),
        Opts
    ), rescind_invoice).

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


get_request_spec('create_invoice') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'InvoiceParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_invoice_access_token') ->
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
get_request_spec('fulfill_invoice') ->
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
get_request_spec('get_invoice_by_external_id') ->
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
get_request_spec('get_invoice_by_id') ->
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
get_request_spec('get_invoice_events') ->
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
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'eventID', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, false}]
        }}
    ];
get_request_spec('get_invoice_payment_methods') ->
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
get_request_spec('rescind_invoice') ->
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
        {'Reason', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('create_invoice', 201) ->
    {'InvoiceAndToken', 'InvoiceAndToken'};

get_response_spec('create_invoice', 400) ->
    {'inline_response_400_6', 'inline_response_400_6'};

get_response_spec('create_invoice', 401) ->
    undefined;

get_response_spec('create_invoice', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec('create_invoice_access_token', 201) ->
    {'AccessToken', 'AccessToken'};

get_response_spec('create_invoice_access_token', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('create_invoice_access_token', 401) ->
    undefined;

get_response_spec('create_invoice_access_token', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('fulfill_invoice', 204) ->
    undefined;

get_response_spec('fulfill_invoice', 400) ->
    {'inline_response_400_7', 'inline_response_400_7'};

get_response_spec('fulfill_invoice', 401) ->
    undefined;

get_response_spec('fulfill_invoice', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_invoice_by_external_id', 200) ->
    {'Invoice', 'Invoice'};

get_response_spec('get_invoice_by_external_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_invoice_by_external_id', 401) ->
    undefined;

get_response_spec('get_invoice_by_external_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_invoice_by_id', 200) ->
    {'Invoice', 'Invoice'};

get_response_spec('get_invoice_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_invoice_by_id', 401) ->
    undefined;

get_response_spec('get_invoice_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_invoice_events', 200) ->
    {'list', 'InvoiceEvent'};

get_response_spec('get_invoice_events', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_invoice_events', 401) ->
    undefined;

get_response_spec('get_invoice_events', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_invoice_payment_methods', 200) ->
    {'list', 'PaymentMethod'};

get_response_spec('get_invoice_payment_methods', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_invoice_payment_methods', 401) ->
    undefined;

get_response_spec('get_invoice_payment_methods', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('rescind_invoice', 204) ->
    undefined;

get_response_spec('rescind_invoice', 400) ->
    {'inline_response_400_12', 'inline_response_400_12'};

get_response_spec('rescind_invoice', 401) ->
    undefined;

get_response_spec('rescind_invoice', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
