%% -*- mode: erlang -*-
-module(swag_client_customers_api).

%% generated methods

-export([create_binding/2]).
-export([create_binding/3]).

-export([create_customer/2]).
-export([create_customer/3]).

-export([create_customer_access_token/2]).
-export([create_customer_access_token/3]).

-export([delete_customer/2]).
-export([delete_customer/3]).

-export([get_binding/2]).
-export([get_binding/3]).

-export([get_bindings/2]).
-export([get_bindings/3]).

-export([get_customer_by_id/2]).
-export([get_customer_by_id/3]).

-export([get_customer_events/2]).
-export([get_customer_events/3]).

-export([get_customer_payment_methods/2]).
-export([get_customer_payment_methods/3]).


-spec create_binding(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_binding(Endpoint, Params) ->
    create_binding(Endpoint, Params, []).

-spec create_binding(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_binding(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID/bindings"),
        Params,
        get_request_spec(create_binding),
        Opts
    ), create_binding).

-spec create_customer(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_customer(Endpoint, Params) ->
    create_customer(Endpoint, Params, []).

-spec create_customer(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_customer(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers"),
        Params,
        get_request_spec(create_customer),
        Opts
    ), create_customer).

-spec create_customer_access_token(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_customer_access_token(Endpoint, Params) ->
    create_customer_access_token(Endpoint, Params, []).

-spec create_customer_access_token(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_customer_access_token(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID/access-tokens"),
        Params,
        get_request_spec(create_customer_access_token),
        Opts
    ), create_customer_access_token).

-spec delete_customer(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
delete_customer(Endpoint, Params) ->
    delete_customer(Endpoint, Params, []).

-spec delete_customer(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
delete_customer(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        delete,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID"),
        Params,
        get_request_spec(delete_customer),
        Opts
    ), delete_customer).

-spec get_binding(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_binding(Endpoint, Params) ->
    get_binding(Endpoint, Params, []).

-spec get_binding(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_binding(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID/bindings/:customerBindingID"),
        Params,
        get_request_spec(get_binding),
        Opts
    ), get_binding).

-spec get_bindings(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_bindings(Endpoint, Params) ->
    get_bindings(Endpoint, Params, []).

-spec get_bindings(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_bindings(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID/bindings"),
        Params,
        get_request_spec(get_bindings),
        Opts
    ), get_bindings).

-spec get_customer_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_customer_by_id(Endpoint, Params) ->
    get_customer_by_id(Endpoint, Params, []).

-spec get_customer_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_customer_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID"),
        Params,
        get_request_spec(get_customer_by_id),
        Opts
    ), get_customer_by_id).

-spec get_customer_events(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_customer_events(Endpoint, Params) ->
    get_customer_events(Endpoint, Params, []).

-spec get_customer_events(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_customer_events(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID/events"),
        Params,
        get_request_spec(get_customer_events),
        Opts
    ), get_customer_events).

-spec get_customer_payment_methods(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_customer_payment_methods(Endpoint, Params) ->
    get_customer_payment_methods(Endpoint, Params, []).

-spec get_customer_payment_methods(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_customer_payment_methods(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/customers/:customerID/payment-methods"),
        Params,
        get_request_spec(get_customer_payment_methods),
        Opts
    ), get_customer_payment_methods).

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


get_request_spec('create_binding') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'CustomerBindingParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_customer') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'CustomerParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_customer_access_token') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
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
get_request_spec('delete_customer') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
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
get_request_spec('get_binding') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerBindingID', #{
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
get_request_spec('get_bindings') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
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
get_request_spec('get_customer_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
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
get_request_spec('get_customer_events') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
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
get_request_spec('get_customer_payment_methods') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'customerID', #{
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


get_response_spec('create_binding', 201) ->
    {'CustomerBinding', 'CustomerBinding'};

get_response_spec('create_binding', 400) ->
    {'inline_response_400_2', 'inline_response_400_2'};

get_response_spec('create_binding', 401) ->
    undefined;

get_response_spec('create_binding', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('create_binding', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec('create_customer', 201) ->
    {'CustomerAndToken', 'CustomerAndToken'};

get_response_spec('create_customer', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('create_customer', 401) ->
    undefined;

get_response_spec('create_customer', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec('create_customer_access_token', 201) ->
    {'AccessToken', 'AccessToken'};

get_response_spec('create_customer_access_token', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('create_customer_access_token', 401) ->
    undefined;

get_response_spec('create_customer_access_token', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('delete_customer', 204) ->
    undefined;

get_response_spec('delete_customer', 400) ->
    {'inline_response_400_1', 'inline_response_400_1'};

get_response_spec('delete_customer', 401) ->
    undefined;

get_response_spec('delete_customer', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_binding', 200) ->
    {'CustomerBinding', 'CustomerBinding'};

get_response_spec('get_binding', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_binding', 401) ->
    undefined;

get_response_spec('get_binding', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_bindings', 200) ->
    {'list', 'CustomerBinding'};

get_response_spec('get_bindings', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_bindings', 401) ->
    undefined;

get_response_spec('get_bindings', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_customer_by_id', 200) ->
    {'Customer', 'Customer'};

get_response_spec('get_customer_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_customer_by_id', 401) ->
    undefined;

get_response_spec('get_customer_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_customer_events', 200) ->
    {'list', 'CustomerEvent'};

get_response_spec('get_customer_events', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_customer_events', 401) ->
    undefined;

get_response_spec('get_customer_events', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_customer_payment_methods', 200) ->
    {'list', 'PaymentMethod'};

get_response_spec('get_customer_payment_methods', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_customer_payment_methods', 401) ->
    undefined;

get_response_spec('get_customer_payment_methods', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
