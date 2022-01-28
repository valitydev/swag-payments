%% -*- mode: erlang -*-
-module(swagger_tokens_api).

%% generated methods

-export([create_payment_resource/2]).
-export([create_payment_resource/3]).


-spec create_payment_resource(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_payment_resource(Endpoint, Params) ->
    create_payment_resource(Endpoint, Params, []).

-spec create_payment_resource(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_payment_resource(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        post,
        swagger_utils:get_url(Endpoint, "/v2/processing/payment-resources"),
        Params,
        get_request_spec(create_payment_resource),
        Opts
    ), create_payment_resource).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swagger_procession:process_response(
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


-spec get_request_spec(OperationID :: swagger:operation_id()) ->
    Spec :: swagger_procession:request_spec() | no_return().


get_request_spec('create_payment_resource') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'PaymentResourceParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swagger:operation_id(), Code :: swagger_procession:code()) ->
    Spec :: swagger_procession:response_spec() | no_return().


get_response_spec('create_payment_resource', 201) ->
    {'PaymentResourceResult', 'PaymentResourceResult'};

get_response_spec('create_payment_resource', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('create_payment_resource', 401) ->
    undefined;

get_response_spec('create_payment_resource', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
