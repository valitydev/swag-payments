%% -*- mode: erlang -*-
-module(swag_client_webhooks_api).

%% generated methods

-export([create_webhook/2]).
-export([create_webhook/3]).

-export([delete_webhook_by_id/2]).
-export([delete_webhook_by_id/3]).

-export([get_webhook_by_id/2]).
-export([get_webhook_by_id/3]).

-export([get_webhooks/2]).
-export([get_webhooks/3]).

-export([get_webhooks_for_party/2]).
-export([get_webhooks_for_party/3]).


-spec create_webhook(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_webhook(Endpoint, Params) ->
    create_webhook(Endpoint, Params, []).

-spec create_webhook(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_webhook(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/webhooks"),
        Params,
        get_request_spec(create_webhook),
        Opts
    ), create_webhook).

-spec delete_webhook_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
delete_webhook_by_id(Endpoint, Params) ->
    delete_webhook_by_id(Endpoint, Params, []).

-spec delete_webhook_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
delete_webhook_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        delete,
        swag_client_utils:get_url(Endpoint, "/v2/processing/webhooks/:webhookID"),
        Params,
        get_request_spec(delete_webhook_by_id),
        Opts
    ), delete_webhook_by_id).

-spec get_webhook_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_webhook_by_id(Endpoint, Params) ->
    get_webhook_by_id(Endpoint, Params, []).

-spec get_webhook_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_webhook_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/webhooks/:webhookID"),
        Params,
        get_request_spec(get_webhook_by_id),
        Opts
    ), get_webhook_by_id).

-spec get_webhooks(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_webhooks(Endpoint, Params) ->
    get_webhooks(Endpoint, Params, []).

-spec get_webhooks(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_webhooks(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/webhooks"),
        Params,
        get_request_spec(get_webhooks),
        Opts
    ), get_webhooks).

-spec get_webhooks_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_webhooks_for_party(Endpoint, Params) ->
    get_webhooks_for_party(Endpoint, Params, []).

-spec get_webhooks_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_webhooks_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/webhooks"),
        Params,
        get_request_spec(get_webhooks_for_party),
        Opts
    ), get_webhooks_for_party).

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


get_request_spec('create_webhook') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'Webhook', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('delete_webhook_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'webhookID', #{
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
get_request_spec('get_webhook_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'webhookID', #{
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
get_request_spec('get_webhooks') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_webhooks_for_party') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyID', #{
            source => binding,
            rules  => [{type, 'binary'}, true
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


get_response_spec('create_webhook', 201) ->
    {'Webhook', 'Webhook'};

get_response_spec('create_webhook', 400) ->
    {'inline_response_400_13', 'inline_response_400_13'};

get_response_spec('create_webhook', 401) ->
    undefined;

get_response_spec('create_webhook', 429) ->
    {'inline_response_429', 'inline_response_429'};

get_response_spec('delete_webhook_by_id', 204) ->
    undefined;

get_response_spec('delete_webhook_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('delete_webhook_by_id', 401) ->
    undefined;

get_response_spec('delete_webhook_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_webhook_by_id', 200) ->
    {'Webhook', 'Webhook'};

get_response_spec('get_webhook_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_webhook_by_id', 401) ->
    undefined;

get_response_spec('get_webhook_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_webhooks', 200) ->
    {'list', 'Webhook'};

get_response_spec('get_webhooks', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_webhooks', 401) ->
    undefined;

get_response_spec('get_webhooks_for_party', 200) ->
    {'list', 'Webhook'};

get_response_spec('get_webhooks_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_webhooks_for_party', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
