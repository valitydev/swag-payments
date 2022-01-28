%% -*- mode: erlang -*-
-module(swagger_claims_api).

%% generated methods

-export([create_claim/2]).
-export([create_claim/3]).

-export([get_claim_by_id/2]).
-export([get_claim_by_id/3]).

-export([get_claims/2]).
-export([get_claims/3]).

-export([revoke_claim_by_id/2]).
-export([revoke_claim_by_id/3]).


-spec create_claim(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_claim(Endpoint, Params) ->
    create_claim(Endpoint, Params, []).

-spec create_claim(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_claim(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        post,
        swagger_utils:get_url(Endpoint, "/v2/processing/claims"),
        Params,
        get_request_spec(create_claim),
        Opts
    ), create_claim).

-spec get_claim_by_id(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_claim_by_id(Endpoint, Params) ->
    get_claim_by_id(Endpoint, Params, []).

-spec get_claim_by_id(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_claim_by_id(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/claims/:claimID"),
        Params,
        get_request_spec(get_claim_by_id),
        Opts
    ), get_claim_by_id).

-spec get_claims(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_claims(Endpoint, Params) ->
    get_claims(Endpoint, Params, []).

-spec get_claims(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_claims(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/claims"),
        Params,
        get_request_spec(get_claims),
        Opts
    ), get_claims).

-spec revoke_claim_by_id(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
revoke_claim_by_id(Endpoint, Params) ->
    revoke_claim_by_id(Endpoint, Params, []).

-spec revoke_claim_by_id(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
revoke_claim_by_id(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        put,
        swagger_utils:get_url(Endpoint, "/v2/processing/claims/:claimID/revoke"),
        Params,
        get_request_spec(revoke_claim_by_id),
        Opts
    ), revoke_claim_by_id).

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


get_request_spec('create_claim') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'ClaimChangeset', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_claim_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'claimID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_claims') ->
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
        }},
        {'claimStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['pending', 'accepted', 'denied', 'revoked']}, true
, {required, false}]
        }}
    ];
get_request_spec('revoke_claim_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'claimID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'claimRevision', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'Reason', #{
            source => body,
            rules  => [schema, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swagger:operation_id(), Code :: swagger_procession:code()) ->
    Spec :: swagger_procession:response_spec() | no_return().


get_response_spec('create_claim', 201) ->
    {'Claim', 'Claim'};

get_response_spec('create_claim', 400) ->
    {'inline_response_400_2', 'inline_response_400_2'};

get_response_spec('create_claim', 401) ->
    undefined;

get_response_spec('get_claim_by_id', 200) ->
    {'Claim', 'Claim'};

get_response_spec('get_claim_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_claim_by_id', 401) ->
    undefined;

get_response_spec('get_claim_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_claims', 200) ->
    {'list', 'Claim'};

get_response_spec('get_claims', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_claims', 401) ->
    undefined;

get_response_spec('revoke_claim_by_id', 204) ->
    undefined;

get_response_spec('revoke_claim_by_id', 400) ->
    {'inline_response_400_3', 'inline_response_400_3'};

get_response_spec('revoke_claim_by_id', 401) ->
    undefined;

get_response_spec('revoke_claim_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
