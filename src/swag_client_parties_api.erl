%% -*- mode: erlang -*-
-module(swag_client_parties_api).

%% generated methods

-export([activate_party_by_id/2]).
-export([activate_party_by_id/3]).

-export([get_party_by_id/2]).
-export([get_party_by_id/3]).

-export([suspend_party_by_id/2]).
-export([suspend_party_by_id/3]).


-spec activate_party_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_party_by_id(Endpoint, Params) ->
    activate_party_by_id(Endpoint, Params, []).

-spec activate_party_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_party_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        put,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/activate"),
        Params,
        get_request_spec(activate_party_by_id),
        Opts
    ), activate_party_by_id).

-spec get_party_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_party_by_id(Endpoint, Params) ->
    get_party_by_id(Endpoint, Params, []).

-spec get_party_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_party_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID"),
        Params,
        get_request_spec(get_party_by_id),
        Opts
    ), get_party_by_id).

-spec suspend_party_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_party_by_id(Endpoint, Params) ->
    suspend_party_by_id(Endpoint, Params, []).

-spec suspend_party_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_party_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        put,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/suspend"),
        Params,
        get_request_spec(suspend_party_by_id),
        Opts
    ), suspend_party_by_id).

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


get_request_spec('activate_party_by_id') ->
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
    ];
get_request_spec('get_party_by_id') ->
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
    ];
get_request_spec('suspend_party_by_id') ->
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


get_response_spec('activate_party_by_id', 204) ->
    undefined;

get_response_spec('activate_party_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('activate_party_by_id', 401) ->
    undefined;

get_response_spec('activate_party_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_party_by_id', 200) ->
    {'Party', 'Party'};

get_response_spec('get_party_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_party_by_id', 401) ->
    undefined;

get_response_spec('get_party_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('suspend_party_by_id', 204) ->
    undefined;

get_response_spec('suspend_party_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('suspend_party_by_id', 401) ->
    undefined;

get_response_spec('suspend_party_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
