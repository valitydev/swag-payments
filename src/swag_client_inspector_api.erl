%% -*- mode: erlang -*-
-module(swag_client_inspector_api).

%% generated methods

-export([inspect_user/2]).
-export([inspect_user/3]).


-spec inspect_user(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
inspect_user(Endpoint, Params) ->
    inspect_user(Endpoint, Params, []).

-spec inspect_user(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
inspect_user(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/antifraud/inspectUser"),
        Params,
        get_request_spec(inspect_user),
        Opts
    ), inspect_user).

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


get_request_spec('inspect_user') ->
    [
        {'UserInspectRequest', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('inspect_user', 200) ->
    {'inline_response_200', 'inline_response_200'};

get_response_spec('inspect_user', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('inspect_user', 401) ->
    undefined;

get_response_spec('inspect_user', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
