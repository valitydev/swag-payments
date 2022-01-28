%% -*- mode: erlang -*-
-module(swag_client_geo_api).

%% generated methods

-export([get_locations_names/2]).
-export([get_locations_names/3]).


-spec get_locations_names(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_locations_names(Endpoint, Params) ->
    get_locations_names(Endpoint, Params, []).

-spec get_locations_names(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_locations_names(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/reference/geo/location/names"),
        Params,
        get_request_spec(get_locations_names),
        Opts
    ), get_locations_names).

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


get_request_spec('get_locations_names') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'language', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }},
        {'geoIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'integer'}, {format, 'int32'}, true
]}, true
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


get_response_spec('get_locations_names', 200) ->
    {'list', 'LocationName'};

get_response_spec('get_locations_names', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_locations_names', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
