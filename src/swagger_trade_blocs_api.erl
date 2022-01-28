%% -*- mode: erlang -*-
-module(swagger_trade_blocs_api).

%% generated methods

-export([get_trade_bloc_by_id/2]).
-export([get_trade_bloc_by_id/3]).

-export([get_trade_blocs/2]).
-export([get_trade_blocs/3]).


-spec get_trade_bloc_by_id(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_trade_bloc_by_id(Endpoint, Params) ->
    get_trade_bloc_by_id(Endpoint, Params, []).

-spec get_trade_bloc_by_id(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_trade_bloc_by_id(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/tradeblocs/:tradeBlocID"),
        Params,
        get_request_spec(get_trade_bloc_by_id),
        Opts
    ), get_trade_bloc_by_id).

-spec get_trade_blocs(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_trade_blocs(Endpoint, Params) ->
    get_trade_blocs(Endpoint, Params, []).

-spec get_trade_blocs(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_trade_blocs(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/tradeblocs"),
        Params,
        get_request_spec(get_trade_blocs),
        Opts
    ), get_trade_blocs).

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


get_request_spec('get_trade_bloc_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'tradeBlocID', #{
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
get_request_spec('get_trade_blocs') ->
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
    ].

-spec get_response_spec(OperationID :: swagger:operation_id(), Code :: swagger_procession:code()) ->
    Spec :: swagger_procession:response_spec() | no_return().


get_response_spec('get_trade_bloc_by_id', 200) ->
    {'TradeBloc', 'TradeBloc'};

get_response_spec('get_trade_bloc_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_trade_bloc_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_trade_blocs', 200) ->
    {'list', 'TradeBloc'};

get_response_spec('get_trade_blocs', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
