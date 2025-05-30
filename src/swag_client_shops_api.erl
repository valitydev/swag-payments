%% -*- mode: erlang -*-
-module(swag_client_shops_api).

%% generated methods

-export([activate_shop_for_party/2]).
-export([activate_shop_for_party/3]).

-export([get_shop_by_id_for_party/2]).
-export([get_shop_by_id_for_party/3]).

-export([get_shops_for_party/2]).
-export([get_shops_for_party/3]).

-export([suspend_shop_for_party/2]).
-export([suspend_shop_for_party/3]).


-spec activate_shop_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_shop_for_party(Endpoint, Params) ->
    activate_shop_for_party(Endpoint, Params, []).

-spec activate_shop_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_shop_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        put,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops/:shopID/activate"),
        Params,
        get_request_spec(activate_shop_for_party),
        Opts
    ), activate_shop_for_party).

-spec get_shop_by_id_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shop_by_id_for_party(Endpoint, Params) ->
    get_shop_by_id_for_party(Endpoint, Params, []).

-spec get_shop_by_id_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shop_by_id_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops/:shopID"),
        Params,
        get_request_spec(get_shop_by_id_for_party),
        Opts
    ), get_shop_by_id_for_party).

-spec get_shops_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shops_for_party(Endpoint, Params) ->
    get_shops_for_party(Endpoint, Params, []).

-spec get_shops_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shops_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops"),
        Params,
        get_request_spec(get_shops_for_party),
        Opts
    ), get_shops_for_party).

-spec suspend_shop_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_shop_for_party(Endpoint, Params) ->
    suspend_shop_for_party(Endpoint, Params, []).

-spec suspend_shop_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_shop_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        put,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops/:shopID/suspend"),
        Params,
        get_request_spec(suspend_shop_for_party),
        Opts
    ), suspend_shop_for_party).

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


get_request_spec('activate_shop_for_party') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shopID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
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
get_request_spec('get_shop_by_id_for_party') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shopID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
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
get_request_spec('get_shops_for_party') ->
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
get_request_spec('suspend_shop_for_party') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shopID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
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


get_response_spec('activate_shop_for_party', 204) ->
    undefined;

get_response_spec('activate_shop_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('activate_shop_for_party', 401) ->
    undefined;

get_response_spec('activate_shop_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_shop_by_id_for_party', 200) ->
    {'Shop', 'Shop'};

get_response_spec('get_shop_by_id_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_shop_by_id_for_party', 401) ->
    undefined;

get_response_spec('get_shop_by_id_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_shops_for_party', 200) ->
    {'list', 'Shop'};

get_response_spec('get_shops_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_shops_for_party', 401) ->
    undefined;

get_response_spec('get_shops_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('suspend_shop_for_party', 204) ->
    undefined;

get_response_spec('suspend_shop_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('suspend_shop_for_party', 401) ->
    undefined;

get_response_spec('suspend_shop_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
