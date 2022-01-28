%% -*- mode: erlang -*-
-module(swagger_shops_api).

%% generated methods

-export([activate_shop/2]).
-export([activate_shop/3]).

-export([activate_shop_for_party/2]).
-export([activate_shop_for_party/3]).

-export([get_shop_by_id/2]).
-export([get_shop_by_id/3]).

-export([get_shop_by_id_for_party/2]).
-export([get_shop_by_id_for_party/3]).

-export([get_shops/2]).
-export([get_shops/3]).

-export([get_shops_for_party/2]).
-export([get_shops_for_party/3]).

-export([suspend_shop/2]).
-export([suspend_shop/3]).

-export([suspend_shop_for_party/2]).
-export([suspend_shop_for_party/3]).


-spec activate_shop(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_shop(Endpoint, Params) ->
    activate_shop(Endpoint, Params, []).

-spec activate_shop(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_shop(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        put,
        swagger_utils:get_url(Endpoint, "/v2/processing/shops/:shopID/activate"),
        Params,
        get_request_spec(activate_shop),
        Opts
    ), activate_shop).

-spec activate_shop_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_shop_for_party(Endpoint, Params) ->
    activate_shop_for_party(Endpoint, Params, []).

-spec activate_shop_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
activate_shop_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        put,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops/:shopID/activate"),
        Params,
        get_request_spec(activate_shop_for_party),
        Opts
    ), activate_shop_for_party).

-spec get_shop_by_id(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shop_by_id(Endpoint, Params) ->
    get_shop_by_id(Endpoint, Params, []).

-spec get_shop_by_id(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shop_by_id(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/shops/:shopID"),
        Params,
        get_request_spec(get_shop_by_id),
        Opts
    ), get_shop_by_id).

-spec get_shop_by_id_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shop_by_id_for_party(Endpoint, Params) ->
    get_shop_by_id_for_party(Endpoint, Params, []).

-spec get_shop_by_id_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shop_by_id_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops/:shopID"),
        Params,
        get_request_spec(get_shop_by_id_for_party),
        Opts
    ), get_shop_by_id_for_party).

-spec get_shops(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shops(Endpoint, Params) ->
    get_shops(Endpoint, Params, []).

-spec get_shops(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shops(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/shops"),
        Params,
        get_request_spec(get_shops),
        Opts
    ), get_shops).

-spec get_shops_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shops_for_party(Endpoint, Params) ->
    get_shops_for_party(Endpoint, Params, []).

-spec get_shops_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shops_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops"),
        Params,
        get_request_spec(get_shops_for_party),
        Opts
    ), get_shops_for_party).

-spec suspend_shop(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_shop(Endpoint, Params) ->
    suspend_shop(Endpoint, Params, []).

-spec suspend_shop(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_shop(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        put,
        swagger_utils:get_url(Endpoint, "/v2/processing/shops/:shopID/suspend"),
        Params,
        get_request_spec(suspend_shop),
        Opts
    ), suspend_shop).

-spec suspend_shop_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_shop_for_party(Endpoint, Params) ->
    suspend_shop_for_party(Endpoint, Params, []).

-spec suspend_shop_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
suspend_shop_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        put,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/shops/:shopID/suspend"),
        Params,
        get_request_spec(suspend_shop_for_party),
        Opts
    ), suspend_shop_for_party).

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


get_request_spec('activate_shop') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
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
get_request_spec('get_shop_by_id') ->
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
get_request_spec('get_shops') ->
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
get_request_spec('suspend_shop') ->
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

-spec get_response_spec(OperationID :: swagger:operation_id(), Code :: swagger_procession:code()) ->
    Spec :: swagger_procession:response_spec() | no_return().


get_response_spec('activate_shop', 204) ->
    undefined;

get_response_spec('activate_shop', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('activate_shop', 401) ->
    undefined;

get_response_spec('activate_shop', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('activate_shop_for_party', 204) ->
    undefined;

get_response_spec('activate_shop_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('activate_shop_for_party', 401) ->
    undefined;

get_response_spec('activate_shop_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_shop_by_id', 200) ->
    {'Shop', 'Shop'};

get_response_spec('get_shop_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_shop_by_id', 401) ->
    undefined;

get_response_spec('get_shop_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_shop_by_id_for_party', 200) ->
    {'Shop', 'Shop'};

get_response_spec('get_shop_by_id_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_shop_by_id_for_party', 401) ->
    undefined;

get_response_spec('get_shop_by_id_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_shops', 200) ->
    {'list', 'Shop'};

get_response_spec('get_shops', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_shops', 401) ->
    undefined;

get_response_spec('get_shops_for_party', 200) ->
    {'list', 'Shop'};

get_response_spec('get_shops_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_shops_for_party', 401) ->
    undefined;

get_response_spec('get_shops_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('suspend_shop', 204) ->
    undefined;

get_response_spec('suspend_shop', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('suspend_shop', 401) ->
    undefined;

get_response_spec('suspend_shop', 404) ->
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
