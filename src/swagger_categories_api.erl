%% -*- mode: erlang -*-
-module(swagger_categories_api).

%% generated methods

-export([get_categories/2]).
-export([get_categories/3]).

-export([get_category_by_ref/2]).
-export([get_category_by_ref/3]).


-spec get_categories(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_categories(Endpoint, Params) ->
    get_categories(Endpoint, Params, []).

-spec get_categories(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_categories(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/categories"),
        Params,
        get_request_spec(get_categories),
        Opts
    ), get_categories).

-spec get_category_by_ref(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_category_by_ref(Endpoint, Params) ->
    get_category_by_ref(Endpoint, Params, []).

-spec get_category_by_ref(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_category_by_ref(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/categories/:categoryID"),
        Params,
        get_request_spec(get_category_by_ref),
        Opts
    ), get_category_by_ref).

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


get_request_spec('get_categories') ->
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
get_request_spec('get_category_by_ref') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'categoryID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
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


get_response_spec('get_categories', 200) ->
    {'list', 'Category'};

get_response_spec('get_categories', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_categories', 401) ->
    undefined;

get_response_spec('get_category_by_ref', 200) ->
    {'Category', 'Category'};

get_response_spec('get_category_by_ref', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_category_by_ref', 401) ->
    undefined;

get_response_spec('get_category_by_ref', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
