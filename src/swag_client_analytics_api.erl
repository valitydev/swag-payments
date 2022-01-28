%% -*- mode: erlang -*-
-module(swag_client_analytics_api).

%% generated methods

-export([get_payment_conversion_stats/2]).
-export([get_payment_conversion_stats/3]).

-export([get_payment_geo_stats/2]).
-export([get_payment_geo_stats/3]).

-export([get_payment_method_stats/2]).
-export([get_payment_method_stats/3]).

-export([get_payment_rate_stats/2]).
-export([get_payment_rate_stats/3]).

-export([get_payment_revenue_stats/2]).
-export([get_payment_revenue_stats/3]).


-spec get_payment_conversion_stats(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_conversion_stats(Endpoint, Params) ->
    get_payment_conversion_stats(Endpoint, Params, []).

-spec get_payment_conversion_stats(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_conversion_stats(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/payments/stats/conversion"),
        Params,
        get_request_spec(get_payment_conversion_stats),
        Opts
    ), get_payment_conversion_stats).

-spec get_payment_geo_stats(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_geo_stats(Endpoint, Params) ->
    get_payment_geo_stats(Endpoint, Params, []).

-spec get_payment_geo_stats(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_geo_stats(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/payments/stats/geo"),
        Params,
        get_request_spec(get_payment_geo_stats),
        Opts
    ), get_payment_geo_stats).

-spec get_payment_method_stats(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_method_stats(Endpoint, Params) ->
    get_payment_method_stats(Endpoint, Params, []).

-spec get_payment_method_stats(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_method_stats(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/customers/stats/payment_method"),
        Params,
        get_request_spec(get_payment_method_stats),
        Opts
    ), get_payment_method_stats).

-spec get_payment_rate_stats(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_rate_stats(Endpoint, Params) ->
    get_payment_rate_stats(Endpoint, Params, []).

-spec get_payment_rate_stats(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_rate_stats(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/customers/stats/rate"),
        Params,
        get_request_spec(get_payment_rate_stats),
        Opts
    ), get_payment_rate_stats).

-spec get_payment_revenue_stats(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_revenue_stats(Endpoint, Params) ->
    get_payment_revenue_stats(Endpoint, Params, []).

-spec get_payment_revenue_stats(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_revenue_stats(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/analytics/shops/:shopID/payments/stats/revenue"),
        Params,
        get_request_spec(get_payment_revenue_stats),
        Opts
    ), get_payment_revenue_stats).

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


get_request_spec('get_payment_conversion_stats') ->
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
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'splitUnit', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['minute', 'hour', 'day', 'week', 'month', 'year']}, true
, {required, true}]
        }},
        {'splitSize', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_geo_stats') ->
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
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'splitUnit', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['minute', 'hour', 'day', 'week', 'month', 'year']}, true
, {required, true}]
        }},
        {'splitSize', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_method_stats') ->
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
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'splitUnit', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['minute', 'hour', 'day', 'week', 'month', 'year']}, true
, {required, true}]
        }},
        {'splitSize', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'paymentMethod', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['bankCard']}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_rate_stats') ->
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
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_revenue_stats') ->
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
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'splitUnit', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['minute', 'hour', 'day', 'week', 'month', 'year']}, true
, {required, true}]
        }},
        {'splitSize', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {min, 1, inclusive}, true
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


get_response_spec('get_payment_conversion_stats', 200) ->
    {'list', 'PaymentConversionStat'};

get_response_spec('get_payment_conversion_stats', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_conversion_stats', 401) ->
    undefined;

get_response_spec('get_payment_conversion_stats', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_geo_stats', 200) ->
    {'list', 'PaymentGeoStat'};

get_response_spec('get_payment_geo_stats', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_geo_stats', 401) ->
    undefined;

get_response_spec('get_payment_geo_stats', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_method_stats', 200) ->
    {'list', 'PaymentMethodStat'};

get_response_spec('get_payment_method_stats', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_method_stats', 401) ->
    undefined;

get_response_spec('get_payment_method_stats', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_rate_stats', 200) ->
    {'PaymentRateStat', 'PaymentRateStat'};

get_response_spec('get_payment_rate_stats', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_rate_stats', 401) ->
    undefined;

get_response_spec('get_payment_rate_stats', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_revenue_stats', 200) ->
    {'list', 'PaymentRevenueStat'};

get_response_spec('get_payment_revenue_stats', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_revenue_stats', 401) ->
    undefined;

get_response_spec('get_payment_revenue_stats', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
