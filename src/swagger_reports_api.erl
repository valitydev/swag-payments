%% -*- mode: erlang -*-
-module(swagger_reports_api).

%% generated methods

-export([create_report/2]).
-export([create_report/3]).

-export([create_report_for_party/2]).
-export([create_report_for_party/3]).

-export([download_file/2]).
-export([download_file/3]).

-export([download_file_for_party/2]).
-export([download_file_for_party/3]).

-export([get_report/2]).
-export([get_report/3]).

-export([get_report_for_party/2]).
-export([get_report_for_party/3]).

-export([get_reports/2]).
-export([get_reports/3]).

-export([get_reports_for_party/2]).
-export([get_reports_for_party/3]).


-spec create_report(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report(Endpoint, Params) ->
    create_report(Endpoint, Params, []).

-spec create_report(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        post,
        swagger_utils:get_url(Endpoint, "/v2/shops/:shopID/reports"),
        Params,
        get_request_spec(create_report),
        Opts
    ), create_report).

-spec create_report_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report_for_party(Endpoint, Params) ->
    create_report_for_party(Endpoint, Params, []).

-spec create_report_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        post,
        swagger_utils:get_url(Endpoint, "/v2/parties/:partyID/shops/:shopID/reports"),
        Params,
        get_request_spec(create_report_for_party),
        Opts
    ), create_report_for_party).

-spec download_file(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
download_file(Endpoint, Params) ->
    download_file(Endpoint, Params, []).

-spec download_file(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
download_file(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/shops/:shopID/reports/:reportID/files/:fileID/download"),
        Params,
        get_request_spec(download_file),
        Opts
    ), download_file).

-spec download_file_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
download_file_for_party(Endpoint, Params) ->
    download_file_for_party(Endpoint, Params, []).

-spec download_file_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
download_file_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/parties/:partyID/shops/:shopID/reports/:reportID/files/:fileID/download"),
        Params,
        get_request_spec(download_file_for_party),
        Opts
    ), download_file_for_party).

-spec get_report(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report(Endpoint, Params) ->
    get_report(Endpoint, Params, []).

-spec get_report(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/shops/:shopID/reports/:reportID"),
        Params,
        get_request_spec(get_report),
        Opts
    ), get_report).

-spec get_report_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report_for_party(Endpoint, Params) ->
    get_report_for_party(Endpoint, Params, []).

-spec get_report_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/parties/:partyID/shops/:shopID/reports/:reportID"),
        Params,
        get_request_spec(get_report_for_party),
        Opts
    ), get_report_for_party).

-spec get_reports(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_reports(Endpoint, Params) ->
    get_reports(Endpoint, Params, []).

-spec get_reports(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_reports(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/shops/:shopID/reports"),
        Params,
        get_request_spec(get_reports),
        Opts
    ), get_reports).

-spec get_reports_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_reports_for_party(Endpoint, Params) ->
    get_reports_for_party(Endpoint, Params, []).

-spec get_reports_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_reports_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/parties/:partyID/shops/:shopID/reports"),
        Params,
        get_request_spec(get_reports_for_party),
        Opts
    ), get_reports_for_party).

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


get_request_spec('create_report') ->
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
        {'ReportParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_report_for_party') ->
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
        {'ReportParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('download_file') ->
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
        {'reportID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'fileID', #{
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
get_request_spec('download_file_for_party') ->
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
        {'reportID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'fileID', #{
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
get_request_spec('get_report') ->
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
        {'reportID', #{
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
get_request_spec('get_report_for_party') ->
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
        {'reportID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
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
get_request_spec('get_reports') ->
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
get_request_spec('get_reports_for_party') ->
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


get_response_spec('create_report', 201) ->
    {'Report', 'Report'};

get_response_spec('create_report', 400) ->
    {'inline_response_400_1', 'inline_response_400_1'};

get_response_spec('create_report', 401) ->
    undefined;

get_response_spec('create_report', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('create_report_for_party', 201) ->
    {'Report', 'Report'};

get_response_spec('create_report_for_party', 400) ->
    {'inline_response_400_1', 'inline_response_400_1'};

get_response_spec('create_report_for_party', 401) ->
    undefined;

get_response_spec('create_report_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('download_file', 200) ->
    {'ReportLink', 'ReportLink'};

get_response_spec('download_file', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('download_file', 401) ->
    undefined;

get_response_spec('download_file', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('download_file_for_party', 200) ->
    {'ReportLink', 'ReportLink'};

get_response_spec('download_file_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('download_file_for_party', 401) ->
    undefined;

get_response_spec('download_file_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_report', 200) ->
    {'Report', 'Report'};

get_response_spec('get_report', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_report', 401) ->
    undefined;

get_response_spec('get_report', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_report_for_party', 200) ->
    {'Report', 'Report'};

get_response_spec('get_report_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_report_for_party', 401) ->
    undefined;

get_response_spec('get_report_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_reports', 200) ->
    {'list', 'Report'};

get_response_spec('get_reports', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('get_reports', 401) ->
    undefined;

get_response_spec('get_reports', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_reports_for_party', 200) ->
    {'list', 'Report'};

get_response_spec('get_reports_for_party', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('get_reports_for_party', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
