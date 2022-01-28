%% -*- mode: erlang -*-
-module(swag_client_payouts_api).

%% generated methods

-export([create_payout/2]).
-export([create_payout/3]).

-export([get_payout/2]).
-export([get_payout/3]).

-export([get_payout_tool_by_id/2]).
-export([get_payout_tool_by_id/3]).

-export([get_payout_tool_by_id_for_party/2]).
-export([get_payout_tool_by_id_for_party/3]).

-export([get_payout_tools/2]).
-export([get_payout_tools/3]).

-export([get_payout_tools_for_party/2]).
-export([get_payout_tools_for_party/3]).

-export([get_schedule_by_ref/2]).
-export([get_schedule_by_ref/3]).


-spec create_payout(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_payout(Endpoint, Params) ->
    create_payout(Endpoint, Params, []).

-spec create_payout(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_payout(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payouts"),
        Params,
        get_request_spec(create_payout),
        Opts
    ), create_payout).

-spec get_payout(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout(Endpoint, Params) ->
    get_payout(Endpoint, Params, []).

-spec get_payout(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payouts/:payoutID"),
        Params,
        get_request_spec(get_payout),
        Opts
    ), get_payout).

-spec get_payout_tool_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tool_by_id(Endpoint, Params) ->
    get_payout_tool_by_id(Endpoint, Params, []).

-spec get_payout_tool_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tool_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/contracts/:contractID/payout_tools/:payoutToolID"),
        Params,
        get_request_spec(get_payout_tool_by_id),
        Opts
    ), get_payout_tool_by_id).

-spec get_payout_tool_by_id_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tool_by_id_for_party(Endpoint, Params) ->
    get_payout_tool_by_id_for_party(Endpoint, Params, []).

-spec get_payout_tool_by_id_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tool_by_id_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/contracts/:contractID/payout_tools/:payoutToolID"),
        Params,
        get_request_spec(get_payout_tool_by_id_for_party),
        Opts
    ), get_payout_tool_by_id_for_party).

-spec get_payout_tools(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tools(Endpoint, Params) ->
    get_payout_tools(Endpoint, Params, []).

-spec get_payout_tools(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tools(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/contracts/:contractID/payout_tools"),
        Params,
        get_request_spec(get_payout_tools),
        Opts
    ), get_payout_tools).

-spec get_payout_tools_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tools_for_party(Endpoint, Params) ->
    get_payout_tools_for_party(Endpoint, Params, []).

-spec get_payout_tools_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payout_tools_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/contracts/:contractID/payout_tools"),
        Params,
        get_request_spec(get_payout_tools_for_party),
        Opts
    ), get_payout_tools_for_party).

-spec get_schedule_by_ref(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_schedule_by_ref(Endpoint, Params) ->
    get_schedule_by_ref(Endpoint, Params, []).

-spec get_schedule_by_ref(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_schedule_by_ref(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/schedules/:scheduleID"),
        Params,
        get_request_spec(get_schedule_by_ref),
        Opts
    ), get_schedule_by_ref).

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


get_request_spec('create_payout') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'PayoutParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payout') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'payoutID', #{
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
get_request_spec('get_payout_tool_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'contractID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'payoutToolID', #{
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
get_request_spec('get_payout_tool_by_id_for_party') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'contractID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'payoutToolID', #{
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
get_request_spec('get_payout_tools') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'contractID', #{
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
get_request_spec('get_payout_tools_for_party') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'contractID', #{
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
get_request_spec('get_schedule_by_ref') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'scheduleID', #{
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

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('create_payout', 201) ->
    {'Payout', 'Payout'};

get_response_spec('create_payout', 400) ->
    {'inline_response_400_17', 'inline_response_400_17'};

get_response_spec('create_payout', 401) ->
    undefined;

get_response_spec('get_payout', 200) ->
    {'Payout', 'Payout'};

get_response_spec('get_payout', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payout', 401) ->
    undefined;

get_response_spec('get_payout', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payout_tool_by_id', 200) ->
    {'PayoutTool', 'PayoutTool'};

get_response_spec('get_payout_tool_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payout_tool_by_id', 401) ->
    undefined;

get_response_spec('get_payout_tool_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payout_tool_by_id_for_party', 200) ->
    {'PayoutTool', 'PayoutTool'};

get_response_spec('get_payout_tool_by_id_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payout_tool_by_id_for_party', 401) ->
    undefined;

get_response_spec('get_payout_tool_by_id_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payout_tools', 200) ->
    {'list', 'PayoutTool'};

get_response_spec('get_payout_tools', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payout_tools', 401) ->
    undefined;

get_response_spec('get_payout_tools', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payout_tools_for_party', 200) ->
    {'list', 'PayoutTool'};

get_response_spec('get_payout_tools_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payout_tools_for_party', 401) ->
    undefined;

get_response_spec('get_payout_tools_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_schedule_by_ref', 200) ->
    {'Schedule', 'Schedule'};

get_response_spec('get_schedule_by_ref', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_schedule_by_ref', 401) ->
    undefined;

get_response_spec('get_schedule_by_ref', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
