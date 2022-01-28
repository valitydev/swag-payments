%% -*- mode: erlang -*-
-module(swagger_contracts_api).

%% generated methods

-export([get_contract_adjustment_by_id/2]).
-export([get_contract_adjustment_by_id/3]).

-export([get_contract_adjustment_by_id_for_party/2]).
-export([get_contract_adjustment_by_id_for_party/3]).

-export([get_contract_adjustments/2]).
-export([get_contract_adjustments/3]).

-export([get_contract_adjustments_for_party/2]).
-export([get_contract_adjustments_for_party/3]).

-export([get_contract_by_id/2]).
-export([get_contract_by_id/3]).

-export([get_contract_by_id_for_party/2]).
-export([get_contract_by_id_for_party/3]).

-export([get_contracts/2]).
-export([get_contracts/3]).

-export([get_contracts_for_party/2]).
-export([get_contracts_for_party/3]).


-spec get_contract_adjustment_by_id(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustment_by_id(Endpoint, Params) ->
    get_contract_adjustment_by_id(Endpoint, Params, []).

-spec get_contract_adjustment_by_id(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustment_by_id(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/contracts/:contractID/adjustments/:adjustmentID"),
        Params,
        get_request_spec(get_contract_adjustment_by_id),
        Opts
    ), get_contract_adjustment_by_id).

-spec get_contract_adjustment_by_id_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustment_by_id_for_party(Endpoint, Params) ->
    get_contract_adjustment_by_id_for_party(Endpoint, Params, []).

-spec get_contract_adjustment_by_id_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustment_by_id_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/contracts/:contractID/adjustments/:adjustmentID"),
        Params,
        get_request_spec(get_contract_adjustment_by_id_for_party),
        Opts
    ), get_contract_adjustment_by_id_for_party).

-spec get_contract_adjustments(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustments(Endpoint, Params) ->
    get_contract_adjustments(Endpoint, Params, []).

-spec get_contract_adjustments(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustments(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/contracts/:contractID/adjustments"),
        Params,
        get_request_spec(get_contract_adjustments),
        Opts
    ), get_contract_adjustments).

-spec get_contract_adjustments_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustments_for_party(Endpoint, Params) ->
    get_contract_adjustments_for_party(Endpoint, Params, []).

-spec get_contract_adjustments_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_adjustments_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/contracts/:contractID/adjustments"),
        Params,
        get_request_spec(get_contract_adjustments_for_party),
        Opts
    ), get_contract_adjustments_for_party).

-spec get_contract_by_id(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_by_id(Endpoint, Params) ->
    get_contract_by_id(Endpoint, Params, []).

-spec get_contract_by_id(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_by_id(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/contracts/:contractID"),
        Params,
        get_request_spec(get_contract_by_id),
        Opts
    ), get_contract_by_id).

-spec get_contract_by_id_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_by_id_for_party(Endpoint, Params) ->
    get_contract_by_id_for_party(Endpoint, Params, []).

-spec get_contract_by_id_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contract_by_id_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/contracts/:contractID"),
        Params,
        get_request_spec(get_contract_by_id_for_party),
        Opts
    ), get_contract_by_id_for_party).

-spec get_contracts(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contracts(Endpoint, Params) ->
    get_contracts(Endpoint, Params, []).

-spec get_contracts(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contracts(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/contracts"),
        Params,
        get_request_spec(get_contracts),
        Opts
    ), get_contracts).

-spec get_contracts_for_party(Endpoint :: swagger:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contracts_for_party(Endpoint, Params) ->
    get_contracts_for_party(Endpoint, Params, []).

-spec get_contracts_for_party(Endpoint :: swagger:endpoint(), Params :: map(), Opts :: swagger:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_contracts_for_party(Endpoint, Params, Opts) ->
    process_response(swagger_procession:process_request(
        get,
        swagger_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/contracts"),
        Params,
        get_request_spec(get_contracts_for_party),
        Opts
    ), get_contracts_for_party).

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


get_request_spec('get_contract_adjustment_by_id') ->
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
        {'adjustmentID', #{
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
get_request_spec('get_contract_adjustment_by_id_for_party') ->
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
        {'adjustmentID', #{
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
get_request_spec('get_contract_adjustments') ->
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
get_request_spec('get_contract_adjustments_for_party') ->
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
get_request_spec('get_contract_by_id') ->
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
get_request_spec('get_contract_by_id_for_party') ->
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
get_request_spec('get_contracts') ->
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
get_request_spec('get_contracts_for_party') ->
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

-spec get_response_spec(OperationID :: swagger:operation_id(), Code :: swagger_procession:code()) ->
    Spec :: swagger_procession:response_spec() | no_return().


get_response_spec('get_contract_adjustment_by_id', 200) ->
    {'ContractAdjustment', 'ContractAdjustment'};

get_response_spec('get_contract_adjustment_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contract_adjustment_by_id', 401) ->
    undefined;

get_response_spec('get_contract_adjustment_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_contract_adjustment_by_id_for_party', 200) ->
    {'ContractAdjustment', 'ContractAdjustment'};

get_response_spec('get_contract_adjustment_by_id_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contract_adjustment_by_id_for_party', 401) ->
    undefined;

get_response_spec('get_contract_adjustment_by_id_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_contract_adjustments', 200) ->
    {'list', 'ContractAdjustment'};

get_response_spec('get_contract_adjustments', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contract_adjustments', 401) ->
    undefined;

get_response_spec('get_contract_adjustments', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_contract_adjustments_for_party', 200) ->
    {'list', 'ContractAdjustment'};

get_response_spec('get_contract_adjustments_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contract_adjustments_for_party', 401) ->
    undefined;

get_response_spec('get_contract_adjustments_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_contract_by_id', 200) ->
    {'Contract', 'Contract'};

get_response_spec('get_contract_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contract_by_id', 401) ->
    undefined;

get_response_spec('get_contract_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_contract_by_id_for_party', 200) ->
    {'Contract', 'Contract'};

get_response_spec('get_contract_by_id_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contract_by_id_for_party', 401) ->
    undefined;

get_response_spec('get_contract_by_id_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_contracts', 200) ->
    {'list', 'Contract'};

get_response_spec('get_contracts', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contracts', 401) ->
    undefined;

get_response_spec('get_contracts_for_party', 200) ->
    {'list', 'Contract'};

get_response_spec('get_contracts_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_contracts_for_party', 401) ->
    undefined;

get_response_spec('get_contracts_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
