%% -*- mode: erlang -*-
-module(swag_client_payment_institutions_api).

%% generated methods

-export([get_payment_institution_by_ref/2]).
-export([get_payment_institution_by_ref/3]).

-export([get_payment_institution_payment_terms/2]).
-export([get_payment_institution_payment_terms/3]).

-export([get_payment_institution_payout_methods/2]).
-export([get_payment_institution_payout_methods/3]).

-export([get_payment_institution_payout_methods_for_party/2]).
-export([get_payment_institution_payout_methods_for_party/3]).

-export([get_payment_institution_payout_schedules/2]).
-export([get_payment_institution_payout_schedules/3]).

-export([get_payment_institution_payout_schedules_for_party/2]).
-export([get_payment_institution_payout_schedules_for_party/3]).

-export([get_payment_institutions/2]).
-export([get_payment_institutions/3]).

-export([get_service_provider_by_id/2]).
-export([get_service_provider_by_id/3]).


-spec get_payment_institution_by_ref(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_by_ref(Endpoint, Params) ->
    get_payment_institution_by_ref(Endpoint, Params, []).

-spec get_payment_institution_by_ref(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_by_ref(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payment-institutions/:paymentInstitutionID"),
        Params,
        get_request_spec(get_payment_institution_by_ref),
        Opts
    ), get_payment_institution_by_ref).

-spec get_payment_institution_payment_terms(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payment_terms(Endpoint, Params) ->
    get_payment_institution_payment_terms(Endpoint, Params, []).

-spec get_payment_institution_payment_terms(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payment_terms(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payment-institutions/:paymentInstitutionID/terms/payments"),
        Params,
        get_request_spec(get_payment_institution_payment_terms),
        Opts
    ), get_payment_institution_payment_terms).

-spec get_payment_institution_payout_methods(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_methods(Endpoint, Params) ->
    get_payment_institution_payout_methods(Endpoint, Params, []).

-spec get_payment_institution_payout_methods(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_methods(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payment-institutions/:paymentInstitutionID/terms/payouts/methods"),
        Params,
        get_request_spec(get_payment_institution_payout_methods),
        Opts
    ), get_payment_institution_payout_methods).

-spec get_payment_institution_payout_methods_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_methods_for_party(Endpoint, Params) ->
    get_payment_institution_payout_methods_for_party(Endpoint, Params, []).

-spec get_payment_institution_payout_methods_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_methods_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/payment-institutions/:paymentInstitutionID/terms/payouts/methods"),
        Params,
        get_request_spec(get_payment_institution_payout_methods_for_party),
        Opts
    ), get_payment_institution_payout_methods_for_party).

-spec get_payment_institution_payout_schedules(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_schedules(Endpoint, Params) ->
    get_payment_institution_payout_schedules(Endpoint, Params, []).

-spec get_payment_institution_payout_schedules(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_schedules(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payment-institutions/:paymentInstitutionID/terms/payouts/schedules"),
        Params,
        get_request_spec(get_payment_institution_payout_schedules),
        Opts
    ), get_payment_institution_payout_schedules).

-spec get_payment_institution_payout_schedules_for_party(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_schedules_for_party(Endpoint, Params) ->
    get_payment_institution_payout_schedules_for_party(Endpoint, Params, []).

-spec get_payment_institution_payout_schedules_for_party(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institution_payout_schedules_for_party(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/parties/:partyID/payment-institutions/:paymentInstitutionID/terms/payouts/schedules"),
        Params,
        get_request_spec(get_payment_institution_payout_schedules_for_party),
        Opts
    ), get_payment_institution_payout_schedules_for_party).

-spec get_payment_institutions(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institutions(Endpoint, Params) ->
    get_payment_institutions(Endpoint, Params, []).

-spec get_payment_institutions(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payment_institutions(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/payment-institutions"),
        Params,
        get_request_spec(get_payment_institutions),
        Opts
    ), get_payment_institutions).

-spec get_service_provider_by_id(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_service_provider_by_id(Endpoint, Params) ->
    get_service_provider_by_id(Endpoint, Params, []).

-spec get_service_provider_by_id(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_service_provider_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v2/processing/service-providers/:serviceProviderID"),
        Params,
        get_request_spec(get_service_provider_by_id),
        Opts
    ), get_service_provider_by_id).

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


get_request_spec('get_payment_institution_by_ref') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentInstitutionID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_institution_payment_terms') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentInstitutionID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_institution_payout_methods') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentInstitutionID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'currency', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_institution_payout_methods_for_party') ->
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
        {'paymentInstitutionID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'currency', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_institution_payout_schedules') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'paymentInstitutionID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'currency', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }},
        {'payoutMethod', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['BankAccount', 'InternationalBankAccount', 'Wallet']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_institution_payout_schedules_for_party') ->
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
        {'paymentInstitutionID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'currency', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }},
        {'payoutMethod', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['BankAccount', 'InternationalBankAccount', 'Wallet']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payment_institutions') ->
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
        }},
        {'residence', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }},
        {'realm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['test', 'live']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_service_provider_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'serviceProviderID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 100}, {min_length, 1}, true
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


get_response_spec('get_payment_institution_by_ref', 200) ->
    {'PaymentInstitution', 'PaymentInstitution'};

get_response_spec('get_payment_institution_by_ref', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institution_by_ref', 401) ->
    undefined;

get_response_spec('get_payment_institution_by_ref', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_institution_payment_terms', 200) ->
    {'PaymentTerms', 'PaymentTerms'};

get_response_spec('get_payment_institution_payment_terms', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institution_payment_terms', 401) ->
    undefined;

get_response_spec('get_payment_institution_payment_terms', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_institution_payout_methods', 200) ->
    {'list', 'string'};

get_response_spec('get_payment_institution_payout_methods', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institution_payout_methods', 401) ->
    undefined;

get_response_spec('get_payment_institution_payout_methods', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_institution_payout_methods_for_party', 200) ->
    {'list', 'string'};

get_response_spec('get_payment_institution_payout_methods_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institution_payout_methods_for_party', 401) ->
    undefined;

get_response_spec('get_payment_institution_payout_methods_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_institution_payout_schedules', 200) ->
    {'list', 'integer'};

get_response_spec('get_payment_institution_payout_schedules', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institution_payout_schedules', 401) ->
    undefined;

get_response_spec('get_payment_institution_payout_schedules', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_institution_payout_schedules_for_party', 200) ->
    {'list', 'integer'};

get_response_spec('get_payment_institution_payout_schedules_for_party', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institution_payout_schedules_for_party', 401) ->
    undefined;

get_response_spec('get_payment_institution_payout_schedules_for_party', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payment_institutions', 200) ->
    {'list', 'PaymentInstitution'};

get_response_spec('get_payment_institutions', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payment_institutions', 401) ->
    undefined;

get_response_spec('get_service_provider_by_id', 200) ->
    {'ServiceProvider', 'ServiceProvider'};

get_response_spec('get_service_provider_by_id', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_service_provider_by_id', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).
