%% -*- mode: erlang -*-

%% basic handler
-module(swag_server_invoices_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id  :: swag_server:operation_id(),
    logic_handler :: module(),
    swagger_handler_opts :: swag_server_router:swagger_handler_opts(),
    context       :: swag_server:request_context()
}).

-type state()              :: state().
-type content_type()       :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type processed_response() :: {stop, cowboy_req:req(), state()}.

%% Cowboy REST callbacks

-spec init(Req :: cowboy_req:req(), Opts :: swag_server_router:init_opts()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.

init(Req, {_Operations, LogicHandler, SwaggerHandlerOpts} = InitOpts) ->
    OperationID    = swag_server_utils:get_operation_id(Req, InitOpts),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = #{cowboy_req => Req}
    },
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateInvoice'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateInvoiceAccessToken'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'FulfillInvoice'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetInvoiceByExternalID'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetInvoiceByExternalIDForParty'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetInvoiceByID'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetInvoiceEvents'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetInvoicePaymentMethods'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'RescindInvoice'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req   :: cowboy_req:req(),
        State :: state()
    }.

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'CreateInvoice' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'CreateInvoiceAccessToken' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'FulfillInvoice' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetInvoiceByExternalID' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetInvoiceByExternalIDForParty' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetInvoiceByID' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetInvoiceEvents' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetInvoicePaymentMethods' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'RescindInvoice' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), AcceptResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateInvoice'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateInvoiceAccessToken'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'FulfillInvoice'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetInvoiceByExternalID'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetInvoiceByExternalIDForParty'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetInvoiceByID'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetInvoiceEvents'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetInvoicePaymentMethods'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'RescindInvoice'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), ProvideResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_request_json}
    ], Req, State}.

-spec charsets_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Charsets :: [binary()], Req :: cowboy_req:req(), State :: state()}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State) ->
    {false, Req, State}.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


%% Handlers

-spec handle_request_json(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = Context
    }
) ->
    ValidationOpts = maps:get(validation_opts, SwaggerHandlerOpts, #{}),
    case populate_request(LogicHandler, OperationID, Req0, ValidationOpts) of
        {ok, Populated, Req1} ->
            {Status, Resp} = handle_request(LogicHandler, OperationID, Populated, Context),
            ok = validate_response(Status, Resp, OperationID, ValidationOpts),
            process_response(ok, encode_response(Resp), Req1, State);
        {error, Reason, Req1} ->
            process_response(error, Reason, Req1, State)
    end.


%% Internal

populate_request(LogicHandler, OperationID, Req, ValidationOpts) ->
    Spec = get_request_spec(OperationID),
    swag_server_handler_api:populate_request(LogicHandler, OperationID, Spec, Req, ValidationOpts).

handle_request(LogicHandler, OperationID, Populated, Context) ->
    swag_server_logic_handler:handle_request(LogicHandler, OperationID, Populated, Context).

validate_response(error, _, _, _) ->
    ok;
validate_response(ok, {Code, _Headers, Body}, OperationID, ValidationOpts) ->
    Spec = get_response_spec(OperationID, Code),
    swag_server_handler_api:validate_response(OperationID, Spec, Body, ValidationOpts).

encode_response(Resp) ->
    swag_server_handler_api:encode_response(Resp).

process_response(Status, Result, Req0, State = #state{operation_id = OperationID}) ->
    Req = swag_server_handler_api:process_response(Status, Result, Req0, OperationID),
    {stop, Req, State}.

validate_headers(_, Req) ->
    {true, Req}.

-spec get_request_spec(OperationID :: swag_server:operation_id()) ->
    Spec :: swag_server_handler_api:request_spec() | no_return().


get_request_spec('CreateInvoice') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'InvoiceParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('CreateInvoiceAccessToken') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
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
get_request_spec('FulfillInvoice') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'Reason', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('GetInvoiceByExternalID') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('GetInvoiceByExternalIDForParty') ->
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
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('GetInvoiceByID') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
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
get_request_spec('GetInvoiceEvents') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'eventID', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, false}]
        }}
    ];
get_request_spec('GetInvoicePaymentMethods') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
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
get_request_spec('RescindInvoice') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'invoiceID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'Reason', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_server:operation_id(), Code :: cowboy:http_status()) ->
    Spec :: swag_server_handler_api:response_spec() | no_return().


get_response_spec('CreateInvoice', 201) ->
    {'InvoiceAndToken', 'InvoiceAndToken'};

get_response_spec('CreateInvoice', 400) ->
    {'inline_response_400_6', 'inline_response_400_6'};

get_response_spec('CreateInvoice', 401) ->
    undefined;

get_response_spec('CreateInvoice', 409) ->
    {'ExternalIDConflictError', 'ExternalIDConflictError'};

get_response_spec('CreateInvoiceAccessToken', 201) ->
    {'AccessToken', 'AccessToken'};

get_response_spec('CreateInvoiceAccessToken', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('CreateInvoiceAccessToken', 401) ->
    undefined;

get_response_spec('CreateInvoiceAccessToken', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('FulfillInvoice', 204) ->
    undefined;

get_response_spec('FulfillInvoice', 400) ->
    {'inline_response_400_7', 'inline_response_400_7'};

get_response_spec('FulfillInvoice', 401) ->
    undefined;

get_response_spec('FulfillInvoice', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('GetInvoiceByExternalID', 200) ->
    {'Invoice', 'Invoice'};

get_response_spec('GetInvoiceByExternalID', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('GetInvoiceByExternalID', 401) ->
    undefined;

get_response_spec('GetInvoiceByExternalID', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('GetInvoiceByExternalIDForParty', 200) ->
    {'Invoice', 'Invoice'};

get_response_spec('GetInvoiceByExternalIDForParty', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('GetInvoiceByExternalIDForParty', 401) ->
    undefined;

get_response_spec('GetInvoiceByExternalIDForParty', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('GetInvoiceByID', 200) ->
    {'Invoice', 'Invoice'};

get_response_spec('GetInvoiceByID', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('GetInvoiceByID', 401) ->
    undefined;

get_response_spec('GetInvoiceByID', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('GetInvoiceEvents', 200) ->
    {'list', 'InvoiceEvent'};

get_response_spec('GetInvoiceEvents', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('GetInvoiceEvents', 401) ->
    undefined;

get_response_spec('GetInvoiceEvents', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('GetInvoicePaymentMethods', 200) ->
    {'list', 'PaymentMethod'};

get_response_spec('GetInvoicePaymentMethods', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('GetInvoicePaymentMethods', 401) ->
    undefined;

get_response_spec('GetInvoicePaymentMethods', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('RescindInvoice', 204) ->
    undefined;

get_response_spec('RescindInvoice', 400) ->
    {'inline_response_400_12', 'inline_response_400_12'};

get_response_spec('RescindInvoice', 401) ->
    undefined;

get_response_spec('RescindInvoice', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(OperationID, Code) ->
    error({invalid_response_code, OperationID, Code}).
