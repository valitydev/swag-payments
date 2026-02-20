%% -*- mode: erlang -*-
-module(swag_server_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_server:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw()))).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema = #{?DEFINITIONS := Defs}) ->
    try
        Parents = enumerate_parents(Defs),
        DefsFixed = maps:fold(fun correct_definition/3, Defs, Parents),
        Schema#{?DEFINITIONS := DefsFixed}
    catch
        _:Error ->
            handle_error(Error)
    end;
enumerate_discriminator_children(_) ->
    handle_error(no_definitions).

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

enumerate_parents(Definitions) ->
    maps:fold(
        fun
            (Name, #{<<"allOf">> := AllOf}, AccIn) ->
                lists:foldl(
                    fun
                        (#{<<"$ref">> := <<"#/definitions/", Parent/binary>>}, Acc) ->
                            Schema = maps:get(Parent, Definitions),
                            Discriminator = maps:get(<<"discriminator">>, Schema, undefined),
                            add_parent_child(Discriminator, Parent, Name, Acc);
                        (_Schema, Acc) ->
                            Acc
                    end,
                    AccIn,
                    AllOf
                );
            (Name, #{<<"discriminator">> := _}, Acc) ->
                add_parent(Name, Acc);
            (_Name, _Schema, AccIn) ->
                AccIn
        end,
        #{},
        Definitions
    ).

add_parent_child(undefined, _Parent, _Child, Acc) ->
    Acc;
add_parent_child(_Discriminator, Parent, Child, Acc) ->
    maps:put(Parent, [Child | maps:get(Parent, Acc, [])], Acc).

add_parent(Parent, Acc) when not is_map_key(Parent, Acc) ->
    maps:put(Parent, [], Acc);
add_parent(_Parent, Acc) ->
    Acc.

correct_definition(Parent, Children, Definitions) ->
    ParentSchema1 = maps:get(Parent, Definitions),
    Discriminator = maps:get(<<"discriminator">>, ParentSchema1),
    ParentSchema2 = deep_put([<<"properties">>, Discriminator, <<"enum">>], Children, ParentSchema1),
    maps:put(Parent, ParentSchema2, Definitions).

deep_put([K], V, M) ->
    M#{K => V};
deep_put([K | Ks], V, M) ->
    maps:put(K, deep_put(Ks, V, maps:get(K, M)), M).

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"swagger">> => <<"2.0">>,
  <<"info">> => #{
    <<"description">> => <<"## Description\nAPI is designed for the merchants who accept payments via user interface such as a website or a mobile app and it is the only interaction point with the system for goods and services payment transactions.\n## Interaction details\nWhenever an API is accessed, its unique ID must be passed in the header X-Request-ID of the corresponding request:\n```\n X-Request-ID: 37d735d4-0f42-4f05-89fa-eaa478fb5aa9\n```\n### Content type and coding\nThe system accepts and returns data in JSON format and UTF-8 coding:\n```\n  Content-Type: application/json; charset=utf-8\n```\n### Date formats\nThe system accepts and returns timestamp values in the format date-time, described in [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339):\n```\n  2017-01-01T00:00:00Z\n  2017-01-01T00:00:01+00:00\n```\n### Maximum request processing time\nWhenever an API is accessed, the time cutoff parameters, that define maximum request processing time of the transaction completion, can be passed in the header `X-Request-Deadline` of the corresponding request:\n```\n X-Request-Deadline: 10s\n```\nThe system stops processing the request upon the specified time. It is recommended to specify a value that is not more than one minute and not less than three seconds.\n`X-Request-Deadline` can be:\n\n* specified in the format `date-time` according to [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339);\n* specified in relative values: in milliseconds (`150000ms`), in seconds (`540s`) or in minutes (`3.5m`).\n">>,
    <<"version">> => <<"2.0.1">>,
    <<"title">> => <<"Vality Payments API">>
  },
  <<"basePath">> => <<"/v2">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Parties">>,
    <<"description">> => <<"A system party is a data set about your company, structure and conditions of concluded contracts, and also the information about the shops associated with the company.\n">>,
    <<"x-displayName">> => <<"Parties">>
  }, #{
    <<"name">> => <<"Shops">>,
    <<"description">> => <<"A shop is a display of your website or a point of sales in the system. Financial terms, that determine, particularly, the system fee percentage, are linked to the shop. Each shop has its linked accounts that accumulate money sent by payers. Only one account can be in each currency. The shop category is determined by the group of offered goods and services. A banking terminal can be linked to a shop on the side of an acquiring bank. Any changes of these shops require system verification.\nShops created in the test category are used for test payment. The system creates a test shop automatically during the participant registration.\nYour website or point of sales may have more than one shop. The closest analogue can be POS-terminals at a point of sales.\n## Asynchronous notifications\nIt is possible to specify a URL for any shop to receive asynchronous notifications about data status change by setting up webhook. For example, you can set up webhook by specifying URL of your application to which the system will send data about invoice changes. The corresponding public key, created during the webhook setup, is used to verify integrity of data sent to your application URL. You can receive this key in your account.\n">>,
    <<"x-displayName">> => <<"Shops">>
  }, #{
    <<"name">> => <<"Invoices">>,
    <<"description">> => <<"An invoice is a fundamental model for work with payment acceptance system. It is necessary to create an invoice and find out its ID before rendering the payment form launching debit transactions, holding funds on the payer’s card and launching other similar business processes.\nIn general, an invoice is a container for payments, data about goods and a shop. Invoices have customizable limited lifetime. Once lifetime is expired, invoice status is impossible to change.\n\n## Invoice statuses\n### Table of invoice statuses:\n\n| Status     | Indication  |  Description                                                                                                  |\n| --         | --          | --                                                                                                            |\n| Unpaid     | `unpaid`    | An invoice has been created but financial obligations are not fulfilled.                                      |\n| Cancelled  | `cancelled` | An invoice is cancelled with reason, all obligations under it are null and void.                              |\n| Paid       | `paid`      | Financial obligations under an invoice are paid but goods or services has not been provided yet to the payer. |\n| Fulfilled  | `fulfilled` | All obligations, both payer’s and merchant’s ones, are fulfilled.                                             |\n\nInvoice statuses are indicated in the diagram nodes, narrows are marked by the processes. Successful completion of processes generates change from one status to another.\n![Invoice State diagram](wsd/img/invoice.svg)\n## Invoice and payment metadata\nThe system provides you a possibility to fill and save any necessary metadata both in invoice and payment pattern. Data is described by the JSON array. Later the system will provide this data to you when you request invoice or payment data by its ID or it will send it to webhook in asynchronous mode that is set up for the relevant shop if there is one.\n## Invoice events\nAny data status changes generate events. You can receive a full list of events that led to the specific data status or the latest event that describes the current data status. For example, you can request all events or the latest one within the specified invoice ID to find out an invoice status so that to make a decision about the shipment of goods or providing services to the payer.\n## Authorization\nOperations:\n* invoice creation,\n* invoice cancellation,\n* invoice fulfillment,\n* getting a *new* invoice access token (after invoice creation)\n\nare authorised with your API key.\n### Invoice access token\nThe invoice access token authorises a limited amount of transactions needed to make [payments](#tag/Payments) by the specified invoice, in particular:\n* [tokenization](#tag/Tokens) of payment instrument,\n* payment creation by this and only this invoice,\n* getting the invoice status.\n\nThe token is valid for 3 days from the creation. After this it will be impossible to use it to authorise transactions.\n## Money distribution data\nYou can specify the distribution of funds among several shops within one invoice. If necessary, you can add a fee that will be charged to the shop specified during the invoice creation (hereinafter invoice shop). Total amount of all distribution transactions shouldn’t exceed the invoice amount. There shouldn’t be more than one transaction per one shop in the distribution. The distribution transactions can be:\n* With AllocationBodyAmount body which transmits the amount to be transferred to the shop. You must create a transaction in favour of an invoice shop to add a fee.\n* With AllocationBodyTotal body which transmits the total amount of transactions and its fee that can be:\n\n  * AllocationFeeFixed or fee amount in favour of an invoice shop.\n\n  * AllocationFeeShare or some percent of the total amount of transaction in favour of an invoice shop.\n">>,
    <<"x-displayName">> => <<"Invoices">>
  }, #{
    <<"name">> => <<"InvoiceTemplates">>,
    <<"description">> => <<"Invoice templates make invoicing easy. An invoice template is linked to the shop and contains specification that can be used for invoice creation by specifying the cost of goods and services and/or invoice metadata. If a template contains the fixed cost, it can be removed during invoice creation. If invoice metadata is not specified when an invoice is created by a template, they will be taken from a template (if metadata is contained in a template).\n\nThe creation, update and deletion of an invoice template doesn’t require the system verification and requests for these changes.\n## Authorization\nThe creation, update and deletion of an invoice template is authorised by your API key.\n### Invoice template access token\nAn invoice template access token is created in the result of template creation transaction. It authorises:\n* the getting of invoice template by its ID,\n* invoice creation by the template.\n">>,
    <<"x-displayName">> => <<"Invoice templates">>
  }, #{
    <<"name">> => <<"Payments">>,
    <<"description">> => <<"The actual debiting the payer’s funds is made by calling of payment creation method. Before payments the invoice, within which the system will attempt to debit, has to be created and payer’s payment token has to be specified. This way the system provides you an interface that allows your server code to initiate and control the debiting process. This process can be both synchronous, when you are waiting for system response, and asynchronous, when you are waiting for notifications on the webhook set up for the corresponding shop after the payments are launched.\n## Payment options\nThe system provides two payment methods: one-step and two-step, PaymentFlowInstant and PaymentFlowHold.\nOne-step payment (PaymentFlowInstant) is performed by calling of one API method. The result of it is authorisation and further debiting in favour of a shop within one transaction.\nTwo-step payment (PaymentFlowHold) means the call of two methods: one for authorisation and one for debiting. After the successful authorisation the transaction amount will be blocked on a payer’s account so a payer can’t use it.\nThe debiting (capturePayment) can be confirmed on equal or less authorisation amount. If the less amount is specified, the balance will be refunded to a payer. The successful authorisation can be confirmed or cancelled both manually by calling the corresponding API method (capturePayment or cancelPayment) and automatically according to the chosen strategy onHoldExpiration. The manual confirmation period is set in the system settings by yourself and it is usually from 3 to 7 calendar days.\n## Payment session\nThe system ensures the idempotency of debiting funds from payment instrument by providing the unique payment session ID. This ID is provided during the creation of [payment instrument tokens](#tag/Tokens) and guarantees the idempotency of debit requests, providing the protection from erroneous repeated debits.\n## Payment processing time limit\nWhen the payment is created within the system, you can set up the payment processing time in the field `processingDeadline`. When it is expired, the system is trying to stop processing the payment and changing its status to `failed` with the error `processing_deadline_reached`.\n\nProcessing time limit should be considered as a recommendation as the system can fail to stop processing on the basis of the payment instrument and the current payment status. If a field value is not set up, the system will choose it by itself to have enough time for payment transfer in general conditions.\n\nPayment processing time limit, similarly to the header `X-Request-Deadline`, can be specified in format described in RFC 3339 or in relative values.\n## Authorization\nPayment request APIs are authorised by an invoice access token that is used to create the payment or by API key.\n">>,
    <<"x-displayName">> => <<"Payments">>
  }, #{
    <<"name">> => <<"Tokens">>,
    <<"description">> => <<"The system provides you the possibility to initiate the funds withdrawal from payer’s charge cards by yourself and undertakes the processes of the certification and PCS-DSS standard compliance. The standard declares the prohibition on cardholder data processing and storage on the merchant’s side. The approaches used in interface implementation provide the opportunity of HTLM form layout and output for cardholder data on your server side code. To ensure the standard compliance we provide our developed JS-library that collects cardholder data in asynchronous mode and sends it to the system interface for further cryptography and tokenization after it is embedded in HTLM code of your payment form. In response, JS-library returns an unique payment card token, that can be used to run payments, to your payment form.\n\nThe [payment session](#tag/Payments), that ensures the idempodency of funds withdrawal from the payment instrument, is provided during the token creation.\n">>,
    <<"x-displayName">> => <<"Payment tokens">>
  }, #{
    <<"name">> => <<"Categories">>,
    <<"description">> => <<"Categories are used to describe groups of goods and services offered by shops. Categories can influence on statistics provision, shops organisation and also system financial terms.\n">>,
    <<"x-displayName">> => <<"Shop categories">>
  }, #{
    <<"name">> => <<"Webhooks">>,
    <<"description">> => <<"This section describes the methods that allow to manage Webhooks or tools to receive asynchronous notifications via HTTP requests when one or a group of events of interest occur, for example, that the payment within the created invoice has been successfully paid.\nAttention! Only Webhooks Management API is a part of this specification. You will need to read the specification [Vality Webhooks Events API] (https://github.com/valitydev/swag-payments-webhook-events) in order to implement notification handler.\n">>,
    <<"x-displayName">> => <<"Webhooks">>
  }, #{
    <<"name">> => <<"PaymentInstitutions">>,
    <<"description">> => <<"A payment institution is an institution that provides services for financial transactions that occur as a result of system business processes.\n">>,
    <<"x-displayName">> => <<"Payment Institutions">>
  }, #{
    <<"name">> => <<"Error Codes">>,
    <<"description">> => <<"## Business logic errors\nAll business logic errors have as follows:\n```json\n{\n  \"code\": \"string\",\n  \"message\": \"string\"\n}\n```\n\nThe error type is in the field `code` and additional information about the error that occurred is in `message`.\nThere are the following error codes at the present moment:\n| Code                             | Description                                                                                                                                |\n|----------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|\n| **operationNotPermitted**        | Unavailable transaction within the current contract.                                                                                       |\n| **invalidPartyStatus**           | Your participant is blocked or its transactions has been suspended. In the latter case, you can [resume](#operation/activateMyParty) them. |\n| **invalidShopStatus**            | Your shop is blocked or its transactions has been suspended. In the latter case, you can [resume](#operation/activateShop) them.           |\n| **invalidContractStatus**        | Your contract is not valid anymore due to its expiration or termination.                                                                   |\n| **invalidShopID**                | The shop with the specified ID doesn’t exist or unavailable.                                                                               |\n| **invalidInvoiceCost**           | Invoice cost is not specified or invalid, particularly, it isn’t equal to the item cost in the cart.                                       |\n| **invalidInvoiceCart**           | Incorrect cart in invoice, for example, empty.                                                                                             |\n| **invalidInvoiceStatus**         | Invalid [invoice status](#tag/Invoices). For example, in an attempt to [pay](#operation/createPayment) the cancelled invoice.              |\n| **invoiceTermsViolated**         | An invoice violates limitations set within the current contract.                                                                           |\n| **invoicePaymentPending**        | The last pending payment by the specified invoice has not reached the final status yet.                                                    |\n| **invalidPaymentStatus**         | Invalid [payment status](#tag/Payments). For example, in an attempt to [confirm](#operation/capturePayment) unsuccessful payment.          |\n| **invalidPaymentResource**       | The payment instrument that is not supported or connected to the system within the current contract.                                       |\n| **invalidPaymentToolToken**      | Invalid content of payment instrument token.                                                                                               |\n| **invalidProcessingDeadline**    | Invalid format of the payment authorisation time limit.                                                                                    |\n| **invalidPaymentSession**        | Invalid content of the payment session.                                                                                                    |\n| **invalidRecurrentParent**       | Invalid parent recurrent payment is specified.                                                                                             |\n| **insufficentAccountBalance**    | Insufficient account balance on the shop account, for example, for the refund.                                                             |\n| **invoicePaymentAmountExceeded** | Refund attempt exceeds the payment amount.                                                                                                 |\n| **inconsistentRefundCurrency**   | Refund attempt in the currency is different from the payment currency.                                                                     |\n| **changesetConflict**            | An attempt to make changes to the participant that conflicts with changes in other pending requests.                                       |\n| **invalidChangeset**             | Invalid changes to the participant, for example, an attempt to create a shop in the currency that is unavailable within the contract.      |\n| **limitExceeded**                | The reasonable sampling time limit is exceeded. In this case it is better to request less volume of data.                                  |\n| **invalidDeadline**              | Invalid time format.                                                                                                                       |\n| **chargebackInProgress**         | Refund attempt while the chargeback is in progress.                                                                                        |\n| **invalidRequest**               | Other invalid request data.                                                                                                                |\n| **invalidPartyID**               | The participant with the specified ID doesn't exist or unavailable.                                                                        |\n| **ambiguousPartyID**             | It is impossible to define the participant ID, specify the ID more clearly in the request.                                                 |\n| **invalidAllocation**            | Invalid distribution of funds, for example, more than one transaction in favour of one of shops.                                           |\n| **allocationNotPermitted**       | The distribution is not available within the contract.                                                                                     |\n| **refundCartConflict**           | It is impossible to define the refund content as the refund distribution and cart are sent at the same time.                               |\n## General errors\nThe errors that occur during the transaction attempts with the objects that are not registered in the system. They look like\n\n  ```json\n  {\n      \"message\": \"string\"\n  }\n  ```\n\nThe information about the occurred error is in the field `message`. For example:\n\n  ```json\n  {\n      \"message\": \"Invoice not found\"\n  }\n  ```\n\n## Errors in processing requests\nDifferent unpredictable situations can happen during the request processing with the support of our system. The system sends a signal about them according to the HTTP protocol using the corresponding [statuses][5xx] that specify the server errors.\n| Code    | Description                                                                                                                                                                                                                          |\n|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|\n| **500** | An unpredictable situation has occurred during request processing by the system. We recommend contacting the technical support if you receive such a response code.                                                                  |\n| **503** | The system is temporarily unavailable and not ready to serve this request. The request isn’t guaranteed fulfilled, if you receive such a response code, try to resend it later when the availability of the system will be restored. |\n| **504** | The system has exceeded the time allowable for request processing, the result of the request is undefined. Try to resend the request or find our the result of the original request if the repeated request is undesirable.          |\n\n[5xx]: https://tools.ietf.org/html/rfc7231#section-6.6\n\n## Payment errors\nThe errors sent to the payment form (payers can see them):\n| Code                   | Description                                                                                                                                                                                      |\n|------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|\n| InvalidPaymentTool     | Invalid payment instrument (invalid card number, missing account has been entered, etc.)                                                                                                         |\n| AccountLimitsExceeded  | Limits are exceeded (for example, the payment amount or withdrawal country limits are set up in the personal account)                                                                            |\n| InsufficientFunds      | Insufficient funds on the account                                                                                                                                                                |\n| PreauthorizationFailed | Pre-authorisation is failed (invalid SD-Secure code has been entered, cancellation link has been clicked in SD-Secure form)                                                                      |\n| RejectedByIssuer       | The payment is rejected by the issuer (it has been prohibited to withdraw inside the country or to purchase in the Internet, the payment is rejected by the issuer’s anti-fraud entity and etc.) |\n| PaymentRejected        | the payment is rejected by other reasons                                                                                                                                                         |\n\nThe errors sent to the personal merchant’s account (only you can see them):\n- timeout\n\n  Timeout of payment attempt\n\n- rejected_by_inspector\n\n  Rejected by anti-fraud service\n\n- preauthorization_failed\n\n  Preauthorisation error (3DS)\n\n- authorization_failed:\n\n  Provider payment authorisation error\n\n  - unknown\n\n    Unknown authorisation error\n\n  - merchant_blocked\n\n    A merchant is blocked\n\n  - operation_blocked\n\n    A payment transaction is blocked\n\n  - account_not_found\n\n    An account is not found\n\n  - account_blocked\n\n    An account is blocked\n\n  - account_stolen\n\n    An account is stolen\n\n  - insufficient_funds\n\n    Insufficient funds\n\n  - processing_deadline_reached\n\n    Payment fullfillment timeout (see [Payment processing time limit](#section/Payment-processing-time-limit))\n\n  - account_limit_exceeded:\n\n    Payer’s account limit is exceeded\n\n    - unknown\n\n      Limit object is unknown\n\n    - amount\n\n      Amount limit\n\n    - number\n\n      Attempt number limit\n\n  - provider_limit_exceeded:\n\n    The provider limit is exceeded for this merchant or system in general\n\n    - unknown\n\n      Limit object is unknown\n\n    - amount\n\n      Amount limit\n\n    - number\n\n      Attempt number limit\n\n  - payment_tool_rejected:\n\n    A payment instrument is rejected\n\n    - unknown\n\n      An unknown payment instrument\n\n    - bank_card_rejected:\n\n      A bank card is rejected\n\n      - unknown\n\n        The reason is unknown\n\n      - card_number_invalid\n\n        A card number is invalid\n\n      - card_expired\n\n        A card is expired\n\n      - card_holder_invalid\n\n        A cardholder is invalid\n\n      - cvv_invalid\n\n        CVV code is invalid\n\n      - issuer_not_found\n\n        An issuer is not found\n\n  - security_policy_violated\n\n    Security policy violations\n\n  - temporarily_unavailable\n\n    Temporary unavailability of the third parties\n\n  - rejected_by_issuer\n\n    Rejected by the issuer\n\n\nFor example, in the case of invalid CVV:\n```\n{\n   \"code\":\"authorization_failed\",\n   \"subError\":{\n      \"code\":\"payment_tool_rejected\",\n      \"subError\":{\n         \"code\":\"bank_card_rejected\",\n         \"subError\":{\n            \"code\":\"cvv_invalid\"\n         }\n      }\n   }\n}\n```\nIf you have an error that is not described here, contact the technical support.\n">>,
    <<"x-displayName">> => <<"Error codes">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/processing/antifraud/inspectUser">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Inspector">> ],
        <<"summary">> => <<"Inspect user to use payment method">>,
        <<"operationId">> => <<"inspectUser">>,
        <<"parameters">> => [ #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"body">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/body">>
          }
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"OK">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/categories">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Categories">> ],
        <<"description">> => <<"Get list of categories">>,
        <<"operationId">> => <<"getCategories">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"List of categories">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Category">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/processing/categories/{categoryID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Categories">> ],
        <<"description">> => <<"Get category data by identifier">>,
        <<"operationId">> => <<"getCategoryByRef">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"categoryID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Category reference">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Category found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Category">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/countries">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Countries">> ],
        <<"description">> => <<"Get list of countries">>,
        <<"operationId">> => <<"getCountries">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Country list">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Country">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          }
        }
      }
    },
    <<"/processing/countries/{countryID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Countries">> ],
        <<"description">> => <<"Get country data by country identifier">>,
        <<"operationId">> => <<"getCountryByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"countryID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<" Alpha-3 country code by standard [ISO 3166-1] (https://en.wikipedia.org/wiki/ISO_3166-1)">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Country found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Country">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoice-templates">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"InvoiceTemplates">> ],
        <<"description">> => <<"Create an new invoice template.">>,
        <<"operationId">> => <<"createInvoiceTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"invoiceTemplateCreateParams">>,
          <<"description">> => <<"Invoice template parameters.">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceTemplateCreateParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Invoice template created.">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvoiceTemplateAndToken">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data for invoice template creation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ExternalIDConflictError">>
            }
          }
        }
      }
    },
    <<"/processing/invoice-templates/{invoiceTemplateID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"InvoiceTemplates">> ],
        <<"description">> => <<"Get an invoice template by identifier.">>,
        <<"operationId">> => <<"getInvoiceTemplateByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice template ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Invoice template">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvoiceTemplate">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      },
      <<"put">> => #{
        <<"tags">> => [ <<"InvoiceTemplates">> ],
        <<"description">> => <<"Change the invoice template.">>,
        <<"operationId">> => <<"updateInvoiceTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice template ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"invoiceTemplateUpdateParams">>,
          <<"description">> => <<"Invoice template parameters.">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceTemplateUpdateParams">>
          }
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"The invoice template has been changed.">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvoiceTemplate">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data for invoice template change">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_1">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      },
      <<"delete">> => #{
        <<"tags">> => [ <<"InvoiceTemplates">> ],
        <<"description">> => <<"Remove invoice template.">>,
        <<"operationId">> => <<"deleteInvoiceTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice template ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Invoice template removed.">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data for invoice template deletion">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_2">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoice-templates/{invoiceTemplateID}/invoices">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"InvoiceTemplates">> ],
        <<"description">> => <<"Create a new invoice using the invoice template.">>,
        <<"operationId">> => <<"createInvoiceWithTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice template ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"invoiceParamsWithTemplate">>,
          <<"description">> => <<"Invoice parameters">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceParamsWithTemplate">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Invoice created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvoiceAndToken">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data for invoice creation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_3">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ExternalIDConflictError">>
            }
          }
        }
      }
    },
    <<"/processing/invoice-templates/{invoiceTemplateID}/payment-methods">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"InvoiceTemplates">> ],
        <<"description">> => <<"Get the available payment methods for the invoice from the invoice template.">>,
        <<"operationId">> => <<"getInvoicePaymentMethodsByTemplateID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice template ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment methods">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/PaymentMethod">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Get invoice by specified external identifier.">>,
        <<"operationId">> => <<"getInvoiceByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External invoice identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Invoice">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Invoice">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Create a new invoice.">>,
        <<"operationId">> => <<"createInvoice">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"invoiceParams">>,
          <<"description">> => <<"Invoice parameters">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Invoice created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvoiceAndToken">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data for invoice creation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_4">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ExternalIDConflictError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Get an invoice by identifier.">>,
        <<"operationId">> => <<"getInvoiceByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Invoice details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Invoice">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/access-tokens">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Create a new token to access the specified invoice.">>,
        <<"operationId">> => <<"createInvoiceAccessToken">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Access token created.">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/AccessToken">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/events">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Get the history of the specified invoice as a list of events.">>,
        <<"operationId">> => <<"getInvoiceEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"eventID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Event identifier.\nAll events that occurred in the system _after_ the specified event will be included in the selection.\n">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"A list of events">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/InvoiceEvent">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/fulfill">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"To fulfill the specified invoice.">>,
        <<"operationId">> => <<"fulfillInvoice">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"fulfillInvoice">>,
          <<"description">> => <<"Reason for the operation">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Reason">>
          }
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Инвойс погашен">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invoice fulfillment error">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_5">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payment-methods">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Get the payment methods available for the invoice.">>,
        <<"operationId">> => <<"getInvoicePaymentMethods">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment methods">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/PaymentMethod">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get all payments for the specified invoice.">>,
        <<"operationId">> => <<"getPayments">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"List of invoice payments">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Payment">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Create a new payment for the specified invoice.">>,
        <<"operationId">> => <<"createPayment">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"paymentParams">>,
          <<"description">> => <<"Parameters of the payment to be created">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/PaymentParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Payment created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Payment">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data to start the payment">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_6">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ExternalIDConflictError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get payment for the specified invoice.">>,
        <<"operationId">> => <<"getPaymentByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Payment">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}/cancel">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Cancel the specified payment">>,
        <<"operationId">> => <<"cancelPayment">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"cancelPayment">>,
          <<"description">> => <<"Reason for the operation">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Reason">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Payment cancelation request received">>
          },
          <<"400">> => #{
            <<"description">> => <<"Payment cancel error">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_7">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}/capture">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Capture the specified payment. In case the capture amount is less than the original amount, the remainder of the payment will be refunded. (see. [Payment options](#tag/Payments))\n">>,
        <<"operationId">> => <<"capturePayment">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"capturePayment">>,
          <<"description">> => <<"Payment capture parameters">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/CaptureParams">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Request to capture payment accepted">>
          },
          <<"400">> => #{
            <<"description">> => <<"Payment capture error">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_8">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}/chargebacks">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get all chargebacks on the specified payment.">>,
        <<"operationId">> => <<"getChargebacks">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Chargebacks data on payment">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Chargeback">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}/chargebacks/{chargebackID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get data on the chargeback of the specified payment.">>,
        <<"operationId">> => <<"getChargebackByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"chargebackID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Chargeback identifier within the payment">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Chargeback details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Chargeback">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}/refunds">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get all refunds of the specified payment.">>,
        <<"operationId">> => <<"getRefunds">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Refunds data on payment">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Refund">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Create a refund of the specified payment">>,
        <<"operationId">> => <<"createRefund">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"refundParams">>,
          <<"description">> => <<"Parameters of the payment refund to be created">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/RefundParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Refund created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Refund">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid refund data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_9">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ExternalIDConflictError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/payments/{paymentID}/refunds/{refundID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get data on the refund of the specified payment.">>,
        <<"operationId">> => <<"getRefundByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"refundID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Refund identifier within the payment">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Refund details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Refund">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/invoices/{invoiceID}/rescind">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Set the invoice to \"Rescind\"">>,
        <<"operationId">> => <<"rescindInvoice">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Invoice ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"rescindInvoice">>,
          <<"description">> => <<"Reason for the operation">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Reason">>
          }
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Invoice rescinded">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invoice rescind error">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_10">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Parties">> ],
        <<"operationId">> => <<"getPartyByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Party">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Party">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/activate">> => #{
      <<"put">> => #{
        <<"tags">> => [ <<"Parties">> ],
        <<"description">> => <<"Activate party by ID">>,
        <<"operationId">> => <<"activatePartyByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Party activated">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/invoices">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Invoices">> ],
        <<"description">> => <<"Get an invoice by external identifier.">>,
        <<"operationId">> => <<"getInvoiceByExternalIDForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External invoice identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Invoice">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Invoice">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/payments">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get payment by specified external identifier.">>,
        <<"operationId">> => <<"getPaymentByExternalIDForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Payment">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/refunds">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get refund by specified external identifier.">>,
        <<"operationId">> => <<"getRefundByExternalIDForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External refund identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Refund details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Refund">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/shops">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Shops">> ],
        <<"description">> => <<"Get all shops">>,
        <<"operationId">> => <<"getShopsForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"List of shops">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Shop">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/shops/{shopID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Shops">> ],
        <<"description">> => <<"Get shop by id">>,
        <<"operationId">> => <<"getShopByIDForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Shop ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Shop found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Shop">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/shops/{shopID}/activate">> => #{
      <<"put">> => #{
        <<"tags">> => [ <<"Shops">> ],
        <<"description">> => <<"Activate shop">>,
        <<"operationId">> => <<"activateShopForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Shop ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Shop activated">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/shops/{shopID}/cash-limits">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Shops">> ],
        <<"description">> => <<"Get shop cash limits">>,
        <<"operationId">> => <<"getShopCashLimitsForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Shop ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Cash limits found. Each item is scoped to a specific payment method.\nIf a payment method is absent in the list, that method is not allowed for the shop.\n">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/inline_response_200_1">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/shops/{shopID}/suspend">> => #{
      <<"put">> => #{
        <<"tags">> => [ <<"Shops">> ],
        <<"description">> => <<"Suspend the shop. This type of requests is processed by the platform automatically and is executed immediately after sending.\n">>,
        <<"operationId">> => <<"suspendShopForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Shop ID">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Shop suspended">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/suspend">> => #{
      <<"put">> => #{
        <<"tags">> => [ <<"Parties">> ],
        <<"description">> => <<"Suspend party by ID">>,
        <<"operationId">> => <<"suspendPartyByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Party suspended">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/parties/{partyID}/webhooks">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Get a list of installed webhooks">>,
        <<"operationId">> => <<"getWebhooksForParty">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"A list of webhooks">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Webhook">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/processing/payment-institutions">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"PaymentInstitutions">> ],
        <<"description">> => <<"Get a list of payment institutions">>,
        <<"operationId">> => <<"getPaymentInstitutions">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"residence">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Residence, alpha-3 code according to standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"realm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Payment institution's mode">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"test">>, <<"live">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"List of payment institutions">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/PaymentInstitution">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/processing/payment-institutions/{paymentInstitutionID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"PaymentInstitutions">> ],
        <<"description">> => <<"Get data of the payment institution by identifier">>,
        <<"operationId">> => <<"getPaymentInstitutionByRef">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentInstitutionID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Payment institution reference">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment institution found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/PaymentInstitution">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/payment-resources">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Tokens">> ],
        <<"description">> => <<"Create a new one-time payment token provided by the payer, as well as a new unique payment session. The payment instrument token and session identifier are required to create a invoice payment and has a limited lifetime.\n">>,
        <<"operationId">> => <<"createPaymentResource">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"paymentResource">>,
          <<"description">> => <<"Data for the creation of a payment resource">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/PaymentResourceParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Token and session created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/PaymentResourceResult">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/processing/payments">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get payment by the specified external identifier.">>,
        <<"operationId">> => <<"getPaymentByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External payment identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Payment">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/refunds">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payments">> ],
        <<"description">> => <<"Get refund by specified external identifier.">>,
        <<"operationId">> => <<"getRefundByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External refund identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Refund details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Refund">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/service-providers/{serviceProviderID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"PaymentInstitutions">> ],
        <<"description">> => <<"Get data of payment service provider by identifier">>,
        <<"operationId">> => <<"getServiceProviderByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"serviceProviderID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Service provider identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 100,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Payment service provider found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ServiceProvider">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/tradeblocs">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"TradeBlocs">> ],
        <<"description">> => <<"Get a list of trade blocks">>,
        <<"operationId">> => <<"getTradeBlocs">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"List of trade blocs">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/TradeBloc">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          }
        }
      }
    },
    <<"/processing/tradeblocs/{tradeBlocID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"TradeBlocs">> ],
        <<"description">> => <<"Get trade block data by ID">>,
        <<"operationId">> => <<"getTradeBlocByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"tradeBlocID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Trade bloc identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Trade bloc found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/TradeBloc">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/processing/webhooks">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Get list of installed webhooks.">>,
        <<"operationId">> => <<"getWebhooks">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"A list of webhooks">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Webhook">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Set up a new webhook.">>,
        <<"operationId">> => <<"createWebhook">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"webhookParams">>,
          <<"description">> => <<"Parameters of the installed webhook">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Webhook">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Webhook is set">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Webhook">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid webhook data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_11">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"429">> => #{
            <<"description">> => <<"The limit on the number of installed webhooks has been exceeded">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_429">>
            }
          }
        }
      }
    },
    <<"/processing/webhooks/{webhookID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Get a webhook by identifier.">>,
        <<"operationId">> => <<"getWebhookByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"webhookID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Webhook identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Webhook's data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Webhook">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      },
      <<"delete">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Remove the specified webhook.">>,
        <<"operationId">> => <<"deleteWebhookByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"webhookID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Webhook identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Webhook successfully removed">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"Target resource not found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Interaction between the merchant and the system is performed via a secure protocol (HTTPS).\n\nHTTP requests via unencrypted channel are not supported.\n\nThe contents of the API key should be passed in the `Authorization` header:\n\n``` Authorization: Bearer MjMyNDQxMjM6NDUzRmRnZDQ0M... ```\n\nKeys are not divided into test and production keys. Use the test shop ID for test transactions.\n\n**Important: Do not pass the API-key to third parties!**\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"AccessToken">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"payload">> ],
      <<"properties">> => #{
        <<"payload">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Access token payload\n">>
        }
      },
      <<"example">> => #{
        <<"payload">> => <<"payload">>
      }
    },
    <<"Allocation">> => #{
      <<"type">> => <<"array">>,
      <<"description">> => <<"Allocation of cash\n">>,
      <<"items">> => #{
        <<"$ref">> => <<"#/definitions/AllocationTransaction">>
      },
      <<"minItems">> => 1,
      <<"maxItems">> => 100
    },
    <<"AllocationBodyAmount">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/AllocationTransaction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">>, <<"currency">> ],
        <<"properties">> => #{
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"The amount transferred to the selected destination in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
            <<"minimum">> => 1
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        }
      } ],
      <<"description">> => <<"Allocation body amount">>
    },
    <<"AllocationBodyTotal">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/AllocationTransaction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"currency">>, <<"fee">>, <<"total">> ],
        <<"properties">> => #{
          <<"total">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Total transaction amount (includes fees) in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
            <<"minimum">> => 1
          },
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"The amount transferred to the selected destination in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
            <<"readOnly">> => true,
            <<"minimum">> => 1
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"fee">> => #{
            <<"$ref">> => <<"#/definitions/AllocationFee">>
          }
        }
      } ],
      <<"description">> => <<"Transaction body with fee indication">>
    },
    <<"AllocationFee">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"allocationFeeType">> ],
      <<"discriminator">> => <<"allocationFeeType">>,
      <<"properties">> => #{
        <<"target">> => #{
          <<"$ref">> => <<"#/definitions/AllocationFee_target">>
        },
        <<"allocationFeeType">> => #{
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"AllocationFeeFixed">>, <<"AllocationFeeShare">> ]
        }
      },
      <<"description">> => <<"Transaction fee">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"AllocationFeeFixed">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/AllocationFee">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">> ],
        <<"properties">> => #{
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"The value of the fee in minor monetary units, e.g. cents if US dollars are specified as the transaction currency.\n">>,
            <<"minimum">> => 1
          }
        }
      } ],
      <<"description">> => <<"Transaction fee in absolute values">>
    },
    <<"AllocationFeeShare">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/AllocationFee">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"share">> ],
        <<"properties">> => #{
          <<"share">> => #{
            <<"$ref">> => <<"#/definitions/Decimal">>
          },
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"The value of the fee in minor monetary units, e.g. cents if US dollars are specified as the transaction currency.\n">>,
            <<"readOnly">> => true,
            <<"minimum">> => 1
          }
        }
      } ],
      <<"description">> => <<"Transaction fee in relative values">>
    },
    <<"AllocationTarget">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"allocationTargetType">> ],
      <<"discriminator">> => <<"allocationTargetType">>,
      <<"properties">> => #{
        <<"allocationTargetType">> => #{
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"AllocationTargetShop">> ]
        }
      },
      <<"description">> => <<"Target of the transaction">>,
      <<"example">> => #{
        <<"allocationTargetType">> => <<"AllocationTargetShop">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"AllocationTargetShop">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/AllocationTarget">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"shopID">> ],
        <<"properties">> => #{
          <<"shopID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Shop ID">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 40
          }
        }
      } ],
      <<"description">> => <<"The shop as the target of the transaction">>
    },
    <<"AllocationTransaction">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"allocationBodyType">>, <<"target">> ],
      <<"discriminator">> => <<"allocationBodyType">>,
      <<"properties">> => #{
        <<"target">> => #{
          <<"$ref">> => <<"#/definitions/AllocationTarget">>
        },
        <<"allocationBodyType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Transaction body">>,
          <<"enum">> => [ <<"AllocationBodyAmount">>, <<"AllocationBodyTotal">> ]
        },
        <<"cart">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceCart">>
        }
      },
      <<"description">> => <<"Cash allocation transaction">>,
      <<"example">> => #{
        <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
        <<"cart">> => <<"">>,
        <<"target">> => #{
          <<"allocationTargetType">> => <<"AllocationTargetShop">>
        }
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"ApiExtensionRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"apiType">> ],
        <<"properties">> => #{
          <<"apiType">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"API type to use in subsequent requests">>
          }
        }
      } ]
    },
    <<"ApplePay">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/TokenizedCardData">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"merchantID">>, <<"paymentToken">> ],
        <<"properties">> => #{
          <<"merchantID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Apple Pay merchant identifier">>
          },
          <<"paymentToken">> => #{
            <<"type">> => <<"object">>,
            <<"description">> => <<"Aggregate of open and encrypted payment data">>,
            <<"properties">> => #{ }
          }
        },
        <<"description">> => <<"Apple Pay data">>
      } ]
    },
    <<"BankCard">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentSystems">> ],
        <<"properties">> => #{
          <<"paymentSystems">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of payment systems">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"description">> => <<"Payment system.\nThe list of systems available for making payments can be found by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
            }
          },
          <<"tokenProviders">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of payment token providers">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"description">> => <<"Payment token provider.\nThe list of providers available for making payments can be found by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
            }
          },
          <<"tokenProviderData">> => #{
            <<"$ref">> => <<"#/definitions/BankCard_tokenProviderData">>
          }
        }
      } ]
    },
    <<"BankCardDetails">> => #{
      <<"required">> => [ <<"cardNumberMask">>, <<"paymentSystem">> ],
      <<"properties">> => #{
        <<"cardNumberMask">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Masked card number">>,
          <<"pattern">> => <<"^\\d{0,6}\\*+\\d{0,4}$">>
        },
        <<"first6">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"First digits of the card number.\nAbsent for tokenized payment methods.\n">>,
          <<"pattern">> => <<"^\\d{6}$">>
        },
        <<"last4">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Card last digits">>,
          <<"pattern">> => <<"^\\d{0,4}$">>
        },
        <<"paymentSystem">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment system.\nThe list of systems available for making payments can be found by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
        },
        <<"tokenProvider">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment token provider.\nThe list of providers available for making payments can be found by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
        },
        <<"tokenizationMethod">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Tokenization method">>,
          <<"enum">> => [ <<"dpan">>, <<"none">> ]
        }
      }
    },
    <<"BankCardPaymentSystem">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Payment system.\nThe list of systems available for making payments can be found by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
    },
    <<"BankCardTokenizationMethod">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Tokenization method">>,
      <<"enum">> => [ <<"dpan">>, <<"none">> ]
    },
    <<"BankCardTokenProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Payment token provider.\nThe list of providers available for making payments can be found by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
    },
    <<"BankCardTokenProviderData">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"merchantID">>, <<"realm">> ],
      <<"properties">> => #{
        <<"merchantID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Merchant identifier in a payment organization.\nCan be used to pass payment tokens to the provider. For example, this parameter is expected to be passed as gatewayMerchantID for GooglePay and or YandexPay and then used to bind the token to the specified shop.\n">>
        },
        <<"merchantName">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The name of the merchant in the payment organization.\nCan be used, for example, as `merchantInfo.merchantName` in GooglePay or `merchant.name` in YandexPay or `displayName` in ApplePay.\n">>
        },
        <<"orderID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The identifier of the paid account in the payment organization.\nCan be used, for example, as `orderNumber` in SamsungPay or `order.id` in YandexPay.\nUsing the system identifier can be useful when debugging or reconciling data with provider data.\n">>
        },
        <<"realm">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment institution's mode">>,
          <<"enum">> => [ <<"test">>, <<"live">> ]
        }
      },
      <<"description">> => <<"Data for integration with payment token providers. These parameters are set in our system and can be used to build requests to the token provider or to display the payment form correctly.\n">>
    },
    <<"BrowserGetRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/BrowserRequest">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"uriTemplate">> ],
        <<"properties">> => #{
          <<"uriTemplate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"URL value template for browser navigation\nThe template is represented according to the standard [RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
          }
        }
      } ]
    },
    <<"BrowserPostRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/BrowserRequest">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"form">>, <<"uriTemplate">> ],
        <<"properties">> => #{
          <<"uriTemplate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"URL value template for form submission\nThe template is represented according to the standard [RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
          },
          <<"form">> => #{
            <<"$ref">> => <<"#/definitions/UserInteractionForm">>
          }
        }
      } ]
    },
    <<"BrowserRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"requestType">> ],
      <<"discriminator">> => <<"requestType">>,
      <<"properties">> => #{
        <<"requestType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of browser operation">>
        }
      }
    },
    <<"CaptureParams">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Reason">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Captured payment amount, in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
            <<"minimum">> => 1
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"cart">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"A shopping cart with a list of items of **provided** goods or services">>,
            <<"items">> => #{
              <<"$ref">> => <<"#/definitions/InvoiceLine">>
            },
            <<"maxItems">> => 100,
            <<"minItems">> => 1
          },
          <<"allocation">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Final cash allocation\n">>,
            <<"items">> => #{
              <<"$ref">> => <<"#/definitions/AllocationTransaction">>
            },
            <<"maxItems">> => 100,
            <<"minItems">> => 1
          }
        },
        <<"description">> => <<"Data of captured payment amount">>
      } ]
    },
    <<"CardData">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentTool">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"cardNumber">>, <<"expDate">> ],
        <<"properties">> => #{
          <<"cardNumber">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Bankcard number">>,
            <<"pattern">> => <<"^\\d{12,19}$">>
          },
          <<"expDate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Bank card expiration date">>,
            <<"pattern">> => <<"^\\d{2}\\/(\\d{2}|\\d{4})$">>
          },
          <<"cvv">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Verification code">>,
            <<"pattern">> => <<"^\\d{3,4}$">>
          },
          <<"cardHolder">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Cardholder name">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 100,
            <<"pattern">> => <<"^[[:alpha:][:space:][:punct:]]+$">>
          }
        },
        <<"description">> => <<"Bank card">>
      } ]
    },
    <<"CashLimitBound">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"inclusive">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"minimum">> => 0
        },
        <<"inclusive">> => #{
          <<"type">> => <<"boolean">>
        }
      },
      <<"description">> => <<"Cash limit bound.">>,
      <<"example">> => #{
        <<"inclusive">> => true,
        <<"amount">> => 0
      }
    },
    <<"Category">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"categoryID">>, <<"name">> ],
      <<"properties">> => #{
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 100
        },
        <<"categoryID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 1000
        }
      },
      <<"example">> => #{
        <<"name">> => <<"name">>,
        <<"description">> => <<"description">>,
        <<"categoryID">> => 0
      }
    },
    <<"Chargeback">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"createdAt">>, <<"currency">>, <<"id">>, <<"levy">>, <<"stage">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Chargeback ID">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time of creation">>
        },
        <<"body">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Chargeback amount, in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
          <<"minimum">> => 1
        },
        <<"levy">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Chargeback levy amount, in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
          <<"minimum">> => 1
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"reasonCode">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Chargeback reason code">>,
          <<"maxLength">> => 1000
        },
        <<"stage">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Chargeback stage">>,
          <<"enum">> => [ <<"chargeback">>, <<"pre-arbitration">>, <<"arbitration">> ]
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Chargeback status">>,
          <<"enum">> => [ <<"pending">>, <<"rejected">>, <<"accepted">>, <<"cancelled">> ]
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"stage">> => <<"chargeback">>,
        <<"currency">> => <<"currency">>,
        <<"id">> => <<"id">>,
        <<"reasonCode">> => <<"reasonCode">>,
        <<"body">> => 1,
        <<"levy">> => 1,
        <<"status">> => <<"pending">>
      }
    },
    <<"ClientInfo">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"fingerprint">> ],
      <<"properties">> => #{
        <<"fingerprint">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payer's user agent unique fingerprint">>,
          <<"maxLength">> => 1000
        },
        <<"ip">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"ip-address">>,
          <<"description">> => <<"Payer IP-address">>,
          <<"maxLength">> => 45
        },
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri">>,
          <<"description">> => <<"URL from which the payment form was received by the client">>,
          <<"maxLength">> => 1000
        }
      },
      <<"description">> => <<"Payer's client device data">>,
      <<"example">> => #{
        <<"ip">> => <<"ip">>,
        <<"fingerprint">> => <<"fingerprint">>,
        <<"url">> => <<"http://example.com/aeiou">>
      }
    },
    <<"ContactInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"email">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"email">>,
          <<"description">> => <<"Email address">>,
          <<"maxLength">> => 100
        },
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"^\\+\\d{4,15}$">>,
          <<"description">> => <<"Mobile phone number with international prefix according to [E.164](https://en.wikipedia.org/wiki/E.164).\n">>
        },
        <<"firstName">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"John">>,
          <<"description">> => <<"First name">>,
          <<"maxLength">> => 100
        },
        <<"lastName">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Doe">>,
          <<"description">> => <<"Last name">>,
          <<"maxLength">> => 100
        },
        <<"country">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUS">>,
          <<"description">> => <<"Alpha-3 country code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"state">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Colorado">>,
          <<"description">> => <<"State">>,
          <<"maxLength">> => 40
        },
        <<"city">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Denver">>,
          <<"description">> => <<"City">>,
          <<"maxLength">> => 40
        },
        <<"address">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10th Street 13">>,
          <<"description">> => <<"Address">>,
          <<"maxLength">> => 1000
        },
        <<"postalCode">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"00012">>,
          <<"description">> => <<"Postal code">>,
          <<"maxLength">> => 40
        },
        <<"dateOfBirth">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date">>,
          <<"example">> => <<"1970-01-01">>,
          <<"description">> => <<"Date of birth">>
        },
        <<"documentId">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"1234-5 678-abcd">>,
          <<"description">> => <<"Identity document number">>,
          <<"maxLength">> => 40
        }
      },
      <<"description">> => <<"Contact details">>
    },
    <<"ContinuationToken">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"A token signaling that only a part of the data has been transmitted in the response.\nTo receive the next part of the data, it is necessary to reapply to the service, specifying the same list of conditions and the received token. If there is no token, the last part of data is received.\n">>
    },
    <<"CostAmountRange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"lowerBound">>, <<"upperBound">> ],
      <<"properties">> => #{
        <<"upperBound">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"An upper (inclusive) limit on the value of goods or services.">>,
          <<"minimum">> => 1
        },
        <<"lowerBound">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"A lower (inclusive) limit on the value of goods or services.">>,
          <<"minimum">> => 1
        }
      }
    },
    <<"Country">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUS">>,
          <<"description">> => <<"Alpha-3 country code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 200
        },
        <<"tradeBlocs">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Trade bloc identifiers">>
          }
        }
      },
      <<"description">> => <<"Country">>,
      <<"example">> => #{
        <<"tradeBlocs">> => [ <<"tradeBlocs">>, <<"tradeBlocs">> ],
        <<"name">> => <<"name">>,
        <<"id">> => <<"RUS">>
      }
    },
    <<"CountryCode">> => #{
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>,
      <<"description">> => <<"Alpha-3 country code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
      <<"example">> => <<"RUS">>
    },
    <<"CryptoCurrency">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Cryptocurrency payment method.\nThe list of cryptocurrencies available for making payments can be found out by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>,
      <<"example">> => <<"BTC">>
    },
    <<"CryptoCurrencyTransferRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"cryptoAddress">>, <<"cryptoAmount">>, <<"symbolicCode">> ],
        <<"properties">> => #{
          <<"cryptoAddress">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"2NBjv8rkUViGXAQar7n2BsdZjNQgupKtdPJ">>,
            <<"description">> => <<"Cryptocurrency wallet address">>
          },
          <<"symbolicCode">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"BTC">>,
            <<"description">> => <<"Cryptocurrency symbolic code">>
          },
          <<"cryptoAmount">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"0.0012">>,
            <<"description">> => <<"Amount of cash in cryptocurrency">>,
            <<"pattern">> => <<"^[0-9]+[.][0-9]+$">>
          }
        }
      } ]
    },
    <<"CryptoWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"cryptoCurrencies">> ],
        <<"properties">> => #{
          <<"cryptoCurrencies">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of cryptocurrencies">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"example">> => <<"BTC">>,
              <<"description">> => <<"Cryptocurrency payment method.\nThe list of cryptocurrencies available for making payments can be found out by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
            }
          }
        }
      } ]
    },
    <<"CryptoWalletData">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentTool">>
      }, #{
        <<"$ref">> => <<"#/definitions/CryptoWalletDetails">>
      } ]
    },
    <<"CryptoWalletDetails">> => #{
      <<"required">> => [ <<"cryptoCurrency">> ],
      <<"properties">> => #{
        <<"cryptoCurrency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"BTC">>,
          <<"description">> => <<"Cryptocurrency payment method.\nThe list of cryptocurrencies available for making payments can be found out by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
        }
      }
    },
    <<"Currency">> => #{
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>,
      <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>
    },
    <<"Decimal">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"exp">>, <<"m">> ],
      <<"properties">> => #{
        <<"m">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Mantissa.\n">>,
          <<"minimum">> => 0
        },
        <<"exp">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Exponent.\n">>
        }
      },
      <<"description">> => <<"Fractional decimal number of arbitrary precision">>
    },
    <<"DefaultLogicError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"DigitalWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"providers">> ],
        <<"properties">> => #{
          <<"providers">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of E-wallet providers">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"example">> => <<"QIWI">>,
              <<"description">> => <<"E-wallet provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>
            }
          }
        }
      } ]
    },
    <<"DigitalWalletData">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentTool">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"id">>, <<"provider">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"E-wallet identifier">>
          },
          <<"provider">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"QIWI">>,
            <<"description">> => <<"E-wallet provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>
          },
          <<"token">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Token">>
          }
        },
        <<"description">> => <<"E-wallet">>
      } ]
    },
    <<"DigitalWalletDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"provider">> ],
      <<"properties">> => #{
        <<"provider">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"QIWI">>,
          <<"description">> => <<"E-wallet provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>
        }
      }
    },
    <<"DigitalWalletProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"E-wallet provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>,
      <<"example">> => <<"QIWI">>
    },
    <<"ExternalID">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"A platform-unique entity identifier for this party.\nIt is used to ensure request idempotency.\n">>
    },
    <<"ExternalIDConflictError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"externalID">> ],
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The passed value of `externalID` for which a request parameter conflict was detected\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Identifier of the content, created by a previous query with the specified `externalID'\n">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"GeneralError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"message">> ],
      <<"properties">> => #{
        <<"message">> => #{
          <<"type">> => <<"string">>
        }
      }
    },
    <<"GooglePay">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/TokenizedCardData">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"gatewayMerchantID">>, <<"paymentToken">> ],
        <<"properties">> => #{
          <<"gatewayMerchantID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Merchant identifier in the system">>
          },
          <<"paymentToken">> => #{
            <<"type">> => <<"object">>,
            <<"description">> => <<"Aggregate of open and encrypted payment data">>,
            <<"properties">> => #{ }
          }
        },
        <<"description">> => <<"Google Pay data">>
      } ]
    },
    <<"Invoice">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"dueDate">>, <<"id">>, <<"metadata">>, <<"product">>, <<"shopID">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Invoice ID">>
        },
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop ID">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"External invoice identifier">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Created at">>
        },
        <<"dueDate">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Expiration date and time">>
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"The price of the goods or services offered, in minor monetary units, e.g. cents if U.S. dollars are specified as the currency\n">>,
          <<"minimum">> => 1
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"product">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Name of the offered goods or services">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Description of the goods or services offered">>,
          <<"maxLength">> => 1000
        },
        <<"invoiceTemplateID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Invoice template identifier (for invoices created from an invoice template).\n">>
        },
        <<"cart">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceCart">>
        },
        <<"allocation">> => #{
          <<"$ref">> => <<"#/definitions/Allocation">>
        },
        <<"bankAccount">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceBankAccount">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Invoice metadata">>,
          <<"properties">> => #{ }
        },
        <<"clientInfo">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceClientInfo">>
        },
        <<"amountRandomized">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceAmountRandomized">>
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Invoice status">>,
          <<"enum">> => [ <<"unpaid">>, <<"cancelled">>, <<"paid">>, <<"fulfilled">> ]
        },
        <<"reason">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Reason for invoice cancellation or redemption">>,
          <<"maxLength">> => 1000
        }
      },
      <<"example">> => #{
        <<"bankAccount">> => #{
          <<"accountType">> => <<"InvoiceRussianBankAccount">>
        },
        <<"reason">> => <<"reason">>,
        <<"amount">> => 1,
        <<"product">> => <<"product">>,
        <<"allocation">> => <<"">>,
        <<"metadata">> => <<"{}">>,
        <<"dueDate">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"externalID">> => <<"externalID">>,
        <<"description">> => <<"description">>,
        <<"clientInfo">> => #{
          <<"trustLevel">> => <<"wellKnown">>
        },
        <<"cart">> => <<"">>,
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"amountRandomized">> => #{
          <<"original">> => 0,
          <<"randomized">> => 0
        },
        <<"currency">> => <<"currency">>,
        <<"id">> => <<"id">>,
        <<"shopID">> => <<"shopID">>,
        <<"invoiceTemplateID">> => <<"invoiceTemplateID">>,
        <<"status">> => <<"unpaid">>
      }
    },
    <<"InvoiceAmountRandomized">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"original">>, <<"randomized">> ],
      <<"properties">> => #{
        <<"original">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Original invoice cost amount in minor monetary units.\n">>,
          <<"minimum">> => 0
        },
        <<"randomized">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Randomized invoice cost amount in minor monetary units.\n">>,
          <<"minimum">> => 0
        }
      },
      <<"description">> => <<"Describes how invoice cost amount was randomized.\n">>,
      <<"example">> => #{
        <<"original">> => 0,
        <<"randomized">> => 0
      }
    },
    <<"InvoiceAndToken">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"invoice">>, <<"invoiceAccessToken">> ],
      <<"properties">> => #{
        <<"invoice">> => #{
          <<"$ref">> => <<"#/definitions/Invoice">>
        },
        <<"invoiceAccessToken">> => #{
          <<"$ref">> => <<"#/definitions/AccessToken">>
        }
      },
      <<"example">> => #{
        <<"invoiceAccessToken">> => #{
          <<"payload">> => <<"payload">>
        },
        <<"invoice">> => #{
          <<"bankAccount">> => #{
            <<"accountType">> => <<"InvoiceRussianBankAccount">>
          },
          <<"reason">> => <<"reason">>,
          <<"amount">> => 1,
          <<"product">> => <<"product">>,
          <<"allocation">> => <<"">>,
          <<"metadata">> => <<"{}">>,
          <<"dueDate">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"externalID">> => <<"externalID">>,
          <<"description">> => <<"description">>,
          <<"clientInfo">> => #{
            <<"trustLevel">> => <<"wellKnown">>
          },
          <<"cart">> => <<"">>,
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"amountRandomized">> => #{
            <<"original">> => 0,
            <<"randomized">> => 0
          },
          <<"currency">> => <<"currency">>,
          <<"id">> => <<"id">>,
          <<"shopID">> => <<"shopID">>,
          <<"invoiceTemplateID">> => <<"invoiceTemplateID">>,
          <<"status">> => <<"unpaid">>
        }
      }
    },
    <<"InvoiceBankAccount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"accountType">> ],
      <<"discriminator">> => <<"accountType">>,
      <<"properties">> => #{
        <<"accountType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Bank account type">>,
          <<"enum">> => [ <<"InvoiceRussianBankAccount">> ]
        }
      },
      <<"description">> => <<"Information on the bank account of the payer, to which transactions this invoice relates\n">>,
      <<"example">> => #{
        <<"accountType">> => <<"InvoiceRussianBankAccount">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"InvoiceCart">> => #{
      <<"type">> => <<"array">>,
      <<"description">> => <<"Products and services cart\n">>,
      <<"items">> => #{
        <<"$ref">> => <<"#/definitions/InvoiceLine">>
      },
      <<"minItems">> => 1,
      <<"maxItems">> => 100
    },
    <<"InvoiceChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changeType">> ],
      <<"discriminator">> => <<"changeType">>,
      <<"properties">> => #{
        <<"changeType">> => #{
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"InvoiceCreated">>, <<"InvoiceStatusChanged">>, <<"PaymentStarted">>, <<"PaymentStatusChanged">>, <<"PaymentInteractionRequested">>, <<"PaymentInteractionCompleted">>, <<"RefundStarted">>, <<"RefundStatusChanged">> ]
        }
      },
      <<"example">> => #{
        <<"changeType">> => <<"InvoiceCreated">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"InvoiceClientInfo">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"trustLevel">> ],
      <<"properties">> => #{
        <<"trustLevel">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Is the payer reliable?">>,
          <<"enum">> => [ <<"wellKnown">>, <<"unknown">> ]
        }
      },
      <<"description">> => <<"Additional client information">>,
      <<"example">> => #{
        <<"trustLevel">> => <<"wellKnown">>
      }
    },
    <<"InvoiceCreated">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"invoice">> ],
        <<"properties">> => #{
          <<"invoice">> => #{
            <<"$ref">> => <<"#/definitions/Invoice">>
          }
        }
      } ]
    },
    <<"InvoiceEvent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changes">>, <<"createdAt">>, <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"integer">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        },
        <<"changes">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceChange">>
          }
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"changes">> => [ #{
          <<"changeType">> => <<"InvoiceCreated">>
        }, #{
          <<"changeType">> => <<"InvoiceCreated">>
        } ],
        <<"id">> => 0
      }
    },
    <<"InvoiceLine">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"price">>, <<"product">>, <<"quantity">> ],
      <<"properties">> => #{
        <<"product">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Description of the goods or services offered">>,
          <<"maxLength">> => 1000
        },
        <<"quantity">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Number of units of goods or services offered in this item\n">>,
          <<"minimum">> => 1,
          <<"default">> => 1
        },
        <<"price">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"The price of the good or service offered, in minor monetary units, e.g. cents if U.S. dollars are specified as the currency\n">>,
          <<"minimum">> => 1
        },
        <<"cost">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"The total value of the item, taking into account the number of units of goods or services\n">>,
          <<"readOnly">> => true,
          <<"minimum">> => 1
        },
        <<"taxMode">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceLineTaxMode">>
        }
      },
      <<"description">> => <<"Product or service item">>,
      <<"example">> => #{
        <<"product">> => <<"product">>,
        <<"quantity">> => 1,
        <<"cost">> => 1,
        <<"price">> => 1,
        <<"taxMode">> => #{
          <<"type">> => <<"InvoiceLineTaxVAT">>
        }
      }
    },
    <<"InvoiceLineTaxMode">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Tax mode">>,
          <<"enum">> => [ <<"InvoiceLineTaxVAT">> ]
        }
      },
      <<"description">> => <<"The tax mode for the proposed good or service.\nTo be specified only if the proposed good or service is taxable.\n">>,
      <<"example">> => #{
        <<"type">> => <<"InvoiceLineTaxVAT">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"InvoiceLineTaxVAT">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceLineTaxMode">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"rate">> ],
        <<"properties">> => #{
          <<"rate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Tax rate">>,
            <<"enum">> => [ <<"0%">>, <<"10%">>, <<"18%">>, <<"20%">>, <<"10/110">>, <<"18/118">>, <<"20/120">> ]
          }
        },
        <<"description">> => <<"Value added tax in the jurisdiction of the Russian Federation">>
      } ]
    },
    <<"InvoiceParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"dueDate">>, <<"metadata">>, <<"product">>, <<"shopID">> ],
      <<"properties">> => #{
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop ID">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"External invoice identifier">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"dueDate">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"The date and time of expiration of the invoice, after which it can no longer be paid\n">>
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"The value of the goods or services offered, in minor monetary units, such as cents if US dollars are specified as the currency.\nIf no value is specified, the value of the invoice will be the total value of the items in the shopping cart.\n">>,
          <<"minimum">> => 1
        },
        <<"randomizeAmount">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceRandomizeAmount">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"product">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Name of the offered goods or services">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Description of the goods or services offered">>,
          <<"maxLength">> => 1000
        },
        <<"cart">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceCart">>
        },
        <<"allocation">> => #{
          <<"$ref">> => <<"#/definitions/Allocation">>
        },
        <<"bankAccount">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceBankAccount">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Invoice metadata">>,
          <<"properties">> => #{ }
        },
        <<"clientInfo">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceClientInfo">>
        }
      },
      <<"example">> => #{
        <<"bankAccount">> => #{
          <<"accountType">> => <<"InvoiceRussianBankAccount">>
        },
        <<"amount">> => 1,
        <<"product">> => <<"product">>,
        <<"allocation">> => <<"">>,
        <<"metadata">> => <<"{}">>,
        <<"dueDate">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"externalID">> => <<"externalID">>,
        <<"description">> => <<"description">>,
        <<"clientInfo">> => #{
          <<"trustLevel">> => <<"wellKnown">>
        },
        <<"cart">> => <<"">>,
        <<"currency">> => <<"currency">>,
        <<"shopID">> => <<"shopID">>,
        <<"partyID">> => <<"partyID">>,
        <<"randomizeAmount">> => #{
          <<"amountMultiplicityCondition">> => 1,
          <<"minAmountCondition">> => 0,
          <<"precision">> => 2,
          <<"maxAmountCondition">> => 0,
          <<"deviation">> => 0,
          <<"direction">> => <<"both">>
        }
      }
    },
    <<"InvoiceParamsWithTemplate">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A platform-unique entity identifier for this party.\nIt is used to ensure request idempotency.\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"The price of the goods or services offered, in minor monetary units, e.g. cents if U.S. dollars are specified as the currency\n">>,
          <<"minimum">> => 1
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Invoice metadata">>,
          <<"properties">> => #{ }
        }
      },
      <<"example">> => #{
        <<"amount">> => 1,
        <<"metadata">> => <<"{}">>,
        <<"externalID">> => <<"externalID">>,
        <<"currency">> => <<"currency">>
      }
    },
    <<"InvoiceRandomizeAmount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"deviation">> ],
      <<"properties">> => #{
        <<"deviation">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Maximum deviation from original amount value in minor monetary units.\nGenerated random value shall correspond uniform distribution within segment `[-deviation, deviation]`.\n">>,
          <<"minimum">> => 0
        },
        <<"precision">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Rounding of generated random value in minor monetary units of given power of `10`.\nWith rounding of `2` generated value `1234` will be rounded down to `1200`.\n">>,
          <<"minimum">> => 0,
          <<"maximum">> => 5,
          <<"default">> => 2
        },
        <<"direction">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Direction of deviation from original amount.\n">>,
          <<"default">> => <<"both">>,
          <<"enum">> => [ <<"both">>, <<"upward">>, <<"downward">> ]
        },
        <<"minAmountCondition">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Minimum amount applicable for randomization, in minor monetary units.\nIf `null` condition is ignored.\n">>,
          <<"minimum">> => 0
        },
        <<"maxAmountCondition">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Maximum amount applicable for randomization, in minor monetary units.\nIf `null` condition is ignored.\n">>,
          <<"minimum">> => 0
        },
        <<"amountMultiplicityCondition">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Amount must be a multiple of given value to be applicable for randomization. Multiplicity value must be in minor monetary units.\nIf `null` condition is ignored.\n">>,
          <<"minimum">> => 1
        }
      },
      <<"description">> => <<"Describes how to randomly modify invoice's cost amount.\n">>,
      <<"example">> => #{
        <<"amountMultiplicityCondition">> => 1,
        <<"minAmountCondition">> => 0,
        <<"precision">> => 2,
        <<"maxAmountCondition">> => 0,
        <<"deviation">> => 0,
        <<"direction">> => <<"both">>
      }
    },
    <<"InvoiceRussianBankAccount">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceBankAccount">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"account">>, <<"bankBik">> ],
        <<"properties">> => #{
          <<"account">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"12345678912345680000">>,
            <<"description">> => <<"Account number">>,
            <<"pattern">> => <<"^\\d{20}$">>
          },
          <<"bankBik">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"123456789">>,
            <<"description">> => <<"BIK of the banking organization">>,
            <<"pattern">> => <<"^\\d{9}$">>
          }
        },
        <<"description">> => <<"Data of a settlement account in a banking organization operating under the jurisdiction of the Russian Federation.\n">>
      } ]
    },
    <<"InvoiceStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Invoice status">>,
          <<"enum">> => [ <<"unpaid">>, <<"cancelled">>, <<"paid">>, <<"fulfilled">> ]
        },
        <<"reason">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Reason for invoice cancellation or redemption">>,
          <<"maxLength">> => 1000
        }
      }
    },
    <<"InvoiceStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"$ref">> => <<"#/definitions/InvoiceStatus">>
      } ]
    },
    <<"InvoicesTopic">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WebhookScope">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"eventTypes">>, <<"shopID">> ],
        <<"properties">> => #{
          <<"shopID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Shop ID">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 40
          },
          <<"eventTypes">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of invoice event types to be notified about">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"enum">> => [ <<"InvoiceCreated">>, <<"InvoicePaid">>, <<"InvoiceCancelled">>, <<"InvoiceFulfilled">>, <<"PaymentStarted">>, <<"PaymentProcessed">>, <<"PaymentCaptured">>, <<"PaymentCancelled">>, <<"PaymentRefunded">>, <<"PaymentFailed">>, <<"PaymentRefundCreated">>, <<"PaymentRefundSucceeded">>, <<"PaymentRefundFailed">>, <<"PaymentUserInteractionRequested">>, <<"PaymentUserInteractionCompleted">> ]
            }
          }
        }
      } ],
      <<"description">> => <<"Scope that includes invoice events within a specific shop\n">>
    },
    <<"InvoiceTemplate">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"details">>, <<"id">>, <<"lifetime">>, <<"shopID">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Invoice template ID">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"External invoice template identifier">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop ID">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Template name">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Description of the goods or services offered">>,
          <<"maxLength">> => 1000
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Template creation date">>
        },
        <<"lifetime">> => #{
          <<"$ref">> => <<"#/definitions/LifetimeInterval">>
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceTemplateDetails">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Metadata that will be associated with the invoice created by the template, in case other metadata is not specified in the invoice creation request.\n">>,
          <<"properties">> => #{ }
        },
        <<"randomizeAmount">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceRandomizeAmount">>
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"metadata">> => <<"{}">>,
        <<"name">> => <<"name">>,
        <<"lifetime">> => #{
          <<"months">> => 0,
          <<"days">> => 0,
          <<"years">> => 0
        },
        <<"externalID">> => <<"externalID">>,
        <<"description">> => <<"description">>,
        <<"details">> => #{
          <<"templateType">> => <<"templateType">>
        },
        <<"id">> => <<"id">>,
        <<"shopID">> => <<"shopID">>,
        <<"randomizeAmount">> => #{
          <<"amountMultiplicityCondition">> => 1,
          <<"minAmountCondition">> => 0,
          <<"precision">> => 2,
          <<"maxAmountCondition">> => 0,
          <<"deviation">> => 0,
          <<"direction">> => <<"both">>
        }
      }
    },
    <<"InvoiceTemplateAndToken">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"invoiceTemplate">>, <<"invoiceTemplateAccessToken">> ],
      <<"properties">> => #{
        <<"invoiceTemplate">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceTemplate">>
        },
        <<"invoiceTemplateAccessToken">> => #{
          <<"$ref">> => <<"#/definitions/AccessToken">>
        }
      },
      <<"example">> => #{
        <<"invoiceTemplate">> => #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => <<"{}">>,
          <<"name">> => <<"name">>,
          <<"lifetime">> => #{
            <<"months">> => 0,
            <<"days">> => 0,
            <<"years">> => 0
          },
          <<"externalID">> => <<"externalID">>,
          <<"description">> => <<"description">>,
          <<"details">> => #{
            <<"templateType">> => <<"templateType">>
          },
          <<"id">> => <<"id">>,
          <<"shopID">> => <<"shopID">>,
          <<"randomizeAmount">> => #{
            <<"amountMultiplicityCondition">> => 1,
            <<"minAmountCondition">> => 0,
            <<"precision">> => 2,
            <<"maxAmountCondition">> => 0,
            <<"deviation">> => 0,
            <<"direction">> => <<"both">>
          }
        },
        <<"invoiceTemplateAccessToken">> => #{
          <<"payload">> => <<"payload">>
        }
      }
    },
    <<"InvoiceTemplateCreateParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"details">>, <<"lifetime">>, <<"shopID">> ],
      <<"properties">> => #{
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop ID">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"External invoice template identifier">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Template name">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Description of the goods or services offered">>,
          <<"maxLength">> => 1000
        },
        <<"lifetime">> => #{
          <<"$ref">> => <<"#/definitions/LifetimeInterval">>
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceTemplateDetails">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Metadata that will be associated with the invoice created by the template, in case other metadata is not specified in the invoice creation request.\n">>,
          <<"properties">> => #{ }
        },
        <<"randomizeAmount">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceRandomizeAmount">>
        }
      },
      <<"example">> => #{
        <<"metadata">> => <<"{}">>,
        <<"name">> => <<"name">>,
        <<"lifetime">> => #{
          <<"months">> => 0,
          <<"days">> => 0,
          <<"years">> => 0
        },
        <<"externalID">> => <<"externalID">>,
        <<"description">> => <<"description">>,
        <<"details">> => #{
          <<"templateType">> => <<"templateType">>
        },
        <<"shopID">> => <<"shopID">>,
        <<"partyID">> => <<"partyID">>,
        <<"randomizeAmount">> => #{
          <<"amountMultiplicityCondition">> => 1,
          <<"minAmountCondition">> => 0,
          <<"precision">> => 2,
          <<"maxAmountCondition">> => 0,
          <<"deviation">> => 0,
          <<"direction">> => <<"both">>
        }
      }
    },
    <<"InvoiceTemplateDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"templateType">> ],
      <<"discriminator">> => <<"templateType">>,
      <<"properties">> => #{
        <<"templateType">> => #{
          <<"type">> => <<"string">>
        }
      },
      <<"example">> => #{
        <<"templateType">> => <<"templateType">>
      }
    },
    <<"InvoiceTemplateLineCost">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"costType">> ],
      <<"discriminator">> => <<"costType">>,
      <<"properties">> => #{
        <<"costType">> => #{
          <<"type">> => <<"string">>
        }
      },
      <<"description">> => <<"Limitations on the value of goods and services for invoices generated by the template.\n">>
    },
    <<"InvoiceTemplateLineCostFixed">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceTemplateLineCost">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">>, <<"currency">> ],
        <<"properties">> => #{
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"The price of the goods or services offered, in minor monetary units, e.g. cents if U.S. dollars are specified as the currency\n">>,
            <<"minimum">> => 1
          }
        }
      } ]
    },
    <<"InvoiceTemplateLineCostRange">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceTemplateLineCost">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"currency">>, <<"range">> ],
        <<"properties">> => #{
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"range">> => #{
            <<"$ref">> => <<"#/definitions/CostAmountRange">>
          }
        }
      } ]
    },
    <<"InvoiceTemplateLineCostUnlim">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceTemplateLineCost">>
      }, #{ } ]
    },
    <<"InvoiceTemplateMultiLine">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceTemplateDetails">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"cart">>, <<"currency">> ],
        <<"properties">> => #{
          <<"cart">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceCart">>
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        }
      } ]
    },
    <<"InvoiceTemplateSingleLine">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceTemplateDetails">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"price">>, <<"product">> ],
        <<"properties">> => #{
          <<"product">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Name of the offered goods or services">>,
            <<"maxLength">> => 100
          },
          <<"price">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceTemplateLineCost">>
          },
          <<"taxMode">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceLineTaxMode">>
          }
        }
      } ]
    },
    <<"InvoiceTemplateUpdateParams">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Template name">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Description of the goods or services offered">>,
          <<"maxLength">> => 1000
        },
        <<"lifetime">> => #{
          <<"$ref">> => <<"#/definitions/LifetimeInterval">>
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceTemplateDetails">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Metadata that will be associated with the invoice created by the template, in case other metadata is not specified in the invoice creation request.\n">>,
          <<"properties">> => #{ }
        },
        <<"randomizeAmount">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceRandomizeAmount">>
        }
      },
      <<"example">> => #{
        <<"metadata">> => <<"{}">>,
        <<"name">> => <<"name">>,
        <<"lifetime">> => #{
          <<"months">> => 0,
          <<"days">> => 0,
          <<"years">> => 0
        },
        <<"description">> => <<"description">>,
        <<"details">> => #{
          <<"templateType">> => <<"templateType">>
        },
        <<"randomizeAmount">> => #{
          <<"amountMultiplicityCondition">> => 1,
          <<"minAmountCondition">> => 0,
          <<"precision">> => 2,
          <<"maxAmountCondition">> => 0,
          <<"deviation">> => 0,
          <<"direction">> => <<"both">>
        }
      }
    },
    <<"LifetimeInterval">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"days">>, <<"months">>, <<"years">> ],
      <<"properties">> => #{
        <<"days">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"minimum">> => 0
        },
        <<"months">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"minimum">> => 0
        },
        <<"years">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"minimum">> => 0
        }
      },
      <<"description">> => <<"The life time of an invoice from the time it was created.">>,
      <<"example">> => #{
        <<"months">> => 0,
        <<"days">> => 0,
        <<"years">> => 0
      }
    },
    <<"LogicError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>
        }
      },
      <<"description">> => <<"Description of the error that occurred during the payment process">>
    },
    <<"MobileCommerce">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"operators">> ],
        <<"properties">> => #{
          <<"operators">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of mobile operators">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"example">> => <<"MTS">>,
              <<"description">> => <<"Cellular operator.\nThe list of operators available for making payments can be found out by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>
            }
          }
        }
      } ]
    },
    <<"MobileCommerceData">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentTool">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"mobilePhone">> ],
        <<"discriminator">> => <<"mobileCommerceType">>,
        <<"properties">> => #{
          <<"mobilePhone">> => #{
            <<"$ref">> => <<"#/definitions/MobileCommercePhone">>
          }
        },
        <<"description">> => <<"Mobile commerce">>
      } ]
    },
    <<"MobileCommerceDetails">> => #{
      <<"required">> => [ <<"phoneNumber">> ],
      <<"properties">> => #{
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"+7******0102">>,
          <<"description">> => <<"Masked cell phone number">>,
          <<"pattern">> => <<"^\\+\\d\\*{1,10}\\d{2,4}$">>
        }
      }
    },
    <<"MobileCommercePhone">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cc">>, <<"ctn">> ],
      <<"properties">> => #{
        <<"cc">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"7">>,
          <<"description">> => <<"Country code (1-3 digits)">>,
          <<"pattern">> => <<"^\\d{1,3}$">>
        },
        <<"ctn">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"0001234567">>,
          <<"description">> => <<"Phone number">>,
          <<"pattern">> => <<"^\\d{10,12}$">>
        }
      },
      <<"description">> => <<"Telephone number, according to ITU-T recommendation [E.164](https://en.wikipedia.org/wiki/E.164)\n">>
    },
    <<"MobileOperator">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Cellular operator.\nThe list of operators available for making payments can be found out by calling the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\n">>,
      <<"example">> => <<"MTS">>
    },
    <<"Party">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"isBlocked">>, <<"isSuspended">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>
        },
        <<"isSuspended">> => #{
          <<"type">> => <<"boolean">>
        }
      },
      <<"example">> => #{
        <<"isSuspended">> => true,
        <<"isBlocked">> => true,
        <<"id">> => <<"id">>
      }
    },
    <<"PartyID">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"The participant's unique identifier within the system.">>
    },
    <<"Payer">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"payerType">> ],
      <<"discriminator">> => <<"payerType">>,
      <<"properties">> => #{
        <<"payerType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment tool type">>
        },
        <<"sessionInfo">> => #{
          <<"$ref">> => <<"#/definitions/Payer_sessionInfo">>
        }
      },
      <<"example">> => #{
        <<"sessionInfo">> => #{
          <<"redirectUrl">> => <<"redirectUrl">>
        },
        <<"payerType">> => <<"payerType">>
      }
    },
    <<"Payment">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"flow">>, <<"id">>, <<"invoiceID">>, <<"payer">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment ID">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A platform-unique entity identifier for this party.\nIt is used to ensure request idempotency.\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"invoiceID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Identifier of the invoice within which the payment was created\n">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Created at">>
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"The price of the goods or services offered, in minor monetary units, e.g. cents if U.S. dollars are specified as the currency\n">>,
          <<"minimum">> => 1
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"flow">> => #{
          <<"$ref">> => <<"#/definitions/PaymentFlow">>
        },
        <<"payer">> => #{
          <<"$ref">> => <<"#/definitions/Payer">>
        },
        <<"transactionInfo">> => #{
          <<"$ref">> => <<"#/definitions/TransactionInfo">>
        },
        <<"makeRecurrent">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"An indication of the creation of a parent recurrence payment. Successful payment with this attribute can be used as a parent payment in other recurring payments.\n">>,
          <<"readOnly">> => true,
          <<"default">> => false
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Payment metadata">>,
          <<"properties">> => #{ }
        },
        <<"allocation">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Allocation of cash\n">>,
          <<"readOnly">> => true,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/AllocationTransaction">>
          },
          <<"maxItems">> => 100,
          <<"minItems">> => 1
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment status">>,
          <<"enum">> => [ <<"pending">>, <<"processed">>, <<"captured">>, <<"cancelled">>, <<"refunded">>, <<"failed">> ]
        },
        <<"error">> => #{
          <<"$ref">> => <<"#/definitions/PaymentError">>
        }
      },
      <<"example">> => #{
        <<"amount">> => 1,
        <<"metadata">> => <<"{}">>,
        <<"allocation">> => [ #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        } ],
        <<"externalID">> => <<"externalID">>,
        <<"error">> => #{
          <<"code">> => <<"code">>,
          <<"subError">> => #{
            <<"code">> => <<"code">>
          }
        },
        <<"payer">> => #{
          <<"sessionInfo">> => #{
            <<"redirectUrl">> => <<"redirectUrl">>
          },
          <<"payerType">> => <<"payerType">>
        },
        <<"transactionInfo">> => #{
          <<"approvalCode">> => <<"approvalCode">>,
          <<"rrn">> => <<"rrn">>
        },
        <<"makeRecurrent">> => false,
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"invoiceID">> => <<"invoiceID">>,
        <<"currency">> => <<"currency">>,
        <<"id">> => <<"id">>,
        <<"flow">> => #{
          <<"type">> => <<"PaymentFlowInstant">>
        },
        <<"status">> => <<"pending">>
      }
    },
    <<"PaymentError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Basic error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubError">>
        }
      },
      <<"description">> => <<"[Error](#tag/Error-Codes) occurred during the payment process\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>,
        <<"subError">> => #{
          <<"code">> => <<"code">>
        }
      }
    },
    <<"PaymentFlow">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment flow type">>,
          <<"default">> => <<"PaymentFlowInstant">>,
          <<"enum">> => [ <<"PaymentFlowInstant">>, <<"PaymentFlowHold">> ]
        }
      },
      <<"example">> => #{
        <<"type">> => <<"PaymentFlowInstant">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"PaymentFlowHold">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentFlow">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"onHoldExpiration">> ],
        <<"properties">> => #{
          <<"onHoldExpiration">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Cash withholding management policy">>,
            <<"default">> => <<"cancel">>,
            <<"enum">> => [ <<"cancel">>, <<"capture">> ]
          },
          <<"heldUntil">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Date and time of withholding of funds">>,
            <<"readOnly">> => true
          }
        }
      } ]
    },
    <<"PaymentFlowInstant">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentFlow">>
      }, #{ } ]
    },
    <<"PaymentInstitution">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">>, <<"realm">>, <<"residences">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 200
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 1000
        },
        <<"residences">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"RUS">>,
            <<"description">> => <<"Alpha-3 country code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        },
        <<"realm">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment institution's mode">>,
          <<"enum">> => [ <<"test">>, <<"live">> ]
        }
      },
      <<"description">> => <<"Payment Institution">>,
      <<"example">> => #{
        <<"name">> => <<"name">>,
        <<"description">> => <<"description">>,
        <<"realm">> => <<"test">>,
        <<"id">> => 0,
        <<"residences">> => [ <<"RUS">>, <<"RUS">> ]
      }
    },
    <<"PaymentInstitutionAccount">> => #{
      <<"type">> => <<"object">>,
      <<"description">> => <<"Payment institution's account">>
    },
    <<"PaymentInteractionCompleted">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentID">> ],
        <<"properties">> => #{
          <<"paymentID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Payment ID">>
          },
          <<"userInteraction">> => #{
            <<"$ref">> => <<"#/definitions/UserInteraction">>
          }
        },
        <<"description">> => <<"Notification on completion of the last requested interaction with the customer\n">>
      } ]
    },
    <<"PaymentInteractionRequested">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentID">>, <<"userInteraction">> ],
        <<"properties">> => #{
          <<"paymentID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Payment ID">>
          },
          <<"userInteraction">> => #{
            <<"$ref">> => <<"#/definitions/UserInteraction">>
          }
        },
        <<"description">> => <<"Require interaction with the customer to continue the payment process\n">>
      } ]
    },
    <<"PaymentMakeRecurrent">> => #{
      <<"type">> => <<"boolean">>,
      <<"description">> => <<"An indication of the creation of a parent recurrence payment. Successful payment with this attribute can be used as a parent payment in other recurring payments.\n">>,
      <<"default">> => <<"false">>
    },
    <<"PaymentMethod">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"method">> ],
      <<"discriminator">> => <<"method">>,
      <<"properties">> => #{
        <<"method">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment method">>,
          <<"enum">> => [ <<"BankCard">>, <<"PaymentTerminal">>, <<"DigitalWallet">>, <<"CryptoWallet">>, <<"MobileCommerce">> ]
        }
      },
      <<"example">> => #{
        <<"method">> => <<"BankCard">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"PaymentParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"flow">>, <<"payer">> ],
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A platform-unique entity identifier for this party.\nIt is used to ensure request idempotency.\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"flow">> => #{
          <<"$ref">> => <<"#/definitions/PaymentFlow">>
        },
        <<"payer">> => #{
          <<"$ref">> => <<"#/definitions/Payer">>
        },
        <<"processingDeadline">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"30m">>,
          <<"description">> => <<"Maximum payment processing time">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"makeRecurrent">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"An indication of the creation of a parent recurrence payment. Successful payment with this attribute can be used as a parent payment in other recurring payments.\n">>,
          <<"default">> => false
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Metadata to be linked with the payment">>,
          <<"properties">> => #{ }
        }
      },
      <<"example">> => #{
        <<"metadata">> => <<"{}">>,
        <<"processingDeadline">> => <<"30m">>,
        <<"externalID">> => <<"externalID">>,
        <<"payer">> => #{
          <<"sessionInfo">> => #{
            <<"redirectUrl">> => <<"redirectUrl">>
          },
          <<"payerType">> => <<"payerType">>
        },
        <<"flow">> => #{
          <<"type">> => <<"PaymentFlowInstant">>
        },
        <<"makeRecurrent">> => false
      }
    },
    <<"PaymentRecurrentParent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"invoiceID">>, <<"paymentID">> ],
      <<"properties">> => #{
        <<"invoiceID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Invoice identifier">>
        },
        <<"paymentID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment identifier">>
        }
      },
      <<"description">> => <<"Parent payment, on the basis of which the current recurrent payment was created">>
    },
    <<"PaymentResource">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"paymentToolToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A payment tool token provided by the payer.\n_Required when creating a payment or binding, can be obtained during [tokenization](#operation/createPaymentResource)_.\n">>,
          <<"maxLength">> => 2000
        },
        <<"paymentSession">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment session identifier.\n_Required when creating a payment or binding, can be obtained during [tokenization](#operation/createPaymentResource)_.\n">>,
          <<"maxLength">> => 1000
        },
        <<"paymentToolDetails">> => #{
          <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
        },
        <<"clientInfo">> => #{
          <<"$ref">> => <<"#/definitions/BankCard_tokenProviderData">>
        }
      },
      <<"description">> => <<"Disposable payment tool data">>
    },
    <<"PaymentResourceParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"clientInfo">>, <<"paymentTool">> ],
      <<"properties">> => #{
        <<"paymentTool">> => #{
          <<"$ref">> => <<"#/definitions/PaymentTool">>
        },
        <<"clientInfo">> => #{
          <<"$ref">> => <<"#/definitions/ClientInfo">>
        }
      },
      <<"example">> => #{
        <<"paymentTool">> => #{
          <<"paymentToolType">> => <<"CardData">>
        },
        <<"clientInfo">> => #{
          <<"ip">> => <<"ip">>,
          <<"fingerprint">> => <<"fingerprint">>,
          <<"url">> => <<"http://example.com/aeiou">>
        }
      }
    },
    <<"PaymentResourcePayer">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Payer">>
      }, #{
        <<"$ref">> => <<"#/definitions/PaymentResource">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"contactInfo">> ],
        <<"properties">> => #{
          <<"contactInfo">> => #{
            <<"$ref">> => <<"#/definitions/ContactInfo">>
          }
        }
      } ],
      <<"description">> => <<"Disposable payment tool">>
    },
    <<"PaymentResourceResult">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"paymentSession">>, <<"paymentToolToken">> ],
      <<"properties">> => #{
        <<"paymentToolToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment tool token provided by the payer">>,
          <<"maxLength">> => 2000
        },
        <<"resourceToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Clean resource token. Can be used for differentiating one resource from another.\n">>,
          <<"maxLength">> => 2000
        },
        <<"paymentSession">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment session identifier">>,
          <<"maxLength">> => 1000
        },
        <<"paymentToolDetails">> => #{
          <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
        },
        <<"clientInfo">> => #{
          <<"$ref">> => <<"#/definitions/BankCard_tokenProviderData">>
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time until which the payment resource token remains valid\n">>,
          <<"readOnly">> => true
        }
      },
      <<"description">> => <<"Disposable payment tool data">>,
      <<"example">> => #{
        <<"paymentToolToken">> => <<"paymentToolToken">>,
        <<"paymentSession">> => <<"paymentSession">>,
        <<"clientInfo">> => #{ },
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"resourceToken">> => <<"resourceToken">>,
        <<"paymentToolDetails">> => #{
          <<"detailsType">> => <<"detailsType">>
        }
      }
    },
    <<"PaymentStarted">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"payment">> ],
        <<"properties">> => #{
          <<"payment">> => #{
            <<"$ref">> => <<"#/definitions/Payment">>
          }
        }
      } ]
    },
    <<"PaymentStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment status">>,
          <<"enum">> => [ <<"pending">>, <<"processed">>, <<"captured">>, <<"cancelled">>, <<"refunded">>, <<"failed">> ]
        },
        <<"error">> => #{
          <<"$ref">> => <<"#/definitions/PaymentError">>
        }
      }
    },
    <<"PaymentStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"$ref">> => <<"#/definitions/PaymentStatus">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentID">> ],
        <<"properties">> => #{
          <<"paymentID">> => #{
            <<"type">> => <<"string">>
          }
        }
      } ]
    },
    <<"PaymentTerminal">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"providers">> ],
        <<"properties">> => #{
          <<"providers">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Providers">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"example">> => <<"EUROSET">>,
              <<"description">> => <<"Payment terminal provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>
            }
          }
        }
      } ]
    },
    <<"PaymentTerminalData">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentTool">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"provider">> ],
        <<"properties">> => #{
          <<"provider">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"EUROSET">>,
            <<"description">> => <<"Payment terminal provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"example">> => #{
              <<"type">> => <<"BankAccountRUS">>,
              <<"accountNumber">> => <<"40817810500000000035">>,
              <<"bankBIC">> => <<"044525716">>
            },
            <<"description">> => <<"Arbitrary metadata further describing this payment instrument.\n">>,
            <<"properties">> => #{ }
          }
        },
        <<"description">> => <<"Payment terminal">>
      } ]
    },
    <<"PaymentTerminalDetails">> => #{
      <<"required">> => [ <<"provider">> ],
      <<"properties">> => #{
        <<"provider">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"EUROSET">>,
          <<"description">> => <<"Payment terminal provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>
        }
      }
    },
    <<"PaymentTerminalProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Payment terminal provider.\nThe list of providers available for making payments can be found by calling the the corresponding [operation](#operation/getInvoicePaymentMethods) after creating an invoice.\nAdditional provider details can be found out by calling [reference operation](#operation/getServiceProviderByID).\n">>,
      <<"example">> => <<"EUROSET">>
    },
    <<"PaymentTerminalReceipt">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"dueDate">>, <<"shortPaymentID">> ],
        <<"properties">> => #{
          <<"shortPaymentID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Account number for payment via payment terminal">>
          },
          <<"dueDate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Expiration date and time">>
          }
        }
      } ]
    },
    <<"PaymentTool">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"paymentToolType">> ],
      <<"discriminator">> => <<"paymentToolType">>,
      <<"properties">> => #{
        <<"paymentToolType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payment tool type">>,
          <<"enum">> => [ <<"CardData">>, <<"PaymentTerminalData">>, <<"DigitalWalletData">>, <<"TokenizedCardData">>, <<"CryptoWalletData">>, <<"MobileCommerceData">> ]
        }
      },
      <<"example">> => #{
        <<"paymentToolType">> => <<"CardData">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"PaymentToolDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"detailsType">> ],
      <<"discriminator">> => <<"detailsType">>,
      <<"properties">> => #{
        <<"detailsType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of payment tool">>
        }
      },
      <<"description">> => <<"Payment tool details">>,
      <<"example">> => #{
        <<"detailsType">> => <<"detailsType">>
      }
    },
    <<"PaymentToolDetailsBankCard">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/BankCardDetails">>
      } ]
    },
    <<"PaymentToolDetailsCryptoWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/CryptoWalletDetails">>
      } ]
    },
    <<"PaymentToolDetailsDigitalWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/DigitalWalletDetails">>
      } ]
    },
    <<"PaymentToolDetailsMobileCommerce">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/MobileCommerceDetails">>
      } ]
    },
    <<"PaymentToolDetailsPaymentTerminal">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/PaymentTerminalDetails">>
      } ]
    },
    <<"ProcessingDeadline">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"Maximum payment processing time">>,
      <<"example">> => <<"30m">>
    },
    <<"QrCodeDisplayRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"qrCode">> ],
        <<"properties">> => #{
          <<"qrCode">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"binary">>,
            <<"description">> => <<"QR code content as byte array">>
          }
        }
      } ]
    },
    <<"RealmMode">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Payment institution's mode">>,
      <<"enum">> => [ <<"test">>, <<"live">> ]
    },
    <<"Reason">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"reason">> ],
      <<"properties">> => #{
        <<"reason">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Operation reason">>,
          <<"maxLength">> => 1000
        }
      },
      <<"example">> => #{
        <<"reason">> => <<"reason">>
      }
    },
    <<"RecurrentPayer">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Payer">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"contactInfo">>, <<"recurrentParentPayment">> ],
        <<"properties">> => #{
          <<"contactInfo">> => #{
            <<"$ref">> => <<"#/definitions/ContactInfo">>
          },
          <<"recurrentParentPayment">> => #{
            <<"$ref">> => <<"#/definitions/PaymentRecurrentParent">>
          },
          <<"paymentToolDetails">> => #{
            <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
          }
        }
      } ],
      <<"description">> => <<"Recurring payment tool based on another payment">>
    },
    <<"Redirect">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"request">> ],
        <<"properties">> => #{
          <<"request">> => #{
            <<"$ref">> => <<"#/definitions/BrowserRequest">>
          }
        }
      } ]
    },
    <<"Refund">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"id">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Refund ID">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A platform-unique entity identifier for this party.\nIt is used to ensure request idempotency.\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Creation date and time">>
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Refund amount, in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
          <<"minimum">> => 1
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"reason">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Refund reason">>
        },
        <<"cart">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"The final cart of goods and services provided, which should be formed from the invoice cart excluding the items for which a refund has been made. The amount of the cart should be the same as the amount of the payment less the amount of the refund.\n">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceLine">>
          },
          <<"maxItems">> => 100,
          <<"minItems">> => 1
        },
        <<"allocation">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"The final cash distribution, which should be formed from the invoice distribution excluding the items for which a refund is made.\n">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/AllocationTransaction">>
          },
          <<"maxItems">> => 100,
          <<"minItems">> => 1
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Refund status">>,
          <<"enum">> => [ <<"pending">>, <<"succeeded">>, <<"failed">> ]
        },
        <<"error">> => #{
          <<"$ref">> => <<"#/definitions/Refund_error">>
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"reason">> => <<"reason">>,
        <<"amount">> => 1,
        <<"allocation">> => [ #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        } ],
        <<"externalID">> => <<"externalID">>,
        <<"currency">> => <<"currency">>,
        <<"id">> => <<"id">>,
        <<"error">> => #{
          <<"code">> => <<"code">>,
          <<"message">> => <<"message">>
        },
        <<"cart">> => [ #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        } ],
        <<"status">> => <<"pending">>
      }
    },
    <<"RefundParams">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A platform-unique entity identifier for this party.\nIt is used to ensure request idempotency.\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Refund amount, in minor monetary units, e.g. cents if US dollars are specified as the currency.\n">>,
          <<"minimum">> => 1
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"reason">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Refund reason">>,
          <<"maxLength">> => 1000
        },
        <<"cart">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"The final cart of goods and services provided, which should be formed from the invoice cart excluding the items for which a refund has been made. The amount of the cart should be the same as the amount of the payment less the amount of the refund.\n">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceLine">>
          },
          <<"maxItems">> => 100,
          <<"minItems">> => 1
        },
        <<"allocation">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Cash allocation, which should be formed from the items for which a refund is made. The sum of all allocation transactions must match the refund amount.\n">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/AllocationTransaction">>
          },
          <<"maxItems">> => 100,
          <<"minItems">> => 1
        }
      },
      <<"example">> => #{
        <<"reason">> => <<"reason">>,
        <<"amount">> => 1,
        <<"allocation">> => [ #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        }, #{
          <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
          <<"cart">> => <<"">>,
          <<"target">> => #{
            <<"allocationTargetType">> => <<"AllocationTargetShop">>
          }
        } ],
        <<"externalID">> => <<"externalID">>,
        <<"currency">> => <<"currency">>,
        <<"cart">> => [ #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        }, #{
          <<"product">> => <<"product">>,
          <<"quantity">> => 1,
          <<"cost">> => 1,
          <<"price">> => 1,
          <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>
          }
        } ]
      }
    },
    <<"RefundStarted">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentID">>, <<"refund">> ],
        <<"properties">> => #{
          <<"paymentID">> => #{
            <<"type">> => <<"string">>
          },
          <<"refund">> => #{
            <<"$ref">> => <<"#/definitions/Refund">>
          }
        }
      } ]
    },
    <<"RefundStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Refund status">>,
          <<"enum">> => [ <<"pending">>, <<"succeeded">>, <<"failed">> ]
        },
        <<"error">> => #{
          <<"$ref">> => <<"#/definitions/Refund_error">>
        }
      }
    },
    <<"RefundStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentID">>, <<"refundID">> ],
        <<"properties">> => #{
          <<"paymentID">> => #{
            <<"type">> => <<"string">>
          },
          <<"refundID">> => #{
            <<"type">> => <<"string">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/RefundStatus">>
      } ]
    },
    <<"SamsungPay">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/TokenizedCardData">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"referenceID">>, <<"serviceID">> ],
        <<"properties">> => #{
          <<"serviceID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Samsung Pay service identifier">>
          },
          <<"referenceID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Samsung Pay reference indentifier">>
          }
        },
        <<"description">> => <<"Samsung Pay data">>
      } ]
    },
    <<"ServiceProvider">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Service provider's identifier">>,
          <<"maxLength">> => 100
        },
        <<"brandName">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Nubank">>,
          <<"description">> => <<"The name of the provider by which it is known to the general public\n">>,
          <<"maxLength">> => 100
        },
        <<"category">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"onlinebanking">>,
          <<"description">> => <<"Provider сategory.\nCan be used for presentation tasks, such as grouping available payment methods by their provider category, if known.\n">>,
          <<"maxLength">> => 100
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"example">> => #{
            <<"dev.vality.checkout">> => #{
              <<"brandLogo">> => #{
                <<"banner">> => <<"/assets/brands/paypal.svg">>
              },
              <<"localization">> => #{
                <<"name">> => #{
                  <<"ja_JP">> => <<"ヱヴァンゲリヲン">>
                }
              }
            }
          },
          <<"description">> => <<"Arbitrary, namespace-separated metadata that further describes a given provider to various consumers.\n">>,
          <<"properties">> => #{ }
        }
      },
      <<"description">> => <<"Payment service provider.\nA third-party organization that provides payment services, such as maintaining a system of e-wallets or payment terminals.\n">>,
      <<"example">> => #{
        <<"brandName">> => <<"Nubank">>,
        <<"metadata">> => #{
          <<"dev.vality.checkout">> => #{
            <<"brandLogo">> => #{
              <<"banner">> => <<"/assets/brands/paypal.svg">>
            },
            <<"localization">> => #{
              <<"name">> => #{
                <<"ja_JP">> => <<"ヱヴァンゲリヲン">>
              }
            }
          }
        },
        <<"id">> => <<"id">>,
        <<"category">> => <<"onlinebanking">>
      }
    },
    <<"Shop">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"categoryID">>, <<"contractID">>, <<"details">>, <<"id">>, <<"isBlocked">>, <<"isSuspended">>, <<"location">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop ID">>
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Is the shop blocked?">>
        },
        <<"isSuspended">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Are operations suspended within the shop?">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Currency character code according to [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"categoryID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"description">> => <<"Сategory identifier of goods and services offered in this shop\n">>
        },
        <<"location">> => #{
          <<"$ref">> => <<"#/definitions/ShopLocation">>
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/ShopDetails">>
        },
        <<"contractID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Contract identifier on the basis of which the shop is serviced\n">>
        }
      },
      <<"description">> => <<"Shop details">>,
      <<"example">> => #{
        <<"isSuspended">> => true,
        <<"isBlocked">> => true,
        <<"contractID">> => <<"contractID">>,
        <<"currency">> => <<"currency">>,
        <<"location">> => #{
          <<"locationType">> => <<"locationType">>
        },
        <<"details">> => #{
          <<"name">> => <<"name">>,
          <<"description">> => <<"description">>
        },
        <<"id">> => <<"id">>,
        <<"categoryID">> => 0
      }
    },
    <<"ShopDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"name">> ],
      <<"properties">> => #{
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop name">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Short description">>,
          <<"maxLength">> => 1000
        }
      },
      <<"example">> => #{
        <<"name">> => <<"name">>,
        <<"description">> => <<"description">>
      }
    },
    <<"ShopLocation">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"locationType">> ],
      <<"discriminator">> => <<"locationType">>,
      <<"properties">> => #{
        <<"locationType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Location type">>
        }
      },
      <<"description">> => <<"The location of the shop, by which it can be found">>,
      <<"example">> => #{
        <<"locationType">> => <<"locationType">>
      }
    },
    <<"ShopLocationUrl">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/ShopLocation">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"url">> ],
        <<"properties">> => #{
          <<"url">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"uri">>,
            <<"description">> => <<"Shop URL">>,
            <<"maxLength">> => 1000
          }
        }
      } ],
      <<"description">> => <<"Shop location as a url on the Internet">>
    },
    <<"SubError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Details of the error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubError">>
        }
      },
      <<"description">> => <<"Detailed description of the error\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>
      }
    },
    <<"TokenizedCardData">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentTool">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"provider">> ],
        <<"discriminator">> => <<"provider">>,
        <<"properties">> => #{
          <<"provider">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [ <<"ApplePay">>, <<"GooglePay">>, <<"SamsungPay">>, <<"YandexPay">> ]
          }
        },
        <<"description">> => <<"Tokenized bank card">>
      } ]
    },
    <<"TradeBloc">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 200
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"maxLength">> => 1000
        }
      },
      <<"description">> => <<"Trade bloc">>,
      <<"example">> => #{
        <<"name">> => <<"name">>,
        <<"description">> => <<"description">>,
        <<"id">> => <<"id">>
      }
    },
    <<"TransactionInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"rrn">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Retrieval Reference Number">>,
          <<"pattern">> => <<"^[a-zA-Z0-9]{4,20}$">>
        },
        <<"approvalCode">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Authorization Approval Code">>
        }
      },
      <<"description">> => <<"Transaction Info">>,
      <<"example">> => #{
        <<"approvalCode">> => <<"approvalCode">>,
        <<"rrn">> => <<"rrn">>
      }
    },
    <<"UserInspectContact">> => #{
      <<"required">> => [ <<"email">> ],
      <<"properties">> => #{
        <<"email">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"email">>,
          <<"description">> => <<"Email address">>,
          <<"maxLength">> => 100
        },
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"^\\+\\d{4,15}$">>,
          <<"description">> => <<"Mobile phone number with international prefix according to [E.164](https://en.wikipedia.org/wiki/E.164).\n">>
        }
      },
      <<"description">> => <<"Customer contact information">>
    },
    <<"UserInspectCustomer">> => #{
      <<"required">> => [ <<"contact">> ],
      <<"properties">> => #{
        <<"device">> => #{
          <<"$ref">> => <<"#/definitions/UserInspectDevice">>
        },
        <<"contact">> => #{
          <<"$ref">> => <<"#/definitions/UserInspectContact">>
        }
      },
      <<"description">> => <<"Information about client">>
    },
    <<"UserInspectDevice">> => #{
      <<"properties">> => #{
        <<"fingerprint">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Payer's user agent unique fingerprint">>,
          <<"maxLength">> => 1000
        },
        <<"ip">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"ip-address">>,
          <<"description">> => <<"Payer IP-address">>,
          <<"maxLength">> => 45
        }
      },
      <<"description">> => <<"Customer device information">>
    },
    <<"UserInspectShop">> => #{
      <<"required">> => [ <<"partyID">>, <<"shopID">> ],
      <<"properties">> => #{
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Shop ID">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        }
      },
      <<"description">> => <<"Shop information">>,
      <<"example">> => #{
        <<"shopID">> => <<"shopID">>,
        <<"partyID">> => <<"partyID">>
      }
    },
    <<"UserInteraction">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"interactionType">> ],
      <<"discriminator">> => <<"interactionType">>,
      <<"properties">> => #{
        <<"interactionType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of interaction with the user">>
        }
      }
    },
    <<"UserInteractionForm">> => #{
      <<"type">> => <<"array">>,
      <<"description">> => <<"Browser submission form">>,
      <<"items">> => #{
        <<"$ref">> => <<"#/definitions/UserInteractionForm_inner">>
      }
    },
    <<"Webhook">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"scope">>, <<"url">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Identifier of the webhook\n">>,
          <<"readOnly">> => true
        },
        <<"active">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Is notification delivery currently enabled?\n">>,
          <<"readOnly">> => true
        },
        <<"scope">> => #{
          <<"$ref">> => <<"#/definitions/WebhookScope">>
        },
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"http-url">>,
          <<"description">> => <<"The URL that will receive notifications of events that have occurred\n">>,
          <<"maxLength">> => 1000
        },
        <<"publicKey">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"hexadecimal">>,
          <<"example">> => <<"MIGJAoGBAM1fmNUvezts3yglTdhXuqG7OhHxQtDFA+Ss//YuUGjw5ossDbEMoS+SIFuYZ/UL9Xg0rEHNRSbmf48OK+mz0FobEtbji8MADayzGfFopXsfRFa7MVy3Uhu5jBDpLsN3DyJapAkK0TAYINlZXxVjDwxRNheTvC+xub5WNdiwc28fAgMBAAE=">>,
          <<"description">> => <<"The content of the public key used to check the authoritativeness of notifications coming to `url`\n">>,
          <<"readOnly">> => true
        }
      },
      <<"example">> => #{
        <<"scope">> => #{
          <<"topic">> => <<"InvoicesTopic">>
        },
        <<"active">> => true,
        <<"id">> => <<"id">>,
        <<"publicKey">> => <<"MIGJAoGBAM1fmNUvezts3yglTdhXuqG7OhHxQtDFA+Ss//YuUGjw5ossDbEMoS+SIFuYZ/UL9Xg0rEHNRSbmf48OK+mz0FobEtbji8MADayzGfFopXsfRFa7MVy3Uhu5jBDpLsN3DyJapAkK0TAYINlZXxVjDwxRNheTvC+xub5WNdiwc28fAgMBAAE=">>,
        <<"partyID">> => <<"partyID">>,
        <<"url">> => <<"url">>
      }
    },
    <<"WebhookScope">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"topic">> ],
      <<"discriminator">> => <<"topic">>,
      <<"properties">> => #{
        <<"topic">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Subject of notifications">>,
          <<"enum">> => [ <<"InvoicesTopic">>, <<"CustomersTopic">> ]
        }
      },
      <<"description">> => <<"The scope of a webhook, limiting the list of event types, for which the notifications should be sent\n">>,
      <<"example">> => #{
        <<"topic">> => <<"InvoicesTopic">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"YandexPay">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/TokenizedCardData">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"gatewayMerchantID">>, <<"paymentToken">> ],
        <<"properties">> => #{
          <<"gatewayMerchantID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Merchant identifier in the system">>
          },
          <<"paymentToken">> => #{
            <<"type">> => <<"object">>,
            <<"description">> => <<"Aggregate of open and encrypted payment data">>,
            <<"properties">> => #{ }
          }
        },
        <<"description">> => <<"Yandex Pay data">>
      } ]
    },
    <<"body">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"customer">>, <<"shops">> ],
      <<"properties">> => #{
        <<"customer">> => #{
          <<"$ref">> => <<"#/definitions/UserInspectCustomer">>
        },
        <<"shops">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/UserInspectShop">>
          }
        }
      }
    },
    <<"inline_response_200">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"blockedShops">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/UserInspectShop">>
          }
        }
      },
      <<"example">> => #{
        <<"blockedShops">> => [ #{
          <<"shopID">> => <<"shopID">>,
          <<"partyID">> => <<"partyID">>
        }, #{
          <<"shopID">> => <<"shopID">>,
          <<"partyID">> => <<"partyID">>
        } ]
      }
    },
    <<"inline_response_400">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyID">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"invalidShopID">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidInvoiceCart">>, <<"ambiguousPartyID">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Lifetime cannot be zero">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_1">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidInvoiceCart">>, <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid party status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_2">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid party status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_3">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"invoiceTermsViolated">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid party status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_4">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyID">>, <<"invalidShopID">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidInvoiceCart">>, <<"invalidAllocation">>, <<"allocationNotPermitted">>, <<"invalidInvoiceCost">>, <<"invoiceTermsViolated">>, <<"ambiguousPartyID">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Shop not found or inaccessible">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_5">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidInvoiceStatus">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid invoice status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_6">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidInvoiceStatus">>, <<"invoicePaymentPending">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidContractStatus">>, <<"invalidPaymentToolToken">>, <<"invalidPaymentSession">>, <<"invalidProcessingDeadline">>, <<"invalidRecurrentParent">>, <<"operationNotPermitted">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid invoice status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_7">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPaymentStatus">>, <<"operationNotPermitted">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid payment status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_8">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPaymentStatus">>, <<"operationNotPermitted">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"invalidInvoiceCart">>, <<"invalidAllocation">>, <<"allocationNotPermitted">>, <<"inconsistentCaptureCurrency">>, <<"amountExceededCaptureBalance">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid payment status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_9">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidContractStatus">>, <<"invalidInvoiceCart">>, <<"invalidAllocation">>, <<"allocationNotPermitted">>, <<"operationNotPermitted">>, <<"invalidPaymentStatus">>, <<"insufficentAccountBalance">>, <<"invoicePaymentAmountExceeded">>, <<"inconsistentRefundCurrency">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"chargebackInProgress">>, <<"refundCartConflict">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Operation not permitted">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_400_10">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidInvoiceStatus">>, <<"invoicePaymentPending">>, <<"invalidPartyStatus">>, <<"invalidShopStatus">>, <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid invoice status">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_200_1">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"lowerBound">>, <<"paymentMethod">>, <<"upperBound">> ],
      <<"properties">> => #{
        <<"currency">> => #{
          <<"$ref">> => <<"#/definitions/Currency">>
        },
        <<"paymentMethod">> => #{
          <<"$ref">> => <<"#/definitions/PaymentMethod">>
        },
        <<"lowerBound">> => #{
          <<"$ref">> => <<"#/definitions/CashLimitBound">>
        },
        <<"upperBound">> => #{
          <<"$ref">> => <<"#/definitions/CashLimitBound">>
        }
      },
      <<"example">> => #{
        <<"upperBound">> => #{
          <<"inclusive">> => true,
          <<"amount">> => 0
        },
        <<"paymentMethod">> => #{
          <<"method">> => <<"BankCard">>
        },
        <<"currency">> => #{ },
        <<"lowerBound">> => #{
          <<"inclusive">> => true,
          <<"amount">> => 0
        }
      }
    },
    <<"inline_response_400_11">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Error code](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidPartyID">>, <<"invalidShopID">>, <<"invalidRequest">>, <<"invalidDeadline">>, <<"ambiguousPartyID">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Shop not found or inaccessible">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"inline_response_429">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"message">> ],
      <<"properties">> => #{
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Webhook limit exceeded">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"AllocationFee_target">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"allocationTargetType">> ],
      <<"properties">> => #{
        <<"allocationTargetType">> => #{
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"AllocationTargetShop">> ]
        }
      },
      <<"description">> => <<"Target of the transaction">>
    },
    <<"BankCard_tokenProviderData">> => #{
      <<"type">> => <<"object">>
    },
    <<"Payer_sessionInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"redirectUrl">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri-template">>,
          <<"description">> => <<"URL of the resource to which the payer should be redirected upon completion of interaction with it in the browser, for example, preauthorization of payment using 3D Secure 2.0 protocol, if such interaction is required.\n">>,
          <<"maxLength">> => 2000
        }
      },
      <<"description">> => <<"Payer's current session data">>,
      <<"example">> => #{
        <<"redirectUrl">> => <<"redirectUrl">>
      }
    },
    <<"Refund_error">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Error code for automatic processing">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Human-readable error description">>
        }
      },
      <<"description">> => <<"Data of the error that occurred during the refund process, if the refund was unsuccessful\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>,
        <<"message">> => <<"message">>
      }
    },
    <<"UserInteractionForm_inner">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"key">>, <<"template">> ],
      <<"properties">> => #{
        <<"key">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The value of the key of the form element to be send by means of browser\n">>
        },
        <<"template">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The template for the form element value\nThe template is presented according to the standard [RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
        }
      }
    }
  },
  <<"parameters">> => #{
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Unique identifier of the request to the system">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    },
    <<"shopID">> => #{
      <<"name">> => <<"shopID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Shop ID">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"invoiceTemplateID">> => #{
      <<"name">> => <<"invoiceTemplateID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Invoice template ID">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"paymentID">> => #{
      <<"name">> => <<"paymentID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Invoice payment identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"refundID">> => #{
      <<"name">> => <<"refundID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Refund identifier within the payment">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"chargebackID">> => #{
      <<"name">> => <<"chargebackID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Chargeback identifier within the payment">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"residence">> => #{
      <<"name">> => <<"residence">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Residence, alpha-3 code according to standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>
    },
    <<"webhookID">> => #{
      <<"name">> => <<"webhookID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Webhook identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"invoiceID">> => #{
      <<"name">> => <<"invoiceID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Invoice ID">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"paymentInstitutionID">> => #{
      <<"name">> => <<"paymentInstitutionID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Payment institution reference">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int32">>
    },
    <<"serviceProviderID">> => #{
      <<"name">> => <<"serviceProviderID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Service provider identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 100,
      <<"minLength">> => 1
    },
    <<"deadline">> => #{
      <<"name">> => <<"X-Request-Deadline">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Maximum request processing time">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"partyID">> => #{
      <<"name">> => <<"partyID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"The participant's unique identifier within the system.">>,
      <<"required">> => true,
      <<"type">> => <<"string">>
    },
    <<"countryID">> => #{
      <<"name">> => <<"countryID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<" Alpha-3 country code by standard [ISO 3166-1] (https://en.wikipedia.org/wiki/ISO_3166-1)">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>
    },
    <<"tradeBlocID">> => #{
      <<"name">> => <<"tradeBlocID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Trade bloc identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>
    }
  },
  <<"responses">> => #{
    <<"NotFound">> => #{
      <<"description">> => <<"Target resource not found">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/GeneralError">>
      }
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Authorization error">>
    },
    <<"DefaultLogicError">> => #{
      <<"description">> => <<"Invalid data">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/DefaultLogicError">>
      }
    },
    <<"ExternalIDConflict">> => #{
      <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/ExternalIDConflictError">>
      }
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": \"personType\",
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {\"$ref\": \"#/definitions/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": \"dummyType\",
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(deep_get([?DEFINITIONS, Parent, <<"properties">>, Discr, <<"enum">>], Schema)).

deep_get([K], M) ->
    maps:get(K, M);
deep_get([K | Ks], M) ->
    deep_get(Ks, maps:get(K, M)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw())),
       ?MODULE:get()
    ).
-endif.
