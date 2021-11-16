:- module(routes,[]).

/** <module> HTTP API
 *
 * The Terminus DB API interface.
 *
 * A RESTful endpoint inventory for weilding the full capabilities of the
 * terminusDB.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

%% TODO: this module should really only need things from core/api and maybe core/account.

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(core(account)).

% http libraries
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_stream)).


% multipart
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/mimepack)).

:- use_module(library(broadcast)).

% chunked
%:- use_module(library(http/http_header)).
%:- use_module(library(http/http_stream)).

% TUS
:- use_module(library(tus)).
:- (   file_upload_storage_path(Path)
   ->  set_tus_options([tus_storage_path(Path)])
   ;   true).

% Authentication library is only half used.
% and Auth is custom, not actually "Basic"
% Results should be cached!
:- use_module(library(http/http_authenticate)).

% Conditional loading of the JWT IO library...
:- if(config:jwt_enabled).
:- use_module(library(jwt_io)).
:- endif.

:- listen(http(Term), http_request_logger(Term)).

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set base location
% We may want to allow this as a setting...
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(root, '/', []).
http:location(api, '/api', []).

%%%%%%%%%%%%% Fallback Path %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler('/api', reply_404_not_found, [prefix]).

reply_404_not_found(Request) :-
    member(path(Path), Request),
    format(string(Msg),'Path not found: ~w', [Path]),
    reply_json(_{'api:status' : 'api:not_found',
                 'api:path' : Path,
                 'api:message' : Msg},
               [status(404)]).

%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(.), cors_handler(Method, connect_handler),
                [method(Method),
                 methods([options,get])]).

/**
 * connect_handler(+Method,+Request:http_request) is det.
 */
/* NOTE: Need to return list of databases and access rights */
connect_handler(get, Request, System_DB, Auth) :-
    findall(Database,
            user_accessible_database(System_DB, Auth, Database),
            Databases),

    write_cors_headers(Request),
    reply_json(Databases).

:- begin_tests(jwt_auth, [
                   condition(config:jwt_enabled)
               ]
              ).

:- use_module(core(util/test_utils)).
/*
 * Tests assume that  setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH", "test/public_key_test.key.pub")
 * setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID", "testkey") are set
 */
test(connection_authorized_user_jwt, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/'], URL),
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tLyIsInN1YiI6ImFkbWluIiwiYXVkIjpbImh0dHBzOi8vdGVybWludXNodWIvcmVnaXN0ZXJVc2VyIiwiaHR0cHM6Ly90ZXJtaW51c2h1Yi5ldS5hdXRoMC5jb20vdXNlcmluZm8iXSwiaWF0IjoxNTkzNzY5MTgzLCJhenAiOiJNSkpuZEdwMHpVZE03bzNQT1RRUG1SSkltWTJobzBhaSIsInNjb3BlIjoib3BlbmlkIHByb2ZpbGUgZW1haWwifQ.Ru03Bi6vSIQ57bC41n6fClSdxlb61m0xX6Q34Yh91gql0_CyfYRWTuqzqPMFoCefe53hPC5E-eoSFdID_u6w1ih_pH-lTTqus9OWgi07Qou3QNs8UZBLiM4pgLqcBKs0N058jfg4y6h9GjIBGVhX9Ni2ez3JGNcz1_U45BhnreE',
    http_get(URL, _, [authorization(bearer(Bearer))]).

test(connection_unauthorized_user_jwt, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/'], URL),

    % mangled the payload so it should not validate
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodW0000000000aDAuY29tLyIsInN1YiI6ImF1dGgwfDVlZmVmY2NkYmIzMzEzMDAxMzlkNTAzNyIsImF1ZCI6WyJodHRwczovL3Rlcm1pbnVzaHViL3JlZ2lzdGVyVXNlciIsImh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTU5Mzc2OTE4MywiYXpwIjoiTUpKbmRHcDB6VWRNN28zUE9UUVBtUkpJbVkyaG8wYWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.hxJphuKyWLbTLTgFq37tHQvNaxDwWxeOyDVbEemYoWDhBbSbjcjP034jJ0PhupYqtdadZV4un4j9QkJeYDLNtZLD7q4tErNK5bDw9gM1z9335BSu9htjLOZEF2_DqJYLGdbazWA3MAGkg6corOCXDVyBZpToekvylsGAMztkZaeAIgnsJlHxIIMMLQHrHNCBRPC1U6ZJQc7WZdgB-gefVlVQco0w8_Q0Z28DeshD9y3XChTMeTAAT-URwmz61RB6aUFMXpr4tTtYwyXGsWdu46LuDNxOV070eTybthDpDjyYSDsn-i4YbHvDGN5kUen9pw6b47CkSdhsSSjVQLsNkA',
    http_get(URL, _, [authorization(bearer(Bearer)), status_code(Status)]),

    Status = 401.

:- end_tests(jwt_auth).

:- begin_tests(connect_handler).
:- use_module(core(util/test_utils)).

test(connection_authorised_user_http_basic, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    admin_pass(Key),
    atomic_list_concat([Server, '/api/'], URL),
    http_get(URL, _, [authorization(basic(admin, Key))]).


test(connection_result_dbs, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    admin_pass(Key),
    atomic_list_concat([Server, '/api/'], URL),
    http_get(URL, Result, [json_object(dict),authorization(basic(admin, Key))]),

    Result = [].

:- end_tests(connect_handler).

%%%%%%%%%%%%%%%%%%%% Message Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(message), cors_handler(Method, message_handler),
                [method(Method),
                 methods([options,get,post])]).

/*
 * message_handler(+Method,+Request) is det.
 */
message_handler(_Method, Request, _System_DB, _Auth) :-
    try_get_param('api:message',Request,Message),

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Message, [])
    ),

    json_log_info_formatted('~N[Message] ~s~n',[Payload]),

    write_cors_headers(Request),

    reply_json(_{'api:status' : 'api:success'}).

%%%%%%%%%%%%%%%%%%%% Info Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(info), cors_handler(Method, info_handler),
                [method(Method),
                 methods([options,get])]).

info_handler(get, Request, System_DB, Auth) :-
    api_report_errors(
        info,
        Request,
        (   info(System_DB, Auth, Info),
            cors_reply_json(Request, _{'@type' : 'api:InfoResponse',
                                       'api:info' : Info,
                                       'api:status' : 'api:success'}))).


%%%%%%%%%%%%%%%%%%%% Ping Handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(ok), cors_handler(Method, ok_handler),
                [method(Method),
                 methods([options,get])]).

ok_handler(_Method, _Request, _System_DB, _Auth) :-
    format('Content-type: application/octets~n', []),
    format('Status: 200 OK~n~n', []).

%%%%%%%%%%%%%%%%%%%% Database Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(db/Account/DB), cors_handler(Method, db_handler(Account, DB)),
                [method(Method),
                 methods([options,post,delete])]).

/**
 * db_handler(Method:atom,DB:atom,Request:http_request) is det.
 */
db_handler(post, Organization, DB, Request, System_DB, Auth) :-
    /* POST: Create database */
    check_content_type_json(Request),
    get_payload(Database_Document,Request),
    do_or_die(
        (_{ comment : Comment,
            label : Label } :< Database_Document),
        error(bad_api_document(Database_Document,[comment,label]),_)),

    Default_Prefixes = _{ '@base' : "terminusdb:///data/",
                          '@schema' : "terminusdb:///schema#" },
    (   _{ prefixes : Input_Prefixes } :< Database_Document
    ->  Prefixes = Default_Prefixes.put(Input_Prefixes)
    ;   Prefixes = Default_Prefixes),

    (   _{ public : Public } :< Database_Document
    ->  true
    ;   Public = false),

    (   _{ schema : Schema } :< Database_Document
    ->  true
    ;   Schema = true),

    api_report_errors(
        create_db,
        Request,
        (   create_db(System_DB, Auth, Organization, DB, Label, Comment, Schema, Public, Prefixes),
            cors_reply_json(Request, _{'@type' : 'api:DbCreateResponse',
                                       'api:status' : 'api:success'}))).
db_handler(delete,Organization,DB,Request, System_DB, Auth) :-
    (   get_payload(Document,Request),
        _{ force: true} :< Document
    ->  Force_Delete = true
    ;   Force_Delete = false),

    /* DELETE: Delete database */
    api_report_errors(
        delete_db,
        Request,
        (   delete_db(System_DB, Auth, Organization, DB, Force_Delete),
            cors_reply_json(Request, _{'@type' : 'api:DbDeleteResponse',
                                       'api:status' : 'api:success'}))).

:- begin_tests(db_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(db_create, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ '@base' : "https://terminushub.com/document",
                           '@schema' : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic(admin, Key))]),
    _{'api:status' : "api:success"} :< In.

create_db_bad_prefixes(URI, Prefix_Name) :-
    dict_create(Prefixes, _, [Prefix_Name-"x"]),
    Doc = _{prefixes : Prefixes, comment : "c", label : "l"},
    admin_pass(Key),
    http_post(URI, json(Doc), JSON,
              [json_object(dict),
               authorization(basic(admin, Key)),
               status_code(Code)]),
    400 = Code,
    _{'@type' : "api:DbCreateErrorResponse", 'api:error' : Error} :< JSON,
    atom_string(Prefix_Name, Prefix_Name_String),
    _{'@type' : "api:InvalidPrefix",
      'api:prefix_name' : Prefix_Name_String,
      'api:prefix_value' : "x"} :< Error.

test(db_create_bad_prefixes, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    forall(member(Prefix_Name, ['@base', '@schema', other]),
           create_db_bad_prefixes(URI, Prefix_Name)).

test(db_create_bad_api_document, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ label : "A label" },
    admin_pass(Key),
    http_post(URI, json(Doc),
              JSON, [json_object(dict),
                   status_code(Code),
                   authorization(basic(admin, Key))]),
    400 = Code,
    _{'@type' : "api:BadAPIDocumentErrorResponse",
      'api:error' : Error} :< JSON,
    _{'@type' : "api:RequiredFieldsMissing"} :< Error.

test(db_create_existing_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_db_without_schema("admin", "TEST_DB"),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic(admin, Key)),
                       status_code(Status)]),

    Status = 400,
    _{'api:status' : "api:failure"} :< Result.

test(db_create_in_unknown_organization_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/THIS_ORG_DOES_NOT_EXIST/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic(admin, Key)),
                       status_code(Status)]),

    Status = 400,
    _{'api:status' : "api:failure"} :< Result.

test(db_create_unauthenticated_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic(admin, "THIS_IS_NOT_THE_CORRECT_PASSWORD")),
                       status_code(Status)]),
    Status = 401,
    _{'api:status' : "api:failure"} :< Result.

test(db_create_unauthorized_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user("TERMINUSQA",some('password'),_User_ID),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic("TERMINUSQA", "password")),
                       status_code(Status)]),

    Status = 403,
    _{'api:status' : "api:forbidden"} :< Result.

test(db_delete, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_db_without_schema("admin", "TEST_DB"),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI, Delete_In, [json_object(dict),
                                 authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In.

test(db_force_delete_unfinalized_system_only, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_context(system_descriptor{}, Context),
    with_transaction(Context,
                     insert_db_object(Context, "admin", "foo", "testdb", "test db", _),
                     _),
    database_exists("admin", "foo"),

    atomic_list_concat([Server, '/api/db/admin/foo'], URI),
    admin_pass(Key),
    http_get(URI,
             Delete_In,
             [method(delete),
              post(json(_{force:true})),
              json_object(dict),
              authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In,

    \+ database_exists("admin", "foo").

test(db_force_delete_unfinalized_system_and_label, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    super_user_authority(Auth),

    organization_database_name("admin","foo",Label),
    triple_store(Store),

    db_create:create_db_unfinalized(system_descriptor{},
                                    Auth, "admin", "foo",
                                    "dblabel", "db comment", false, true,
                                    _{'@base' : "http://foo/",
                                      '@schema' : "http://foo/s#"},
                                    _),
    database_exists("admin", "foo"),
    safe_open_named_graph(Store, Label, _),

    atomic_list_concat([Server, '/api/db/admin/foo'], URI),
    admin_pass(Key),
    http_get(URI,
             Delete_In,
             [method(delete),
              post(json(_{force:true})),
              json_object(dict),
              authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In,

    \+ database_exists("admin", "foo"),
    \+ safe_open_named_graph(Store, Label, _).

test(db_delete_unknown_organization_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/THIS_ORG_DOES_NOT_EXIST/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI,
                Result,
                [json_object(dict),
                 authorization(basic(admin, Key)),
                 status_code(Status)]),

    Status = 400,

    % TODO this test is actually equivalent to the one below.
    % We need to differentiate these errors better, but I don't want to validate the exact error message.
    % We need codes!
    _{'api:status' : "api:failure"} :< Result.

test(db_delete_nonexistent_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI,
                Result,
                [json_object(dict),
                 authorization(basic(admin, Key)),
                 status_code(Status)]),

    Status = 404,

    _{'api:status' : "api:not_found"} :< Result.


test(db_auth_test, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('TERMINUS_QA',some('password'),_User_ID),

    atomic_list_concat([Server, '/api/db/TERMINUS_QA/TEST_DB'], URI),
    Doc = _{ prefixes : _{ '@base' : "https://terminushub.com/document",
                           '@schema' : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },

    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic('TERMINUS_QA', "password"))]),
    _{'api:status' : "api:success"} :< In.

test(bad_json, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    Bad_JSON = "{\"x\"",
    http_post(URI, bytes('application/json', Bad_JSON),
              Return, [status_code(_),
                       json_object(dict),
                       authorization(basic(admin, Key))]),
    Return = _{'api:message':"Submitted object was not valid JSON",
               'api:status':"api:failure",'system:object':Bad_JSON}.

:- end_tests(db_endpoint).

%%%%%%%%%%%%%%%%%%%% Triples Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(triples/Path), cors_handler(Method, triples_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,get,post,put])]).

/*
 * triples_handler(Mode,DB,Request) is det.
 *
 * Get or update a graph with turtle.
 */
triples_handler(get,Path,Request, System_DB, Auth) :-
    (   get_param('format', Request, Format_Atom)
    ->  atom_string(Format_Atom,Format)
    ;   Format = "turtle"
    ),
    api_report_errors(
        triples,
        Request,
        (   graph_dump(System_DB, Auth, Path, Format, String),
            cors_reply_json(Request, String))).
triples_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Triples_Document,Request),
    do_or_die(_{ turtle : TTL,
                 commit_info : Commit_Info } :< Triples_Document,
              error(bad_api_document(Triples_Document,[turtle,commit_info]),_)),

    api_report_errors(
        triples,
        Request,
        (   graph_update(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesUpdateResponse',
                                       'api:status' : "api:success"}))).
triples_handler(put,Path,Request, System_DB, Auth) :-
    get_payload(Triples_Document,Request),
    do_or_die(_{ turtle : TTL,
                 commit_info : Commit_Info } :< Triples_Document,
              error(bad_api_document(Triples_Document,[turtle,commit_info]),_)),

    api_report_errors(
        triples,
        Request,
        (   graph_insert(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesInsertResponse',
                                       'api:status' : "api:success"}))).

:- begin_tests(triples_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(core(document)).

test(triples_update, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema(admin, 'TEST_DB'),
    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_instance.ttl'], TTL_File),
    interpolate([Path, '/terminus-schema/system_schema.json'], Schema_TTL_File),
    open(Schema_TTL_File, read, Stream),
    make_branch_descriptor(admin, 'TEST_DB', Branch_Descriptor),
    create_context(Branch_Descriptor, commit_info{ author: "me", message: "something"}, Ctx),
    with_transaction(Ctx,
                     replace_json_schema(Ctx,Stream),
                     _),
    close(Stream),
    % We actually have to create the graph before we can post to it!
    % First make the schema graph

    read_file_to_string(TTL_File, TTL, []),
    %Server2 = 'http://127.0.0.1:6363',
    %writeq(Server2),
    atomic_list_concat([Server, '/api/triples/admin/TEST_DB/local/branch/main/instance'], URI),
    admin_pass(Key),
    http_post(URI, json(_{commit_info : _{ author : "Test",
                                           message : "testing" },
                          turtle : TTL}),
              _In, [json_object(dict),
                    status_code(_),
                    authorization(basic(admin, Key)),
                    cert_verify_hook(cert_accept_any),
                    reply_header(_)]),

    findall(A-B-C,
            ask(Branch_Descriptor,
                t(A, B, C, "schema")),
            Triples),

    memberchk('http://terminusdb.com/schema/system#Capability'-(rdf:type)-(sys:'Class'), Triples),

    findall(A-B-C,
            ask(Branch_Descriptor,
                t(A, B, C)),
            Triples2),

    memberchk(system-(rdf:type)-'http://terminusdb.com/schema/system#SystemDatabase', Triples2).


test(triples_get, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/triples/_system/schema'], URI),
    admin_pass(Key),
    http_get(URI, In, [json_object(dict),
                       authorization(basic(admin, Key))]),
    string(In).


test(triples_put_two, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "Jumanji"),

    TTL = "
@prefix layer: <http://terminusdb.com/schema/layer#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

layer:LayerIdRestriction a owl:Restriction.",

    atomic_list_concat([Server, '/api/triples/admin/Jumanji/local/branch/main/instance'], URI),
    admin_pass(Key),

    http_put(URI, json(_{commit_info : _{ author : "Test",
                                           message : "testing" },
                         turtle : TTL}),
             _Result, [json_object(dict),
                       authorization(basic(admin, Key))]),

    TTL2 = "
@prefix layer: <http://terminusdb.com/schema/layer#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

layer:LayerIdRestriction2 a owl:Restriction.",

    http_put(URI, json(_{commit_info : _{ author : "Test",
                                          message : "testing" },
                         turtle : TTL2}),
             _Result2, [json_object(dict),
                        authorization(basic(admin, Key))]),

    http_get(URI, Result, [json_object(dict),
                           authorization(basic(admin, Key))]),

    once(sub_string(Result, _, _, _,
                    "LayerIdRestriction>\n  a owl:Restriction")),

    once(sub_string(Result, _, _, _,
                    "LayerIdRestriction2>\n  a owl:Restriction")).


test(get_invalid_descriptor, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/triples/nonsense'], URI),
    admin_pass(Key),

    http_get(URI, In, [json_object(dict),
                        authorization(basic(admin, Key)),
                        status_code(Code)]),
    _{'api:message':_Msg,
      'api:status':"api:failure"} :< In,
    Code = 400.


test(get_bad_descriptor, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/triples/admin/fdsa'], URI),
    admin_pass(Key),

    http_get(URI, In, [json_object(dict),
                        authorization(basic(admin, Key)),
                        status_code(Code)]),
    _{'api:message':_,
      'api:status':"api:failure"} :< In,
    Code = 400.

:- end_tests(triples_endpoint).

%%%%%%%%%%%%%%%%%%%% Document Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(document/Path), cors_handler(Method, document_handler(Path), [add_payload(false)]),
                [method(Method),
                 prefix,
                 methods([options,post,delete,get,put])]).

ensure_json_header_written(Request, As_List, Header_Written) :-
    Header_Written = written(Written),
    (   var(Written)
    ->  nb_setarg(1, Header_Written, true),
        write_cors_headers(Request),
        (   As_List = true
        ->  format("Content-type: application/json; charset=UTF-8~n~n", []),
            format("[~n")
        ;   format("Content-type: application/json; stream=true; charset=UTF-8~n~n", []))
    ;   true).

json_write_with_header(Request, Document, Header_Written, As_List, JSON_Options) :-
    % pretty hairy stuff just to support dumb parsers that can't deal with streaming json
    Header_Written = written(Written),
    (   var(Written)
    ->  First_Element = true
    ;   First_Element = false),
    ensure_json_header_written(Request, As_List, Header_Written),

    (   First_Element = false,
        As_List = true
    ->  format(",~n")
    ;   true),
    json_write_dict(current_output, Document, JSON_Options),

    % only print the newline here if we're not printing as a list.
    % In the case of list printing, the separators handle the newlines.
    (   As_List = true
    ->  true
    ;   nl).

document_handler(get, Path, Request, System_DB, Auth) :-
    (   json_content_type(Request),
        memberchk(content_length(_), Request)
    ->  http_read_json_data(Request, JSON)
    ;   JSON = json{}),

    do_or_die(is_dict(JSON),
              error(malformed_api_document(JSON), _)),

    api_report_errors(
        get_documents,
        Request,
        (
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_or_json_optional(Search, JSON, graph_type, graph, instance, Graph_Type),
            param_value_search_or_json_optional(Search, JSON, skip, nonnegative_integer, 0, Skip),
            param_value_search_or_json_optional(Search, JSON, count, nonnegative_integer, unlimited, Count),
            param_value_search_or_json_optional(Search, JSON, minimized, boolean, true, Minimized),
            param_value_search_or_json_optional(Search, JSON, as_list, boolean, false, As_List),
            param_value_search_or_json_optional(Search, JSON, prefixed, boolean, false, Prefixed),
            param_value_search_or_json_optional(Search, JSON, unfold, boolean, true, Unfold),
            param_value_search_or_json_optional(Search, JSON, id, atom, _, Id),
            param_value_search_or_json_optional(Search, JSON, type, atom, _, Type),

            param_value_json_optional(JSON, query, object, _, Query),

            (   Minimized = true
            ->  JSON_Options = [width(0)]
            ;   JSON_Options = []),

            % Compress is the logical negation of Prefixed.
            (   Prefixed = true
            ->  Compress = false
            ;   Compress = true),

            Header_Written = written(_),
            (   ground(Query)
            ->  forall(api_generate_documents_by_query(System_DB, Auth, Path, Graph_Type, Compress, Unfold, Type, Query, Skip, Count, Document),
                       json_write_with_header(Request, Document, Header_Written, As_List, JSON_Options))
            ;   ground(Id)
            ->  api_get_document(System_DB, Auth, Path, Graph_Type, Compress, Unfold, Id, Document),
                json_write_with_header(Request, Document, Header_Written, As_List, JSON_Options)
            ;   ground(Type)
            ->  forall(api_generate_documents_by_type(System_DB, Auth, Path, Graph_Type, Compress, Unfold, Type, Skip, Count, Document),
                       json_write_with_header(Request, Document, Header_Written, As_List, JSON_Options))
            ;   forall(api_generate_documents(System_DB, Auth, Path, Graph_Type, Compress, Unfold, Skip, Count, Document),
                       json_write_with_header(Request, Document, Header_Written, As_List, JSON_Options))),

            % ensure the header has been written by now.
            ensure_json_header_written(Request, As_List, Header_Written),

            (   As_List = true
            ->  format("~n]~n")
            ;   true))
        ).

document_handler(post, Path, Request, System_DB, Auth) :-
    memberchk(x_http_method_override('GET'), Request),
    !,
    document_handler(get, Path, Request, System_DB, Auth).
document_handler(post, Path, Request, System_DB, Auth) :-
    api_report_errors(
        insert_documents,
        Request,
        (
            check_content_type_json(Request),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_author(Search, Author),
            param_value_search_message(Search, Message),
            param_value_search_graph_type(Search, Graph_Type),
            param_value_search_optional(Search, full_replace, boolean, false, Full_Replace),

            http_read_data(Request, Data, [to(string)]),
            open_string(Data, Stream),
            api_insert_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Full_Replace, Stream, Ids),

            write_cors_headers(Request),
            reply_json(Ids),
            nl
        )).

document_handler(delete, Path, Request, System_DB, Auth) :-
    api_report_errors(
        delete_documents,
        Request,
        (
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_author(Search, Author),
            param_value_search_message(Search, Message),
            param_value_search_graph_type(Search, Graph_Type),
            param_value_search_optional(Search, nuke, boolean, false, Nuke),
            param_value_search_optional(Search, id, atom, _, Id),

            (   Nuke = true
            ->  api_nuke_documents(System_DB, Auth, Path, Graph_Type, Author, Message)
            ;   ground(Id)
            ->  api_delete_document(System_DB, Auth, Path, Graph_Type, Author, Message, Id)
            ;   json_content_type(Request),
                memberchk(content_length(_), Request)
            ->  http_read_data(Request, Data, [to(string)]),
                open_string(Data, Stream),
                api_delete_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Stream)
            ;   throw(error(missing_targets, _))
            ),

            write_cors_headers(Request),
            nl,nl
        )).

document_handler(put, Path, Request, System_DB, Auth) :-
    api_report_errors(
        replace_documents,
        Request,
        (
            check_content_type_json(Request),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_author(Search, Author),
            param_value_search_message(Search, Message),
            param_value_search_graph_type(Search, Graph_Type),
            param_value_search_optional(Search, create, boolean, false, Create),

            http_read_data(Request, Data, [to(string)]),
            open_string(Data, Stream),
            api_replace_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Stream, Create, Ids),

            write_cors_headers(Request),
            reply_json(Ids),
            nl
        )).

%%%%%%%%%%%%%%%%%%%% Frame Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(schema/Path), cors_handler(Method, frame_handler(Path), [add_payload(false)]),
                [method(Method),
                 prefix,
                 methods([options,get,post])]).

/**
 * frame_handler(+Mode, +DB, +Class_ID, +Request:http_request) is det.
 *
 * Establishes frame responses
 */
frame_handler(get, Path, Request, System_DB, Auth) :-
    % TODO This possibly throws a json error, which gets reinterpreted
    % as a schema check failure for some reason. gotta fix that.
    (   json_content_type(Request),
        memberchk(content_length(_), Request)
    ->  http_read_json_data(Request, Posted)
    ;   Posted = _{}),

    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),

    (   get_dict(type, Posted, Class_Uri)
    ->  Class = uri(Class_Uri)
    ;   memberchk(type=Class_Uri, Search)
    ->  Class = uri(Class_Uri)
    ;   Class = all
    ),

    api_report_errors(
        frame,
        Request,
        api_class_frame(System_DB, Auth, Path, Class, Frame)),

    write_cors_headers(Request),
    reply_json(Frame).

:- begin_tests(frame_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(get_frame, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/schema/_system'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ type : "User"
                    }),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               request_header('X-HTTP-Method-Override'='GET')
              ]),

    JSON =
    _{'@documentation':
      _{'@comment':"A database user.",
        '@properties':
        _{capability:"A set of capabilities which the user has access to.",
          key_hash:"An optional key hash for authentication.",
          name:"The users name."}},
      '@key':_{'@fields':["name"],'@type':"Lexical"},
      capability:_{'@class':"Capability",'@type':"Set"},
      key_hash:_{'@class':"xsd:string",'@type':"Optional"},
      name:"xsd:string"}.


:- end_tests(frame_endpoint).

%%%%%%%%%%%%%%%%%%%% WOQL Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
%
:- http_handler(api(woql), cors_handler(Method, woql_handler),
                [method(Method),
                 time_limit(infinite),
                 methods([options,post])]).
:- http_handler(api(woql/Path), cors_handler(Method, woql_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/**
 * woql_handler(+Method:atom, +Request:http_request) is det.
 *
 * Open WOQL with no defined database
 *
 * NOTE: This is not obtaining appropriate cors response data
 * from terminus database on spartacus.
 */
woql_handler(post, Request, System_DB, Auth) :-
    woql_handler_helper(Request, System_DB, Auth, none).

woql_handler(post, Path, Request, System_DB, Auth) :-
    woql_handler_helper(Request, System_DB, Auth, some(Path)).

woql_handler_helper(Request, System_DB, Auth, Path_Option) :-
    check_content_type_json(Request),
    try_get_param('query',Request,Query),

    (   get_param('commit_info', Request, Commit_Info)
    ->  true
    ;   Commit_Info = _{}
    ),
    collect_posted_files(Request,Files),

    (   get_param('all_witnesses', Request, All_Witnesses)
    ->  true
    ;   All_Witnesses = false),

    api_report_errors(
        woql,
        Request,
        (   woql_query_json(System_DB, Auth, Path_Option, Query, Commit_Info, Files, All_Witnesses, JSON),
            write_cors_headers(Request),
            reply_json_dict(JSON)
        )).

% woql_handler Unit Tests

:- begin_tests(woql_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(db_not_there, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/woql/admin/blagblagblagblagblag'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{'query' : ""}),
              JSON,
              [status_code(Code), json_object(dict),authorization(basic(admin,Key))]),
    Code = 404,
    _{'@type' : "api:WoqlErrorResponse",
      'api:error' :
      _{'@type' : "api:UnresolvableAbsoluteDescriptor",
        'api:absolute_descriptor': "admin/blagblagblagblagblag/local/branch/main"}} :< JSON.

test(branch_db, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema(admin,test),
    atomic_list_concat([Server, '/api/woql/admin/test'], URI),

    % TODO: We need branches to pull in the correct 'doc:' prefix.
    Query0 =
    _{'@type' : "AddTriple",
      subject: _{ '@type' : "NodeValue",
                  node: "test_subject"},
      'predicate' : _{ '@type' : "NodeValue",
                       node: "test_predicate"},
      'object' : _{ '@type' : "Value",
                    node: "test_object"}
     },
    * json_write_dict(current_output,Query0,[]),
    Commit = commit_info{ author : 'The Gavinator',
                          message : 'Peace and goodwill' },

    admin_pass(Key),
    http_post(URI,
              json(_{'query' : Query0,
                     'commit_info' : Commit }),
              JSON0,
              [json_object(dict),authorization(basic(admin,Key))]),

    _{bindings : [_{}], inserts: 1, deletes : 0} :< JSON0,

    % Now query the insert...
    Query1 =
    _{'@type' : "Triple",
      subject: _{'@type' : "NodeValue",
                 variable: "Subject"},
      predicate: _{'@type' : "NodeValue",
                   variable: "Predicate"},
      object: _{'@type' : "Value",
                variable: "Object"}},

    http_post(URI,
              json(_{query : Query1}),
              JSON1,
              [json_object(dict),authorization(basic(admin,Key))]),

    (   _{'bindings' : L} :< JSON1
    ->  L = [_{'Object':"test_object",
               'Predicate':"@schema:test_predicate",
               'Subject':"test_subject"}]
    ).

test(bad_cast, [
         setup((setup_temp_server(State, Server),
                create_db_with_test_schema("admin", "test"))),
         cleanup(teardown_temp_server(State))
     ])
:-

    Query0 =
    _{'@type' : 'And',
      and : [First_Insert,
             Second_Insert]},

    First_Insert =
    _{ '@type' : "AddTriple",
       subject: _{ '@type' : "NodeValue",
                   node: "test_subject"},
       predicate: _{ '@type' : "NodeValue",
                     node: "rdf:type"},
       object: _{ '@type' : "Value",
                  node: "BS"}
     },

    Second_Insert =
    _{ '@type' : "AddTriple",
       subject: _{ '@type' : "NodeValue",
                   node: "test_subject"},
       predicate: _{ '@type' : "NodeValue",
                     node: "rdf:label"},
       object: _{ '@type' : "Value",
                  data: _{ '@type' : "xsd:integer",
                           '@value' : "asdf"}}},

    admin_pass(Key),
    atomic_list_concat([Server, '/api/woql/admin/test'], URI),
    http_post(URI,
              json(_{query : Query0, all_witnesses: true}),
              JSON0,
              [json_object(dict),
               status_code(_),
               authorization(basic(admin,Key))]),
    "api:BadCast" = (JSON0.'api:error'.'@type').

:- end_tests(woql_endpoint).

%%%%%%%%%%%%%%%%%%%% Clone Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(clone/Organization/DB), cors_handler(Method, clone_handler(Organization, DB)),
                [method(Method),
                 methods([options,post])]).

clone_handler(post, Organization, DB, Request, System_DB, Auth) :-

    do_or_die(
        (get_payload(Database_Document,Request),
         _{ comment : Comment,
            label : Label,
            remote_url: Remote_URL} :< Database_Document),
        error(bad_api_document(Database_Document,[comment,label,remote_url]),_)),

    (   _{ public : Public } :< Database_Document
    ->  true
    ;   Public = false),

    api_report_errors(
        clone,
        Request,
        (   do_or_die(
                request_remote_authorization(Request, Authorization),
                error(no_remote_authorization,_)),

            clone(System_DB, Auth, Organization,DB,Label,Comment,Public,Remote_URL,authorized_fetch(Authorization),_Meta_Data),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:CloneResponse',
                  'api:status' : 'api:success'})
        )).

:- begin_tests(clone_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(clone_local, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    add_user("TERMINUSQA1",some('password1'),_User_ID1),
    add_user("TERMINUSQA2",some('password2'),_User_ID2),
    create_db_without_schema("TERMINUSQA1", "foo"),
    resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
    create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
    with_transaction(Foo_Context,
                     ask(Foo_Context,
                         insert(a,b,c)),
                     _),

    atomic_list_concat([Server, '/api/clone/TERMINUSQA2/bar'], URL),
    atomic_list_concat([Server, '/TERMINUSQA1/foo'], Remote_URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),

    http_post(URL,
              json(_{comment: "hai hello",
                     label: "bar",
                     remote_url: Remote_URL}),

              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote)]),

    * json_write_dict(current_output, JSON, []),

    _{
        'api:status' : "api:success"
    } :< JSON,

    resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
    once(ask(Bar_Descriptor,
             t(a,b,c))),

    true.

test(clone_remote, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-
    with_triple_store(
        Store_1,
        (   add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _)
        )
    ),

    with_triple_store(
        Store_2,
        (   add_user("TERMINUSQA2",some('password2'),_User_ID2)
        )
    ),

    atomic_list_concat([Server_2, '/api/clone/TERMINUSQA2/bar'], URL),
    atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),

    http_post(URL,
              json(_{comment: "hai hello",
                     label: "bar",
                     remote_url: Remote_URL}),

              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote)]),

    * json_write_dict(current_output, JSON, []),

    _{
        'api:status' : "api:success"
    } :< JSON,

    with_triple_store(
        Store_2,
        (   resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            once(ask(Bar_Descriptor,
                     t(a,b,c)))
        )
    ).


:- end_tests(clone_endpoint).

%%%%%%%%%%%%%%%%%%%% Fetch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(fetch/Path), cors_handler(Method, fetch_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

fetch_handler(post,Path,Request, System_DB, Auth) :-
    api_report_errors(
        fetch,
        Request,
        (   do_or_die(
                request_remote_authorization(Request, Authorization),
                error(no_remote_authorization_for_fetch,_)),

            remote_fetch(System_DB, Auth, Path, authorized_fetch(Authorization),
                         New_Head_Layer_Id, Head_Has_Updated),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:FetchRequest',
                  'api:status' : 'api:success',
                  'api:head_has_changed' : Head_Has_Updated,
                  'api:head' : New_Head_Layer_Id}))).

:- begin_tests(fetch_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(fetch_first_time, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON,

    with_triple_store(
        Store_2,
        (   resolve_absolute_string_descriptor("TERMINUSQA2/bar/origin", Bar_Descriptor_Origin),
            once(ask(Bar_Descriptor_Origin,
                     t(a,b,c))),
            repository_head(Bar_Database_Desc, "origin", Head)
        )
    ).

test(fetch_second_time_no_change, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON1,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),


    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON1,

    http_post(URL,
              json(_{}),
              JSON2,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : false,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON2,


    with_triple_store(
        Store_2,
        (
            repository_head(Bar_Database_Desc, "origin", Head)
        )
    ).

test(fetch_second_time_with_change, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON1,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),


    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON1,

    with_triple_store(
        Store_1,
        (
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context_Extra1),
            with_transaction(Foo_Context_Extra1,
                             ask(Foo_Context_Extra1,
                                 insert(d,e,f)),
                             _),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context_Extra2),
            with_transaction(Foo_Context_Extra2,
                             ask(Foo_Context_Extra2,
                                 insert(g,h,i)),
                             _),

            repository_head(Foo_Database_Desc,"local",New_Head)
        )),


    http_post(URL,
              json(_{}),
              JSON2,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': New_Head,
       'api:status' : "api:success"} :< JSON2,


    with_triple_store(
        Store_2,
        (
            repository_head(Bar_Database_Desc, "origin", New_Head)
        )
    ).

:- end_tests(fetch_endpoint).


%%%%%%%%%%%%%%%%%%%% Rebase Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(rebase/Path), cors_handler(Method, rebase_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

rebase_handler(post, Path, Request, System_DB, Auth) :-
    check_content_type_json(Request),
    get_payload(Document,Request),
    do_or_die(
        (_{ author : Author,
            rebase_from : Their_Path } :< Document),
        error(bad_api_document(Document,[author,rebase_from]),_)),

    api_report_errors(
        rebase,
        Request,
        (   Strategy_Map = [],
            rebase_on_branch(System_DB, Auth, Path, Their_Path, Author, Strategy_Map, Common_Commit_ID_Option, Forwarded_Commits, Reports),

            Incomplete_Reply = _{ '@type' : "api:RebaseResponse",
                                  'api:status' : "api:success",
                                  'api:forwarded_commits' : Forwarded_Commits,
                                  'api:rebase_report': Reports
                                },

            (   Common_Commit_ID_Option = some(Common_Commit_ID)
            ->  put_dict(_{ 'api:common_commit_id' : Common_Commit_ID},
                         Incomplete_Reply,
                         Reply)
            ;   Reply = Incomplete_Reply),
            cors_reply_json(Request, Reply, [status(200)]))).

:- begin_tests(rebase_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(rebase_divergent_history, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    add_user("TERMINUSQA",some('password'),User_ID),
    create_db_without_schema("TERMINUSQA", "foo"),

    Master_Path = "TERMINUSQA/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),

    Second_Path = "TERMINUSQA/foo/local/branch/second",
    branch_create(system_descriptor{}, User_ID, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    % we're also doing a commit on the original branch, to create a divergent history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),
    atomic_list_concat([Server, '/api/rebase/TERMINUSQA/foo'], URI),
    http_post(URI,
              json(_{rebase_from: 'TERMINUSQA/foo/local/branch/second',
                     author : "Gavsky"}),
              JSON,
              [json_object(dict),
               authorization(basic('TERMINUSQA','password')),
               status_code(_Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    _{  '@type' : "api:RebaseResponse",
        'api:forwarded_commits' : [_Thing, _Another_Thing ],
        'api:common_commit_id' : _Common_Something,
        'api:rebase_report' : _Reports,
        'api:status' : "api:success"
    } :< JSON,

    Repository_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor, Commit_Uri, [Commit_A, Commit_B, Commit_C, Commit_D]),

    commit_id_to_metadata(Repository_Descriptor, Commit_A, "test", "commit a", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_B, "test", "commit b", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_C, "test", "commit c", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_D, "Gavsky", "commit d", _).
:- end_tests(rebase_endpoint).

%%%%%%%%%%%%%%%%%%%% Pack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(pack/Path), cors_handler(Method, pack_handler(Path)),
                [method(Method),
                 time_limit(infinite),
                 chunked,
                 methods([options,post])]).

pack_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Document,Request),

    (   _{ repository_head : Layer_ID } :< Document
    ->  Repo_Head_Option = some(Layer_ID)
    ;   Repo_Head_Option = none),

    api_report_errors(
        pack,
        Request,
        pack(System_DB, Auth,
             Path, Repo_Head_Option, Payload_Option)),

    (   Payload_Option = some(Payload)
    ->  format('Content-type: application/octets~n', []),
        format('Status: 200 OK~n~n', []),
        format('~s', [Payload])
    ;   format('Content-type: application/octets~n', []),
        format('Status: 204 No Response~n~n', [])
    ).

% Currently just sending binary around...
:- begin_tests(pack_endpoint).
:- use_module(core(util/test_utils)).
%:- use_module(core(transaction)).
%:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(terminus_store)).

test(pack_stuff, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('_a_test_user_',some('password'),_User_ID),
    create_db_without_schema('_a_test_user_',foo),

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),

    % First commit
    create_context(Descriptor, commit_info{author:"user",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                     _),

    % Second commit
    create_context(Descriptor, commit_info{author:"user",message:"commit b"}, Master_Context2),
    % Before updating, grab the repository head layer_ID
    [Branch_Transaction] = (Master_Context2.transaction_objects),
    Repository = (Branch_Transaction.parent),
    repository_head_layerid(Repository,Repository_Head_Layer_ID),

    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),


    atomic_list_concat([Server, '/api/pack/_a_test_user_/foo'], URI),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    http_post(URI,
              json(Document),
              Data,
              [authorization(basic('_a_test_user_','password'))]),

    payload_repository_head_and_pack(Data, Head, Pack),

    % Check pack validity
    create_context(Descriptor, commit_info{author:"user",message:"commit b"}, New_Head_Context),
    % grab the repository head layer_ID and new graph layer_ID
    [New_Head_Transaction] = (New_Head_Context.transaction_objects),
    New_Head_Repository = (New_Head_Transaction.parent),
    repository_head_layerid(New_Head_Repository,New_Repository_Head_Layer_Id),
    [Instance_Graph] = (New_Head_Transaction.instance_objects),
    Layer = (Instance_Graph.read),
    layer_to_id(Layer,Layer_Id),

    pack_layerids_and_parents(Pack,Layerids_and_Parents),

    memberchk(New_Repository_Head_Layer_Id-_,Layerids_and_Parents),
    memberchk(Layer_Id-_,Layerids_and_Parents),
    memberchk(Head-_,Layerids_and_Parents),

    Head = New_Repository_Head_Layer_Id.


test(pack_nothing, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('_a_test_user_',some('password'),_User_ID),
    create_db_without_schema('_a_test_user_','foo'),

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),
    open_descriptor(Repository_Descriptor, Repo_Transaction),
    repository_head_layerid(Repo_Transaction, Repository_Head_Layer_ID),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    atomic_list_concat([Server, '/api/pack/_a_test_user_/foo'], URI),
    http_post(URI,
              json(Document),
              _Data,
              [authorization(basic('_a_test_user_','password')),status_code(Status)]),
    Status = 204.

:- end_tests(pack_endpoint).

%%%%%%%%%%%%%%%%%%%% Unpack Handlers %%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(unpack/Path), cors_handler(Method, unpack_handler(Path)),
                [method(Method),
                 chunked,
                 time_limit(infinite),
                 methods([options,post])]).

unpack_handler(post, Path, Request, System_DB, Auth) :-

    % This really should use API versioning
    do_or_die(
        (   get_payload(Document, Request),
            (   (   is_dict(Document),
                    _{ resource_uri : Resource_Uri } :< Document,
                    Resource_Or_Payload = resource(Resource_Uri)
                )
            ->  true
            ;   Resource_Or_Payload = payload(Document)
            )
        ),
        error(bad_api_document(Document,[resource_uri]),_)),

    api_report_errors(
        unpack,
        Request,
        (   unpack(System_DB, Auth, Path, Resource_Or_Payload),
            cors_reply_json(Request,
                            _{'@type' : 'api:UnpackResponse',
                              'api:status' : "api:success"},
                            [status(200)])
        )).

%:- begin_tests(unpack_endpoint).
%:- end_tests(unpack_endpoint).

%%%%%%%%%%%%%%%%%%%% TUS Handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(files), auth_wrapper(tus_dispatch),
                [ methods([options,head,post,patch,delete]),
                  prefix
                ]).

:- meta_predicate auth_wrapper(2,?).
auth_wrapper(Goal,Request) :-
    open_descriptor(system_descriptor{}, System_Database),
    catch((      authenticate(System_Database, Request, Auth),
                 www_form_encode(Auth, Domain),
                 call(Goal, [domain(Domain)], Request)),
          error(authentication_incorrect(_Reason),_),
          (   reply_json(_{'@type' : 'api:ErrorResponse',
                           'api:status' : 'api:failure',
                           'api:error' : _{'@type' : 'api:IncorrectAuthenticationError'},
                           'api:message' : 'Incorrect authentication information'
                          },
                         [status(401)]))),
    !.

%%%%%%%%%%%%%%%%%%%% Push Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(push/Path), cors_handler(Method, push_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

push_handler(post,Path,Request, System_DB, Auth) :-
    do_or_die(
        (  get_payload(Document, Request),
           _{ remote : Remote_Name,
              remote_branch : Remote_Branch } :< Document),
        error(bad_api_document(Document,[remote,remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Authorization),
        error(no_remote_authorization)),

    (   get_dict(push_prefixes, Document, true)
    ->  Push_Prefixes = true
    ;   Push_Prefixes = false),

    api_report_errors(
        push,
        Request,
        (   push(System_DB, Auth, Path, Remote_Name, Remote_Branch, [prefixes(Push_Prefixes)],
                 authorized_push(Authorization),Result),
            (   Result = same(Head_ID)
            ->  Head_Updated = false
            ;   Result = new(Head_ID)
            ->  Head_Updated = true
            ;   throw(error(internal_server_error,_))),

            Response =  _{'@type' : "api:PushResponse",
                          'api:repo_head_updated' : Head_Updated,
                          'api:repo_head' : Head_ID,
                          'api:status' : "api:success"},

            cors_reply_json(Request,
                            Response,
                            [status(200)]))).

:- begin_tests(push_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(push_empty_to_empty_does_nothing_succesfully,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    \+ get_dict('api:head', JSON, _),

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON.

test(push_empty_with_prefix_change_to_empty_changes_prefixes,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),

    with_triple_store(
        Store_Origin,
        (   create_context((Origin_Branch_Descriptor.repository_descriptor),
                           Origin_Repository_Context),
            with_transaction(Origin_Repository_Context,
                             update_prefixes(Origin_Repository_Context,
                                             _{doc: "http://this_is_docs/",
                                               scm: "http://this_is_scm/",
                                               foo: "http://this_is_foo/"}),
                             _))),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main",
                       push_prefixes: true
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': true,
      'api:repo_head': _,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Destination,
        (   repository_prefixes((Destination_Branch_Descriptor.repository_descriptor),
                                Prefixes),
            Prefixes = _{doc: 'http://this_is_docs/',
                         scm: 'http://this_is_scm/',
                         foo: 'http://this_is_foo/'})).

test(push_nonempty_to_empty_advances_remote_head,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),
    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse", % todo this should actually not be that
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON.

test(push_nonempty_to_same_nonempty_keeps_remote_head_unchanged,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_nonempty_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse", % todo this should actually not be that
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)).

test(push_nonempty_to_earlier_nonempty_advances_remote_head,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_nonempty_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    with_triple_store(
        Store_Origin,
        (   create_context(Origin_Branch_Descriptor, commit_info{author:"Rosa",message:"hello"}, Origin_Context_1),
            with_transaction(Origin_Context_1,
                             ask(Origin_Context_1,
                                 insert(g,h,i)),
                             _),
            create_context(Origin_Branch_Descriptor, commit_info{author:"Rosa",message:"hello again"}, Origin_Context_2),
            with_transaction(Origin_Context_2,
                             ask(Origin_Context_2,
                                 insert(j,k,l)),
                             _)
        )
    ),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': true,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON,

    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)).

:- end_tests(push_endpoint).

%%%%%%%%%%%%%%%%%%%% Pull Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(pull/Path), cors_handler(Method, pull_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

pull_handler(post,Path,Request, System_DB, Local_Auth) :-
    % Can't we just ask for the default remote?
    do_or_die(
        (   get_payload(Document, Request),
            _{ remote : Remote_Name,
               remote_branch : Remote_Branch_Name
             } :< Document),
        error(bad_api_document(Document, [remote, remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Remote_Auth),
        error(no_remote_authorization)),

    api_report_errors(
        pull,
        Request,
        (   pull(System_DB, Local_Auth, Path, Remote_Name, Remote_Branch_Name,
                 authorized_fetch(Remote_Auth),
                 Result),
            JSON_Base = _{'@type' : 'api:PullResponse',
                          'api:status' : "api:success"},
            put_dict(Result,JSON_Base,JSON_Response),
            cors_reply_json(Request,
                            JSON_Response,
                            [status(200)]))).

:- begin_tests(pull_endpoint, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(pull_from_empty_to_empty,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Database_Descriptor = (Local_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Database_Descriptor = (Remote_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        repository_head(Local_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Remote,
        repository_head(Remote_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : false,
      'api:status':"api:success"} :< JSON.

test(pull_from_something_to_empty,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Remote,
        (   create_context(Remote_Branch_Descriptor, commit_info{author:"KarlKautsky", message:"Boo!"}, Remote_Context_1),
            with_transaction(
                Remote_Context_1,
                ask(Remote_Context_1,
                    insert(a,b,c)),
                _),
            repository_head(Remote_Database_Descriptor, "local", Head),
            branch_head_commit(Remote_Repository_Descriptor, "main", Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Descriptor, Commit_Id, Remote_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_fast_forwarded",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri),
            commit_id_uri(Local_Repository_Descriptor, Commit_Id, Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        branch_head_commit(Local_Repository_Descriptor, "main", Local_Orginal_Commit_Uri)
    ),

    with_triple_store(
        Store_Remote,
        (   create_context(Remote_Branch_Descriptor, commit_info{author:"KarlKautsky", message:"Boo!"}, Remote_Context_1),
            with_transaction(
                Remote_Context_1,
                ask(Remote_Context_1,
                    insert(e,f,g)),
                _),
            repository_head(Remote_Database_Descriptor, "local", Head),
            branch_head_commit(Remote_Repository_Descriptor, "main", Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Descriptor, Commit_Id, Remote_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_fast_forwarded",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri),
            \+ Local_Orginal_Commit_Uri = Local_Commit_Uri,
            commit_id_uri(Local_Repository_Descriptor, Commit_Id, Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something_equal,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : false,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something_equal_other_branch,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Remote,
        (
            agent_name_uri(system_descriptor{}, "KarlKautsky", User_Uri),
            branch_create(system_descriptor{}, User_Uri, "KarlKautsky/foo/local/branch/other", branch("KarlKautsky/foo/local/branch/main"), _),
            repository_head(Remote_Database_Descriptor, "local", Head)
        )
    ),

    with_triple_store(
        Store_Local,
        (
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ).


:- end_tests(pull_endpoint).

%%%%%%%%%%%%%%%%%%%% Branch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(branch/Path), cors_handler(Method, branch_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

branch_handler(post, Path, Request, System_DB, Auth) :-
    do_or_die(
        get_payload(Document, Request),
        error(bad_api_document(Document, []),_)),

    (   get_dict(origin, Document, Origin_Path)
    ->  Origin_Option = branch(Origin_Path)
    ;   ignore(get_dict(prefixes, Document, Input_Prefixes)),
        ignore(get_dict(schema, Document, Schema)),
        Origin_Option = empty(Input_Prefixes, Schema)),

    api_report_errors(
        branch,
        Request,
        (   branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri),
            cors_reply_json(Request,
                            _{'@type' : 'api:BranchResponse',
                              'api:status' : "api:success"}))).
branch_handler(delete, Path, Request, System_DB, Auth) :-
    api_report_errors(
        branch,
        Request,
        (   branch_delete(System_DB, Auth, Path),
            cors_reply_json(Request,
                            _{'@type' : 'api:BranchResponse',
                              'api:status' : "api:success"}))).

:- begin_tests(branch_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(create_empty_branch, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}
                    }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_from_local_without_prefixes, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/main'}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_from_local_with_prefixes, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/main',
                     prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}
                    }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_that_already_exists_error, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/main'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/main',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []).

test(create_branch_from_nonexisting_origin_error, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/bar',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").

test(create_branch_from_commit_graph_error, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'admin/test/local/_commits',
                     prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),
    Status_Code = 400,

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").

test(delete_empty_branch, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-

    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}
                    }),
              _JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo"),

    http_get(URI,
             JSON2,
             [method(delete),
              json_object(dict),
              authorization(basic(admin,Key))]),

    (JSON2.'api:status' = "api:success"),
    \+ has_branch(Repository_Descriptor, "foo").

:- end_tests(branch_endpoint).

%%%%%%%%%%%%%%%%%%%% Prefix Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

:- http_handler(api(prefixes/Path), cors_handler(Method, prefix_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,get])]).

% this allows the client to discover prefixes for a given resource
% from an endpoint, but also regularises the use of JSON-LD contexts
% which can then include the endpoint for context discovery
prefix_handler(get, Path, Request, System_DB, Auth) :-
    api_report_errors(
        prefix,
        Request,
        (   get_prefixes(Path, System_DB, Auth, Prefixes),
            cors_reply_json(Request,
                            Prefixes,
                            [status(200)]))).

:- begin_tests(prefixes_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(create_graph, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),

    atomic_list_concat([Server, '/api/prefixes/admin/test'], URI),
    admin_pass(Key),
    http_get(URI,
             JSON,
             [json_object(dict),authorization(basic(admin,Key))]),

    _{'@base':"http://somewhere.for.now/document/",
      '@schema':"http://somewhere.for.now/schema#",
      '@type':"Context"} :< JSON.

:- end_tests(prefixes_endpoint).

%%%%%%%%%%%%%%%%%%%% User handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(user), cors_handler(Method, user_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(api(user/Name), cors_handler(Method, user_handler(Name)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

user_handler(post, Name, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    api_report_errors(
        user_update,
        Request,
        (   update_user_transaction(System_DB, Auth, Name, Document),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateUserResponse",
                              'api:status' : "api:success"}))).
user_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        user_delete,
        Request,
        (   delete_user_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteUserResponse",
                              'api:status' : "api:success"}))).

user_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ agent_name : Agent_Name } :< Document,
              error(malformed_update_user_document(Document, [agent_name]))
             ),

    api_report_errors(
        user_update,
        Request,
        (   update_user_transaction(System_DB, Auth, Agent_Name, Document),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateUserResponse",
                              'api:status' : "api:success"}))).
user_handler(delete, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ agent_name : Agent_Name },
              error(malformed_user_deletion_document(Document))
             ),

    api_report_errors(
        user_delete,
        Request,
        (   delete_user_transaction(System_DB, Auth, Agent_Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteUserResponse",
                              'api:status' : "api:success"}))).

%%%%%%%%%%%%%%%%%%%% Organization handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% THIS IS A *POTENTIALLY* TEMPORARY HANDLER - YOU SHOULD NOT RELY ON THIS YET
%
:- http_handler(api(user_organizations), cors_handler(Method, user_organizations_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).

user_organizations_handler(get, Request, System_DB, Auth) :-
    api_report_errors(
        user_organizations,
        Request,
        (   user_organizations(System_DB, Auth, Result),
            cors_reply_json(Request, Result)
        )
    ).

%%%%%%%%%%%%%%%%%%%% Organization handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(organization), cors_handler(Method, organization_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(api(organization/Name), cors_handler(Method, organization_handler(Name)),
                [method(Method),
                 prefix,
                 methods([options,delete])]).

organization_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ organization_name : Org,
                 user_name : User } :< Document,
              error(bad_api_document(Document, [organization_name, user_name]))
             ),

    api_report_errors(
        add_organization,
        Request,
        (   add_user_organization_transaction(System_DB, Auth, User, Org),
            cors_reply_json(Request,
                            _{'@type' : "api:AddOrganizationResponse",
                              'api:status' : "api:success"}))).
organization_handler(delete, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ organization_name : Name },
              error(malformed_organization_deletion_document(Document))
             ),

    api_report_errors(
        delete_organization,
        Request,
        (   delete_organization_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"}))).

organization_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        delete_organization,
        Request,
        (   delete_organization_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"}))).


%%%%%%%%%%%%%%%%%%%% Squash handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(squash/Path), cors_handler(Method, squash_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * squash_handler(Mode,Path,Request,System,Auth) is det.
 *
 * Squash commit chain to a new commit
 */
squash_handler(post, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ commit_info : Commit_Info } :< Document),
        error(bad_api_document(Document, [commit_info]), _)),

    api_report_errors(
        squash,
        Request,
        (   api_squash(System_DB, Auth, Path, Commit_Info, Commit, Old_Commit)
        ->  cors_reply_json(Request, _{'@type' : 'api:SquashResponse',
                                       'api:commit' : Commit,
                                       'api:old_commit' : Old_Commit,
                                       'api:status' : "api:success"})
        ;   cors_reply_json(Request, _{'@type' : 'api:EmptySquashResponse',
                                       'api:empty_commit' : true,
                                       'api:status' : "api:success"})
        )).

:- begin_tests(squash_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(terminus_store)).

test(squash_a_branch, [
         setup((setup_temp_server(State, Server),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_server(State))
     ]) :-

    Path = "admin/test",
    atomic_list_concat([Server, '/api/squash/admin/test'], URI),

    resolve_absolute_string_descriptor(Path, Descriptor),

    Commit_Info_1 = commit_info{
                      author : "me",
                      message: "first commit"
                  },

    create_context(Descriptor, Commit_Info_1, Context1),

    with_transaction(
        Context1,
        ask(Context1,
            insert(a,b,c)),
        _),

    Commit_Info_2 = commit_info{
                        author : "me",
                        message: "second commit"
                    },

    create_context(Descriptor, Commit_Info_2, Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(e,f,g)),
        _),

    descriptor_commit_id_uri((Descriptor.repository_descriptor),
                             Descriptor,
                             Commit_Id, _Commit_Uri),

    Commit_Info = commit_info{
                      author : "me",
                      message: "Squash"
                  },

    admin_pass(Key),
    http_post(URI,
              json(_{ commit_info: Commit_Info}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    JSON = _{'@type':"api:SquashResponse",
             'api:commit': New_Commit_Path,
             'api:old_commit' : Old_Commit_Path,
             'api:status':"api:success"},
    resolve_absolute_string_descriptor(New_Commit_Path, Commit_Descriptor),
    open_descriptor(Commit_Descriptor, Transaction),

    [RWO] = (Transaction.instance_objects),
    Layer = (RWO.read),
    \+ parent(Layer,_),
    findall(X-Y-Z,ask(Commit_Descriptor,t(X,Y,Z)), Triples),
    sort(Triples,Sorted),
    sort([a-b-c,e-f-g],Correct),
    ord_seteq(Sorted, Correct),

    resolve_absolute_string_descriptor(Old_Commit_Path, Old_Commit_Descriptor),
    commit_descriptor{ commit_id : Commit_Id } :< Old_Commit_Descriptor.

:- end_tests(squash_endpoint).

%%%%%%%%%%%%%%%%%%%% Reset handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(reset/Path), cors_handler(Method, reset_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * reset_handler(Mode, Path, Request, System, Auth) is det.
 *
 * Reset a branch to a new commit.
 */
reset_handler(post, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ commit_descriptor : Ref } :< Document),
        error(bad_api_document(Document, [commit_descriptor]), _)),

    api_report_errors(
        reset,
        Request,
        (   api_reset(System_DB, Auth, Path, Ref),
            cors_reply_json(Request, _{'@type' : 'api:ResetResponse',
                                       'api:status' : "api:success"}))).

:- begin_tests(reset_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(library(terminus_store)).

test(reset, [
         setup((setup_temp_server(State, Server),
                create_db_without_schema("admin", "testdb"))),
         cleanup(teardown_temp_server(State))
     ]) :-

    Path = "admin/testdb",

    resolve_absolute_string_descriptor(Path,Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),

    super_user_authority(Auth),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 1" },
                    Context),

    with_transaction(
        Context,
        ask(Context,
            insert(a,b,c)),
        _),

    descriptor_commit_id_uri(Repository_Descriptor,
                             Descriptor,
                             Commit_Id,
                             _Commit_Uri),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 2" },
                    Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(e,f,g)),
        _),

    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit",Commit_Id],
                                Commit_Descriptor),

    resolve_absolute_string_descriptor(Ref,Commit_Descriptor),

    atomic_list_concat([Server, '/api/reset/admin/testdb'], URI),

    admin_pass(Key),
    http_post(URI,
              json(_{ commit_descriptor : Ref }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    JSON = _{'@type':"api:ResetResponse",
             'api:status':"api:success"},

    findall(X-Y-Z,
            ask(Descriptor,
                t(X,Y,Z)),
            Triples),

    [a-b-c] = Triples.

:- end_tests(reset_endpoint).


%%%%%%%%%%%%%%%%%%%% Optimize handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(optimize/Path), cors_handler(Method, optimize_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * optimize_handler(Mode,DB,Request) is det.
 *
 * Optimize a resource
 */
optimize_handler(post, Path, Request, System_DB, Auth) :-
    api_report_errors(
        optimize,
        Request,
        (   api_optimize(System_DB, Auth, Path),
            cors_reply_json(Request, _{'@type' : 'api:OptimizeResponse',
                                       'api:status' : "api:success"}))).

:- begin_tests(optimize_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(library(terminus_store)).

test(optimize_system, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    create_db_without_schema("admin", "test2"),
    atomic_list_concat([Server, '/api/optimize/_system'], URI),

    admin_pass(Key),
    http_post(URI,
              json(_{}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    JSON = _{'@type':"api:OptimizeResponse",
             'api:status':"api:success"},

    open_descriptor(system_descriptor{}, Transaction),
    [RWO] = (Transaction.instance_objects),
    Layer = (RWO.read),
    \+ parent(Layer,_).

:- end_tests(optimize_endpoint).


%%%%%%%%%%%%%%%%%%%% Reset handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(remote/Path), cors_handler(Method, remote_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post,put,get,delete])]).

/*
 * reset_handler(Mode, Path, Request, System, Auth) is det.
 *
 * Reset a branch to a new commit.
 */
remote_handler(post, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ remote_name : Remote_Name,
               remote_location : URL
             } :< Document),
        error(bad_api_document(Document, [remote_name, remote_location]), _)),

    api_report_errors(
        remote,
        Request,
        (   add_remote(System_DB, Auth, Path, Remote_Name, URL),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:status' : "api:success"}))).
remote_handler(delete, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ remote_name : Remote_Name } :< Document),
        error(bad_api_document(Document, [remote_name]), _)),

    api_report_errors(
        remote,
        Request,
        (   remove_remote(System_DB, Auth, Path, Remote_Name),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:status' : "api:success"}))).
remote_handler(put, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ remote_name : Remote_Name,
               remote_location : URL
             } :< Document),
        error(bad_api_document(Document, [remote_name]), _)),

    api_report_errors(
        remote,
        Request,
        (   update_remote(System_DB, Auth, Path, Remote_Name, URL),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:status' : "api:success"}))).
remote_handler(get, Path, Request, System_DB, Auth) :-

    api_report_errors(
        remote,
        Request,
        (   get_param(remote_name,Request,Remote_Name)
        ->  show_remote(System_DB, Auth, Path, Remote_Name, Remote_URL),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:remote_name' : Remote_Name,
                                       'api:remote_url' : Remote_URL,
                                       'api:status' : "api:success"})
        ;   list_remotes(System_DB, Auth, Path, Remote_Names),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:remote_names' : Remote_Names,
                                       'api:status' : "api:success"}))).


:- begin_tests(remote_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(library(terminus_store)).

test(remote_add, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/remote/admin/test'], URI),

    Origin = "http://somewhere.com/admin/foo",

    admin_pass(Key),
    http_post(URI,
              json(_{ remote_name : origin,
                      remote_location : Origin
                    }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    JSON = _{'@type':"api:RemoteResponse",
             'api:status':"api:success"},

    super_user_authority(Auth),
    show_remote(system_descriptor{}, Auth, 'admin/test', origin, Origin).

test(remote_remove, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/remote/admin/test'], URI),

    Origin = "http://somewhere.com/admin/foo",

    admin_pass(Key),
    http_post(URI,
              json(_{ remote_name : origin,
                      remote_location : Origin
                    }),
              _JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    http_get(URI,
             JSON,
             [method(delete),
              post(json(_{ remote_name : origin })),
              json_object(dict),
              authorization(basic(admin,Key))]),

    JSON = _{'@type':"api:RemoteResponse",
             'api:status':"api:success"},

    super_user_authority(Auth),
    list_remotes(system_descriptor{}, Auth, 'admin/test', []).

test(remote_set, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/remote/admin/test'], URI),

    Origin = "http://somewhere.com/admin/foo",

    admin_pass(Key),
    http_post(URI,
              json(_{ remote_name : origin,
                      remote_location : Origin
                    }),
              _JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    New_Origin = "http://somewhere.com/admin/foo",

    http_put(URI,
             json(_{ remote_name : origin,
                           remote_location : New_Origin
                   }),
             JSON,
             [json_object(dict),
              authorization(basic(admin,Key))]),

    JSON = _{'@type':"api:RemoteResponse",
             'api:status':"api:success"},

    super_user_authority(Auth),
    show_remote(system_descriptor{}, Auth, 'admin/test', origin, New_Origin).

test(remote_get, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/remote/admin/test'], URI),

    Origin = "http://somewhere.com/admin/foo",

    admin_pass(Key),
    http_post(URI,
              json(_{ remote_name : origin,
                      remote_location : Origin
                    }),
              _JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    atomic_list_concat([URI, '?remote_name=origin'], GET_URI),
    http_get(GET_URI,
             JSON,
             [json_object(dict),
              authorization(basic(admin,Key))]),

    _{ 'api:remote_name' : "origin",
       'api:remote_url' : Origin } :< JSON.

test(remote_list, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/remote/admin/test'], URI),

    Origin = "http://somewhere.com/admin/foo",

    admin_pass(Key),
    http_post(URI,
              json(_{ remote_name : origin,
                      remote_location : Origin
                    }),
              _JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    http_get(URI,
             JSON,
             [json_object(dict),
              authorization(basic(admin,Key))]),

    _{ 'api:remote_names' : ["origin"]} :< JSON.



:- end_tests(remote_endpoint).

%%%%%%%%%%%%%%%%%%%% Console Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(.), cors_handler(Method, console_handler),
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(db), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(home), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(clone), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(collaborate), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(newdb), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(profile), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(hub), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).

/*
 * console_handler(+Method,+Request) is det.
 */
console_handler(get, _Request, _System_DB, _Auth) :-
    config:index_template(Index),
    throw(http_reply(bytes('text/html', Index))).

:- begin_tests(console_route).
:- use_module(core(util/test_utils)).

test(console_route, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s/", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

test(console_route_empty, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

test(console_route_db, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s/db/gavin/baseball", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

test(console_route_home, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s/home/somewhere", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

:- end_tests(console_route).

%%%%%%%%%%%%%%%%%%%% Reply Hackery %%%%%%%%%%%%%%%%%%%%%%
:- meta_predicate cors_handler(+,2,?).
:- meta_predicate cors_handler(+,2,?,+).
cors_handler(Method, Goal, Request) :-
    cors_handler(Method, Goal, [], Request).
cors_handler(options, _Goal, _Options, Request) :-
    !,
    write_cors_headers(Request),
    format('~n').
cors_handler(_Old_Method, Goal, Options, Request) :-
    select(x_http_method_override(Method), Request, New_Request),
    !,
    downcase_atom(Method,Mapped),
    cors_handler(Mapped, Goal, Options, New_Request).
cors_handler(Method, Goal, Options, R) :-
    cors_catch(
        R,
        (
            (   memberchk(Method, [post, put, delete]),
                \+ memberchk(add_payload(false), Options)
            ->  add_payload_to_request(R,Request)
            ;   Request = R),

            open_descriptor(system_descriptor{}, System_Database),
            catch((   authenticate(System_Database, Request, Auth),
                      call_http_handler(Method, Goal, Request, System_Database, Auth)),

                  error(authentication_incorrect(Reason),_),

                  (   write_cors_headers(Request),
                      json_log_error_formatted("~NAuthentication Incorrect for reason: ~q~n", [Reason]),
                      reply_json(_{'@type' : 'api:ErrorResponse',
                                   'api:status' : 'api:failure',
                                   'api:error' : _{'@type' : 'api:IncorrectAuthenticationError'},
                                   'api:message' : 'Incorrect authentication information'
                                  },
                                 [status(401)]))))),
    !.
cors_handler(_Method, Goal, _Options, R) :-
    write_cors_headers(R),
    format(string(Msg), "Failed to run the API endpoint goal ~q", Goal),
    reply_json(_{'@type' : 'api:ErrorResponse',
                 'api:status' : 'api:failure',
                 'api:error' : _{'@type' : 'api:APIEndpointFailed'},
                 'api:message' : Msg
                },
               [status(500)]).

% Evil mechanism for catching, putting CORS headers and re-throwing.
:- meta_predicate cors_catch(+, 0).
:- meta_predicate call_http_handler(+,3,?,?,?).
cors_catch(Request, Goal) :-
    catch(Goal,
          E,
          (
              write_cors_headers(Request),
              customise_exception(E)
          )),
    !.
cors_catch(Request, _Goal) :-
    write_cors_headers(Request),
    % Probably should extract the path from Request
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' :'Unexpected failure in request handler'},
               [status(500)]).

call_http_handler(Method, Goal, Request, System_Database, Auth) :-
    strip_module(Goal, Module, PlainGoal),
    PlainGoal =.. [Head|Args],
    NewArgs = [Method|Args],
    NewPlainGoal =.. [Head|NewArgs],
    NewGoal = Module:NewPlainGoal,

    call(NewGoal, Request, System_Database, Auth).

customise_exception(reply_json(M,Status)) :-
    reply_json(M,
               [status(Status)]).
customise_exception(reply_json(M)) :-
    customise_exception(reply_json(M,200)).
customise_exception(error(E)) :-
    generic_exception_jsonld(E,JSON),
    json_http_code(JSON,Status),
    reply_json(JSON,[status(Status)]).
customise_exception(error(E,_)) :-
    generic_exception_jsonld(E,JSON),
    json_http_code(JSON,Status),
    reply_json(JSON,[status(Status)]).
customise_exception(http_reply(method_not_allowed(JSON))) :-
    reply_json(JSON,[status(405)]).
customise_exception(http_reply(not_found(JSON))) :-
    reply_json(JSON,[status(404)]).
customise_exception(http_reply(authorize(JSON))) :-
    reply_json(JSON,[status(401)]).
customise_exception(http_reply(not_acceptable(JSON))) :-
    reply_json(JSON,[status(406)]).
customise_exception(time_limit_exceeded) :-
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : 'Connection timed out'
               },
               [status(408)]).
customise_exception(error(E)) :-
    format(atom(EM),'Error: ~q', [E]),
    reply_json(_{'api:status' : 'api:server_error',
                 'api:message' : EM},
               [status(500)]).
customise_exception(error(E, CTX)) :-
    json_log_error_formatted('~N[Exception] ~q~n',[error(E,CTX)]),
    (   CTX = context(prolog_stack(Stack),_)
    ->  with_output_to(
            string(Ctx_String),
            print_prolog_backtrace(current_output,Stack))
    ;   format(string(Ctx_String), "~q", [CTX])),
    format(atom(EM),'Error: ~q~n~s~n', [E, Ctx_String]),
    reply_json(_{'api:status' : 'api:server_error',
                 'api:message' : EM},
               [status(500)]).
customise_exception(http_reply(Obj)) :-
    throw(http_reply(Obj)).
customise_exception(E) :-
    json_log_error_formatted('~N[Exception] ~q~n',[E]),
    throw(E).

/*
 * api_error_http_reply(API,Error,Request) is false + exception.
 *
 * Throw an API appropriate JSON-LD response message.
 *
 */
api_error_http_reply(API, Error, Request) :-
    api_error_jsonld(API,Error,JSON),
    json_http_code(JSON,Status),
    cors_reply_json(Request,JSON,[status(Status)]).

api_error_http_reply(API, Error, Type, Request) :-
    api_error_jsonld(API,Error,Type,JSON),
    json_http_code(JSON,Status),
    cors_reply_json(Request,JSON,[status(Status)]).

:- meta_predicate api_report_errors(?,?,0).
api_report_errors(API,Request,Goal) :-
    catch_with_backtrace(
        Goal,
        Error,
        do_or_die(api_error_http_reply(API,Error,Request),
                  Error)
    ).

%%%%%%%%%%%%%%%%%%%% Access Rights %%%%%%%%%%%%%%%%%%%%%%%%%

/*
 *  fetch_authorization_data(+Request, -KS) is semi-determinate.
 *
 *  Fetches the HTTP Basic Authorization data
 */
fetch_authorization_data(Request, Username, KS) :-
    memberchk(authorization(Text), Request),
    http_authorization_data(Text, basic(Username, Key)),
    coerce_literal_string(Key, KS).

:- if(config:jwt_enabled).
/*
 *  fetch_jwt_data(+Request, -Username) is semi-determinate.
 *
 *  Fetches the HTTP JWT data
 */
fetch_jwt_data(Token, Username) :-
    atom_string(TokenAtom, Token),

    do_or_die(jwt_decode(TokenAtom, Payload, []),
              error(authentication_incorrect(jwt_decode_failed(TokenAtom)), _)),

    do_or_die(
        (   atom_json_dict(Payload, PayloadDict, []),
            % replace with dict key get (or whatever it is called)
            get_dict('http://terminusdb.com/schema/system#agent_name', PayloadDict, UsernameString),
            atom_string(Username, UsernameString)),
        error(malformed_jwt_payload(Payload))).
:- else.
fetch_jwt_data(_Token, _Username) :-
    throw(error(authentication_incorrect(jwt_authentication_requested_but_no_key_configured),_)).
:- endif.


/*
 * authenticate(+Database, +Request, -Auth_Obj) is det.
 *
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message.
 */
authenticate(System_Askable, Request, Auth) :-
    fetch_authorization_data(Request, Username, KS),
    !,

    (   user_key_user_id(System_Askable, Username, KS, Auth)
    ->  true
    ;   format(string(Message), "User '~w' failed to authenticate through basic auth", Username),
        json_log_debug(_{
                           message: Message,
                           authMethod: basic,
                           authResult: failure,
                           user: Username
                       }),

        throw(error(authentication_incorrect(basic_auth(Username)),_))),

    format(string(Message), "User '~w' authenticated through basic auth", Username),
    json_log_debug(_{
                       message: Message,
                       authMethod: basic,
                       authResult: success,
                       user: Username
                   }).
authenticate(System_Askable, Request, Auth) :-
    memberchk(authorization(Text), Request),
    pattern_string_split(" ", Text, ["Bearer", Token]),
    !,
    % Try JWT if no http keys
    fetch_jwt_data(Token, Username),
    (   username_auth(System_Askable, Username, Auth)
    ->  true
    ;   format(string(Message), "User '~w' failed to authenticate through JWT", Username),
        json_log_debug(_{
                           message: Message,
                           authMethod: jwt,
                           authResult: failure,
                           user: Username
                       }),
        throw(error(authentication_incorrect(jwt_no_user_with_name(Username)),_))),

    format(string(Message), "User '~w' authenticated through JWT", Username),
    json_log_debug(_{
                       message: Message,
                       authMethod: jwt,
                       authResult: success,
                       user: Username
                   }).
authenticate(_, _, anonymous) :-
    json_log_debug(_{
                       message: "User 'anonymous' authenticated as no authentication information was submitted",
                       authMethod: anonymous,
                       authResult: success,
                       user: "anonymous"
                   }).

/*
 * write_cors_headers(Request) is det.
 *
 * Writes cors headers associated with Resource_URI
 */
write_cors_headers(Request) :-
    (   memberchk(origin(Origin), Request)
    ->  current_output(Out),
        format(Out,'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\n',[]),
        format(Out,'Access-Control-Allow-Credentials: true\n',[]),
        format(Out,'Access-Control-Max-Age: 1728000\n',[]),
        format(Out,'Access-Control-Allow-Headers: Authorization, Authorization-Remote, Accept, Accept-Encoding, Accept-Language, Host, Origin, Referer, Content-Type, Content-Length, Content-Range, Content-Disposition, Content-Description\n',[]),
        format(Out,'Access-Control-Allow-Origin: ~s~n',[Origin])
    ;   true).

cors_reply_json(Request, JSON) :-
    write_cors_headers(Request),
    reply_json(JSON).

cors_reply_json(Request, JSON, Options) :-
    write_cors_headers(Request),
    reply_json(JSON, Options).

%%%%%%%%%%%%%%%%%%%% Response Predicates %%%%%%%%%%%%%%%%%%%%%%%%%

/********************************************************
 * Determinising predicates used in handlers            *
 *                                                      *
 ********************************************************/

/*
 * try_get_param(Key,Request:request,Value) is det.
 *
 * Get a parameter from the request independent of request variety.
 */
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),
    memberchk(multipart(Form_Data), Request),
    !,
    member(mime(Mime_Header,Encoded_Value,_),Form_Data),
    memberchk(name(Key), Mime_Header),
    (   json_mime_type(Mime_Header)
    ->  atom_json_dict(Encoded_Value,Value,[])
    ;   uri_encoded(query_value, Value, Encoded_Value)).
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   memberchk(Method, [post,put]),
        \+ json_content_type(Request)),

    http_parameters(Request, [], [form_data(Data)]),

    (   memberchk(Key=Value,Data)
    <>  throw(error(no_parameter_key_in_query_parameters(Key,Data)))),
    !.
try_get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    json_content_type(Request),

    (   memberchk(payload(Document), Request)
        <>  throw(error(no_document_for_key(Key)))),

    (   get_dict(Key,Document,Value)
        <>  throw(error(no_parameter_key_in_document(Key,Document)))),
    !.
try_get_param(Key,Request,_Value) :-
    % OTHER with method
    memberchk(method(Method), Request),
    !,
    throw(error(no_parameter_key_for_method(Key,Method))).
try_get_param(Key,_Request,_Value) :-
    % Catch all.
    throw(error(no_parameter_key(Key))).

json_content_type(Request) :-
    memberchk(content_type(CT), Request),
    re_match('^application/json', CT, []).

check_content_type(Request, Expected, ContentType) :-
    do_or_die(
        memberchk(content_type(ContentType), Request),
        error(missing_content_type(Expected), _)).

check_content_type_json(Request) :-
    Expected = 'application/json',
    check_content_type(Request, Expected, ContentType),
    atom_concat('^', Expected, RE),
    do_or_die(
        re_match(RE, ContentType, []),
        error(bad_content_type(ContentType, Expected), _)).

json_mime_type(Mime) :-
    memberchk(type(CT),Mime),
    re_match('^application/json', CT, []).

/*
 * get_param_default(Key,Request:request,Value,Default) is semidet.
 *
 * We can fail with this one, so you better do your own checking.
 */
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    memberchk(multipart(Form_Data), Request),
    !,
    memberchk(mime(Mime_Header,Encoded_Value,_),Form_Data),
    memberchk(name(Key), Mime_Header),
    (   json_mime_type(Mime_Header)
    ->  atom_json_dict(Encoded_Value,Value,[])
    ;   uri_encoded(query_value, Value, Encoded_Value)).
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   memberchk(Method, [post,put]),
        \+ json_content_type(Request)),

    http_parameters(Request, [], [form_data(Data)]),
    memberchk(Key=Value,Data),
    !.
get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    json_content_type(Request),
    memberchk(payload(Document), Request),
    Value = Document.get(Key).

http_read_json_data(Request, JSON) :-
    http_read_data(Request, JSON_String, [to(string)]),
    catch(atom_json_dict(JSON_String, JSON, []),
          error(syntax_error(json(_Kind)),_),
          throw(error(malformed_json_payload(JSON_String), _))
         ).

/*
 * add_payload_to_request(Request:request,JSON:json) is det.
 *
 * Updates request with JSON-LD payload in payload(Document).
 * This should really be done automatically at request time
 * using the endpoint wrappers so we don't forget to do it.
 */
add_payload_to_request(Request,[multipart(Form_Data)|Request]) :-
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)
    ),
    !,
    http_read_data(Request, Form_Data, [on_filename(save_post_file),form_data(mime)]).
add_payload_to_request(Request,[payload(Document)|Request]) :-
    json_content_type(Request),
    !,
    http_read_json_data(Request, Document).
add_payload_to_request(Request,[payload(Document)|Request]) :-
    memberchk(content_type(_Some_Other_Type), Request),
    !,
    http_read_data(Request, Document, []).
add_payload_to_request(Request,Request).

get_payload(Payload,Request) :-
    memberchk(payload(Payload),Request),
    !.
get_payload(Payload,Request) :-
    memberchk(multipart(Form_Data),Request),
    member(mime(Meta,Value,_),Form_Data),
    memberchk(name(payload), Meta),
    atom_json_dict(Value,Payload, []).

/*
 * request_remote_authorization(Request, Authorization) is det.
 */
request_remote_authorization(Request, Token) :-
    memberchk(authorization_remote(Token),Request).

/*
 * save_post_file(In,File_Spec,Options) is det.
 *
 * Saves a temporary octet stream to a file. Used for multipart
 * file passing via POST.
 */
save_post_file(In, file(Filename, File), Options) :-
    option(filename(Filename), Options),
    setup_call_cleanup(
        tmp_file_stream(octet, File, Out),
        copy_stream_data(In, Out),
        close(Out)
    ).

/*
 * Make a collection of all posted files for
 * use in a Context via WOQL's get/3.
 */
collect_posted_files(Request,Files) :-
    memberchk(multipart(Parts), Request),
    !,
    convlist([mime(Mime_Header,Data,_),Filename=Temp_Filename]>>(
                 memberchk(filename(Filename),Mime_Header),
                 \+ json_mime_type(Mime_Header),
                 setup_call_cleanup(
                     tmp_file_stream(octet, Temp_Filename, Out),
                     write(Out, Data),
                     close(Out)
                 )
             ),Parts,Files).
collect_posted_files(_Request,[]).

/*
 * Make a collection of all posted files for
 * use in a Context via csv interface
 */
collect_posted_named_files(Request,Files) :-
    memberchk(multipart(Parts), Request),
    !,
    convlist([mime(Mime_Header,Data,_),Name=Temp_Filename]>>(
                 memberchk(filename(_),Mime_Header),
                 memberchk(name(Name),Mime_Header),
                 \+ json_mime_type(Mime_Header),
                 setup_call_cleanup(
                     tmp_file_stream(octet, Temp_Filename, Out),
                     write(Out, Data),
                     close(Out)
                 )
             ),Parts,Files).
collect_posted_named_files(_Request,[]).


%% Logging

match_http_info(method(Method), Method_Upper, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id) :-
    string_upper(Method, Method_Upper).
match_http_info(protocol(Protocol), _Method, Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(host(Host), _Method, _Protocol, Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(port(Port), _Method, _Protocol, _Host, Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(request_uri(Url_Suffix), _Method, _Protocol, _Host, _Port, Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(peer(Peer), _Method, _Protocol, _Host, _Port, _Url_Suffix, Remote_Ip, _User_Agent, _Size, _Operation_Id) :-
    % what about ipv6 though?
    % is there any other sort of peer possible?
    Peer = ip(N1,N2,N3,N4),
    format(string(Remote_Ip), "~w.~w.~w.~w", [N1, N2, N3, N4]).
match_http_info(user_agent(User_Agent), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, User_Agent, _Size, _Operation_Id).
match_http_info(content_length(Size), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, Size_String, _Operation_Id) :-
    term_string(Size, Size_String).
match_http_info(x_operation_id(Operation_Id), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, Operation_Id_String) :-
    term_string(Operation_Id, Operation_Id_String).
match_http_info(_, _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).

extract_http_info_([], _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
extract_http_info_([First|Rest], Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size, Operation_Id) :-
    match_http_info(First, Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size, Operation_Id),
    !,
    extract_http_info_(Rest, Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size, Operation_Id).

extract_http_info(Request, Method, Url, Path, Remote_Ip, User_Agent, Size, Operation_Id) :-
    extract_http_info_(Request, Method, Protocol, Host, Port, Path, Remote_Ip, User_Agent, Size, Operation_Id),

    (   var(Port)
    ->  format(string(Url), "~w://~w~w", [Protocol, Host, Path])
    ;   format(string(Url), "~w://~w:~w~w", [Protocol, Host, Port, Path])).


get_current_id_from_stream(Id) :-
    current_output(CGI),
    cgi_property(CGI, id(Id)).

save_request(Request) :-
    get_time(Now),
    get_current_id_from_stream(Id),
    extract_http_info(Request, Method, Url, Path, Remote_Ip, User_Agent, Size, Submitted_Operation_Id),
    (   var(Submitted_Operation_Id)
    ->  Operation_Id = none
    ;   Operation_Id = some(Submitted_Operation_Id)),

    include([_-V]>>(nonvar(V)), [requestMethod-Method,
                                 requestUrl-Url,
                                 requestSize-Size,
                                 remoteIp-Remote_Ip,
                                 userAgent-User_Agent
                                ],
           Http_Pairs),
    assert(saved_request(Id, Now, Path, Operation_Id, Http_Pairs)).

:- multifile http:request_expansion/2.
http:request_expansion(Request, Request) :-
    save_request(Request).

http_request_logger(_) :-
    % Skip work if info log is not enabled
    \+ info_log_enabled,
    !,
    true.
http_request_logger(request_start(Local_Id, Request)) :-
    extract_http_info(Request, Method, _Url, Path, _Remote_Ip, _User_Agent, _Size, Submitted_Operation_Id),
    generate_request_id(Local_Id, Request_Id),

    (   var(Submitted_Operation_Id)
    ->  Operation_Id = first(Request_Id)
    ;   Operation_Id = Submitted_Operation_Id),

    format(string(Message), "Request ~w started - ~w ~w", [Request_Id, Method, Path]),

    include([_-V]>>(nonvar(V)), [method-Method,
                                 path-Path,
                                 message-Message
                                ],
            Dict_Pairs),
    dict_create(Dict, json, Dict_Pairs),
    json_log_debug(Operation_Id,
                  Request_Id,
                   Dict).

http_request_logger(request_finished(Local_Id, Code, _Status, _Cpu, Bytes)) :-
    term_string(Bytes, Bytes_String),

    saved_request(Local_Id, Start, Path, Submitted_Operation_Id, Initial_Http_Pairs),
    retract(saved_request(Local_Id, Start, Path, Submitted_Operation_Id, Initial_Http_Pairs)),
    get_time(Now),
    Latency is Now - Start,
    format(string(Latency_String), "~9fs", [Latency]),
    Http_Pairs = [
        status-Code,
        responseSize-Bytes_String,
        latency-Latency_String
        |Initial_Http_Pairs],

    dict_create(Http, json, Http_Pairs),

    generate_request_id(Local_Id, Request_Id),
    (   Submitted_Operation_Id = some(Operation_Id)
    ->  true
    ;   Operation_Id = last(Request_Id)),

    format(string(Message), "~w ~w (~w)", [Http.requestMethod, Path, Code]),
    json_log_info(Operation_Id,
                  Request_Id,
                  json{
                      httpRequest: Http,
                      message: Message
                  }).
