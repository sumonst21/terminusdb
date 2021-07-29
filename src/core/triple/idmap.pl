:- module(idmap,
          [ force_value/1,

            register_element_id/4,
            register_layer_subject_id/3,
            register_layer_predicate_id/3,
            register_layer_object_id/3,

            subject_layer_id/3,
            predicate_layer_id/3,
            object_layer_id/3,

            subject_value/2,
            predicate_value/2,
            object_value/2,

            idmap/2,
            xsdtype/2
          ]).
:- use_module(library(terminus_store)).
:- use_module(literals).

register_element_id(Type, Layer, X, Id) :-
    % Fully ground so we should be able to obtain the id now.
    ground(X),
    !,
    (   atom(X)
    ->  atom_string(X,S),
        (   Type = object
        ->  Result = node(S)
        ;   Result = S),
        type_id(Type,Layer,Result,Id)
    ;   Type = object,
        (   X = _^^_
        ;   X = _@_)
    ->  object_storage(X, Result),
        type_id(Type,Layer,Result,Id)
    ).
register_element_id(Type,Layer,X,Id) :-
    % We have an id map already, so we can find ourselves there.
    var(X),
    get_attr(X, idmap_map, IdMap),
    type_idspace(Type,Type_Space),
    layer_to_id(Layer, Layer_Id),
    memberchk(Layer_Id-_-Type_Space-Id,IdMap),
    !.
register_element_id(Type,Layer,X,Id) :-
    % We do not have an id map, or are not in it.
    var(X),
    get_attr(X, idmap_value, _Value),
    !,
    register_value_in_idmap(Type,X,Layer),
    % this should be safe as we *must* either fail to find an id
    % in type_id
    % or acquire an idmap.
    register_element_id(Type,Layer,X,Id).
register_element_id(Type,Layer,X,Id) :-
    var(X),
    !,
    % We have no value or idmap yet.
    layer_to_id(Layer, Layer_Id),
    when(ground(Id),
         (   type_idspace(Type, Id_Space),
             idmap(X, [Layer_Id-Layer-Id_Space-Id])
         )).
register_element_id(Type,Layer,X,Id) :-
    nonvar(X),
    (   X = _^^_
    ;   X = _@_),
    !,
    when(ground(Id),
         (   type_id(Type, Layer, Val, Id),
             type_value_convert(Type, Val, Object),
             X = Object
         )).

register_layer_subject_id(Layer,X,Id) :-
    register_element_id(subject,Layer,X,Id).

register_layer_predicate_id(Layer, X, Id) :-
    register_element_id(predicate,Layer,X,Id).

register_layer_object_id(Layer, X, Id) :-
    register_element_id(object,Layer,X,Id).

force_value(X) :-
    ground(X),
    !.
force_value(X) :-
    nonvar(X),
    !,
    fail.
force_value(X) :-
    get_attr(X, idmap_value, Val),
    !,
    (   string(Val)
    ->  atom_string(X,Val)
    ;   storage_object(Val, Obj),
        X = Obj).
force_value(X) :-
    get_attr(X, idmap_map, IdMap),
    !,
    memberchk(_-Layer-Space-Id,IdMap),
    type_idspace(Type,Space),
    type_id(Type,Layer,Result,Id),
    (   Type = subject
    ->  atom_string(Val,Result)
    ;   Type = predicate
    ->  atom_string(Val,Result)
    ;   Type = object
    ->  storage_object(Result, Val)
    ),
    !,
    X = Val.

idmap(X, Map) :-
    (   var(Map)
    ->  get_attr(X, idmap_map, Map)
    ;   put_attr(Y, idmap_map, Map),
        % force unification to resolve
        X = Y
    ).

subject_value(X,Value) :-
    (   var(Value)
    ->  get_attr(X, idmap_value, Pre_Value),
        string(Pre_Value),
        Value = Pre_Value
    ;   atom(Value)
    ->  atom_string(Value, String),
        put_attr(Y, idmap_value, String),
        X = Y
    ).

predicate_value(X,Value) :-
    (   var(Value)
    ->  get_attr(X, idmap_value, Pre_Value),
        string(Pre_Value),
        Value = Pre_Value
    ;   atom(Value)
    ->  atom_string(Value, String),
        put_attr(Y, idmap_value, String),
        X = Y
    ).

object_value(X,Value) :-
    (   var(Value)
    ->  get_attr(X, idmap_value, Pre_Value),
        (   Pre_Value = node(Node)
        ->  Value = Node
        ;   storage_object(Pre_Value,Value)
        )
    ;   ground(Value)
    ->  object_storage(Value, S),
        put_attr(Y, idmap_value, S),
        X = Y
    ;   (   Value = _^^_
        ;   Value = _@_)
    ->  true
    ).

type_value_convert(subject, Value, Result) :-
    atom_string(Result, Value).
type_value_convert(predicate, Value, Result) :-
    atom_string(Result, Value).
type_value_convert(object, Value, Result) :-
    storage_object(Value, Result).

type_id(subject, Layer, Subject, Id) :-
    subject_id(Layer, Subject, Id).
type_id(predicate, Layer, Predicate, Id) :-
    predicate_id(Layer, Predicate, Id).
type_id(object, Layer, Object, Id) :-
    object_id(Layer, Object, Id).

type_idspace(subject, so).
type_idspace(predicate, p).
type_idspace(object, so).

register_value_in_idmap(Type,X,Layer) :-
    (   get_attr(X, idmap_value, Val)
    ->  layer_to_id(Layer, L),
        type_idspace(Type,ID_Space),
        (   get_attr(X, idmap_map, IDMap)
        ->  (   memberchk(L-_-ID_Space-_,IDMap)
            ->  true
            ;   type_id(Type,Layer,Val,Id),
                merge_id_map([L-Layer-ID_Space-Id],IDMap,New_IDMap),
                put_attr(X, idmap_map, New_IDMap)
            )
        ;
            type_id(Type,Layer,Val,Id),
            put_attr(X, idmap_map, [L-Layer-ID_Space-Id])
        )
    ;   true
    ).

canonize(String,Atom) :-
    string(String),
    !,
    atom_string(Atom,String).
canonize(C,C).

type_layer_id(Type,X,Layer,Id) :-
    get_attr(X, idmap_map, IDMap),
    layer_to_id(Layer, Layer_Id),
    type_idspace(Type,Id_Space),
    memberchk([Layer_Id-_-Id_Space-Id], IDMap).

subject_layer_id(X,Layer,Id) :-
    type_layer_id(subject, X, Layer, Id).

predicate_layer_id(X,Layer,Id) :-
    type_layer_id(predicate, X, Layer, Id).

object_layer_id(X,Layer,Id) :-
    type_layer_id(object, X, Layer, Id).

register_subject(X,Layer) :-
    register_value_in_idmap(subject,X,Layer).

register_predicate(X,Layer) :-
    register_value_in_idmap(predicate,X,Layer).

register_object(X,Layer) :-
    register_value_in_idmap(object,X,Layer).

merge_id_map([], [], []).
merge_id_map([L-Layer-Type-Id|Rest], [], [L-Layer-Type-Id|Rest]).
merge_id_map([], [L-Layer-Type-Id|Rest], [L-Layer-Type-Id|Rest]).
merge_id_map([L1-Layer1-Type1-Id1|Rest1],[L2-Layer2-Type2-Id2|Rest2], [L1-Layer1-Type1-Id1|Rest]) :-
    L1 @< L2,
    !,
    merge_id_map(Rest1,[L2-Layer2-Type2-Id2|Rest2],Rest).
merge_id_map([L1-Layer1-Type1-Id1|Rest1],[L2-Layer2-Type2-Id2|Rest2], [L2-Layer2-Type2-Id2|Rest]) :-
    L1 @> L2,
    !,
    merge_id_map([L1-Layer1-Type1-Id1|Rest1],Rest2,Rest).
merge_id_map([L-Layer-Type-Id|Rest1],[L-_-Type-Id|Rest2], [L-Layer-Type-Id|Rest]) :-
    merge_id_map(Rest1,Rest2,Rest).

value_and_idmap(_, []).
value_and_idmap(Value, [_-Layer-Type-Id|_]) :-
    (   Type = so
    ->  (   atom(Value)
        ->  atom_string(Value,String),
            subject_id(Layer,String,Id)
        ;   (   Value = _^^_
            ;   Value = _@_),
            object_storage(Value, O),
            object_id(Layer,O,Id)
        )
    ;   Type = p
    ->  atom(Value),
        atom_string(Value,String),
        predicate_id(Layer,String,Id)
    ).

xsdtype(X, XSD_Type) :-
    var(XSD_Type),
    !,
    get_attr(X, idmap_type, XSD_Type).
xsdtype(X, XSD_Type) :-
    put_attr(Y, idmap_type, XSD_Type),
    X = Y.

:- public idmap_map:attr_unify_hook/2,
          idmap_map:attribute_goals/3,
          idmap_value:attr_unify_hook/2,
          idmap_value:attribute_goals/3,
          idmap_type:attr_unify_hook/2,
          idmap_type:attribute_goals/3.


%       An attributed variable with attribute value Domain has been
%       assigned the value Y
idmap_map:attr_unify_hook(IDMap, Y) :-
    (   ground(Y)
    ->  value_and_idmap(Y, IDMap)
    ;   nonvar(Y)
    ->  put_attr(X, idmap_map, IDMap),
        force_value(X),
        X = Y
    ;   get_attr(Y, idmap_map, IDMap2)
    ->  (   get_attr(Y, idmap_value, Value)
        ->  value_and_idmap(Value, IDMap)
        ;   true),
        merge_id_map(IDMap,IDMap2,New_IDMap),
        put_attr(Y, idmap_map, New_IDMap)
    ;   (   get_attr(Y, idmap_value, Value)
        ->  value_and_idmap(Value, IDMap)
        ;   true),
        put_attr(Y, idmap_map, IDMap)
    ).

%       Translate attributes from this module to residual goals

idmap_map:attribute_goals(X) -->
    { get_attr(X, idmap_map, List) },
    [ idmap(X, List) ].

idmap_value:attr_unify_hook(Value, Y) :-
    (   var(Y)
    ->  (   get_attr(Y,idmap_value,V)
        ->  V = Value
        ;   put_attr(Y,idmap_value,Value))
    ;   ground(Y)
    ->  (   atom(Y)
        ->  (   atom_string(Y,Value)
            ->  true
            ;   atom_string(Y,Node),
                Value = node(Node))
        ;   object_storage(Y,Value))
    ).

idmap_value:attribute_goals(X) -->
        { get_attr(X, idmap_value, V) },
        [ value(X, V) ].

idmap_type:attr_unify_hook(Type, Y) :-
    (   var(Y)
    ->  (   get_attr(Y,idmap_type,Type2)
        ->  Type = Type2
        ;   put_attr(Y,idmap_type,Type)
        )
    ;   Y = _^^Type
    ).

idmap_type:attribute_goals(X) -->
        { get_attr(X, idmap_type, V) },
        [ xsdtype(X, V) ].

:- begin_tests(idmap).

:- use_module(core(triple)).

test(register, []) :-
    woql_ontology(WOQL),
    triple_store(Store),
    safe_open_named_graph(Store,WOQL,Graph),
    head(Graph, Layer),

    subject_value(X, 'http://terminusdb.com/schema/woql#AddData'),
    get_attr(X, idmap_value, X_Val),
    X_Val = "http://terminusdb.com/schema/woql#AddData",
    % don't leave a binding...
    \+ \+ X = 'http://terminusdb.com/schema/woql#AddData',

    predicate_value(Y, 'http://terminusdb.com/schema/sys#inherits'),
    get_attr(Y, idmap_value, Y_Val),
    Y_Val = "http://terminusdb.com/schema/sys#inherits",
    \+ \+ Y = 'http://terminusdb.com/schema/sys#inherits',

    object_value(Z, 'http://terminusdb.com/schema/woql#Query'),
    get_attr(Z, idmap_value, Z_Val),
    Z_Val = node("http://terminusdb.com/schema/woql#Query"),
    \+ \+ Z = 'http://terminusdb.com/schema/woql#Query',

    register_layer_subject_id(Layer, X, X_Id),
    register_layer_predicate_id(Layer, Y, Y_Id),
    register_layer_object_id(Layer, Z, Z_Id),

    id_triple(Layer, X_Id, Y_Id, Z_Id).

test(register_literals, []) :-
    woql_ontology(WOQL),
    triple_store(Store),
    safe_open_named_graph(Store,WOQL,Graph),
    head(Graph, Layer),

    subject_value(X, 'http://terminusdb.com/schema/woql#ArithmeticValue_Documentation'),
    get_attr(X, idmap_value, X_Val),
    X_Val = "http://terminusdb.com/schema/woql#ArithmeticValue_Documentation",
    % don't leave a binding...
    \+ \+ X = 'http://terminusdb.com/schema/woql#ArithmeticValue_Documentation',

    register_layer_subject_id(Layer, X, X_Id),
    register_layer_predicate_id(Layer, Y, Y_Id),

    \+ \+ (   register_layer_object_id(Layer, Z, Z_Id),
              id_triple(Layer, X_Id, Y_Id, Z_Id),
              force_value(X),
              X = 'http://terminusdb.com/schema/woql#ArithmeticValue_Documentation',
              force_value(Y),
              Y = 'http://terminusdb.com/schema/sys#comment',
              force_value(Z),
              Z = "A variable or node."^^'http://www.w3.org/2001/XMLSchema#string'
          ),

    \+ \+ (   Z = _^^_,
              register_layer_object_id(Layer, Z, Z_Id),
              writeq(here),nl,
              id_triple(Layer, X_Id, Y_Id, Z_Id),
              writeq(here),nl,
              force_value(Z),
              write('Z: '),
              writeq(Z),nl,
              Z = "A variable or node."^^'http://www.w3.org/2001/XMLSchema#string'
          ),


    \+ \+ (   Z = _^^'http://www.w3.org/2001/XMLSchema#string',
              register_layer_object_id(Layer, Z, Z_Id),
              id_triple(Layer, X_Id, Y_Id, Z_Id),
              force_value(Z),
              Z = "A variable or node."^^'http://www.w3.org/2001/XMLSchema#string'
          ),

    \+ \+ (   Z = "A variable or node."^^'http://www.w3.org/2001/XMLSchema#string',
              register_layer_object_id(Layer, Z, Z_Id),
              id_triple(Layer, X_Id, Y_Id, Z_Id),
              force_value(Z),
              Z = "A variable or node."^^'http://www.w3.org/2001/XMLSchema#string'
          ),

    true.

test(fail_literals, []) :-
    woql_ontology(WOQL),
    triple_store(Store),
    safe_open_named_graph(Store,WOQL,Graph),
    head(Graph, Layer),

    subject_value(X, 'http://terminusdb.com/schema/woql#ArithmeticValue_Documentation'),
    get_attr(X, idmap_value, X_Val),
    X_Val = "http://terminusdb.com/schema/woql#ArithmeticValue_Documentation",
    % don't leave a binding...
    \+ \+ X = 'http://terminusdb.com/schema/woql#ArithmeticValue_Documentation',

    register_layer_subject_id(Layer, X, X_Id),

    \+ (   Z = _^^_,
           register_layer_object_id(Layer, Z, Z_Id),
           writeq(here),nl,
           id_triple(Layer, X_Id, _, Z_Id),
           writeq(here),nl,
           force_value(Z),
           write('Z: '),
           Z = "X"^^'http://www.w3.org/2001/XMLSchema#string'
       ),

    \+ (   Z = "X"^^'http://www.w3.org/2001/XMLSchema#string',
           register_layer_object_id(Layer, Z, Z_Id),
           writeq(here),nl,
           id_triple(Layer, X_Id, _, Z_Id),
           writeq(here),nl,
           force_value(Z)
       ),

    true.




:- end_tests(idmap).
