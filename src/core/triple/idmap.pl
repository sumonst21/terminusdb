:- module(idmap,
          [ force_value/1,
            layer_subjectvar_id/3,
            layer_predicatevar_id/3,
            layer_objectvar_id/3,
            layer_id_subjectvar/3,
            layer_id_predicatevar/3,
            layer_id_objectvar/3,
            register_subject/2,
            register_predicate/2,
            register_object/2,
            value/2,
            idmap/2,
            xsdtype/2
          ]).
:- use_module(library(terminus_store)).
:- use_module(literals).

layer_typevar_id(Type,Layer,X,Id) :-
    ground(X),
    !,
    type_id(Type,Layer,X,Id).
layer_typevar_id(Type,Layer,X,Id) :-
    get_attr(X, idmap_map, IdMap),
    type_idspace(Type,Type_Space),
    layer_to_id(Layer, Layer_Id),
    memberchk(Layer_Id-_-Type_Space-Id,IdMap),
    !.
layer_typevar_id(_,_,_,_).

layer_subjectvar_id(Layer,X,Id) :-
    layer_typevar_id(subject,Layer,X,Id).

layer_predicatevar_id(Layer, X, Id) :-
    layer_typevar_id(predicate,Layer,X,Id).

layer_objectvar_id(Layer, X, Id) :-
    layer_typevar_id(object,Layer,X,Id).

layer_id_typevar(Type,Layer,Id,X) :-
    type_idspace(Type,Type_Space),
    layer_to_id(Layer, Layer_Id),
    (   get_attr(X, idmap_map, IdMap)
    ->  (   memberchk(Layer_Id-_-Type_Space-Current,IdMap)
        ->  Current = Id
        ;   idmap(Y, [Layer_Id-Layer-Type_Space-Id]),
            X = Y
        )
    ;   idmap(X, [Layer_Id-Layer-Type_Space-Id])
    ).

layer_id_subjectvar(Layer, Id, X) :-
    layer_id_typevar(subject,Layer,Id,X).

layer_id_predicatevar(Layer, Id, X) :-
    layer_id_typevar(predicate,Layer,Id,X).

layer_id_objectvar(Layer, Id, X) :-
    layer_id_typevar(object,Layer,Id,X).

force_value(X) :-
    nonvar(X),
    !.
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

value(X,Value) :-
    (   var(Value)
    ->  get_attr(X, idmap_value, Pre_Value),
        (   string(Pre_Value)
        ->  Value = Pre_Value
        ;   Pre_Value = node(Value)
        ->  true
        ;   storage_object(Pre_Value,Value)
        )
    ;   (   atom(Value)
        ->  atom_string(Value, String),
            put_attr(Y, idmap_value, String),
            X = Y
        ;   (   Value = _^^_
            ;   Value = _@_)
        ->  object_storage(Value, S),
            put_attr(Y, idmap_value, S),
            X = Y
        )
    ).

type_id(subject, Layer, Subject, Id) :-
    subject_id(Layer, Subject, Id).
type_id(predicate, Layer, Predicate, Id) :-
    predicate_id(Layer, Predicate, Id).
type_id(object, Layer, Object, Id) :-
    object_id(Layer, Object, Id).

type_idspace(subject, so).
type_idspace(predicate, p).
type_idspace(object, so).

register_type(Type,X,Layer) :-
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

register_subject(X,Layer) :-
    register_type(subject,X,Layer).

register_predicate(X,Layer) :-
    register_type(predicate,X,Layer).

register_object(X,Layer) :-
    register_type(object,X,Layer).

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
          idmap_value:attribute_goals/3.


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
    [idmap(X, List)].

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
        [value(X, V)].

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
        [xsdtype(X, V)].

