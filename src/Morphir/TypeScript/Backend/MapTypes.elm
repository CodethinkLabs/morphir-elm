module Morphir.TypeScript.Backend.MapTypes exposing (mapPrivacy, mapTypeDefinition)

{-| This module contains the TypeScript backend that translates the Morphir IR Types
into TypeScript.
-}

import Dict
import Morphir.IR.AccessControlled exposing (Access(..), AccessControlled)
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName as FQName
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.TypeScript.AST as TS


type alias TypeVariablesList =
    List Name


type alias ConstructorDetail a =
    { name : Name
    , privacy : TS.Privacy
    , args : List ( Name, Type a )
    , typeVariables : List (Type a)
    , typeVariableNames : List Name
    }


getConstructorDetails : TS.Privacy -> ( Name, List ( Name, Type a ) ) -> ConstructorDetail a
getConstructorDetails privacy ( ctorName, ctorArgs ) =
    let
        typeVariables : List (Type a)
        typeVariables =
            ctorArgs
                |> List.map Tuple.second
                |> List.filter
                    (\argType ->
                        case argType of
                            Type.Variable _ _ ->
                                True

                            _ ->
                                False
                    )
    in
    { name = ctorName
    , privacy = privacy
    , args = ctorArgs
    , typeVariables = typeVariables
    , typeVariableNames =
        typeVariables
            |> List.map
                (\argType ->
                    case argType of
                        Type.Variable _ name ->
                            name

                        _ ->
                            -- Should never happen
                            []
                )
    }


{-| Map a Morphir type definition into a list of TypeScript type definitions. The reason for returning a list is that
some Morphir type definitions can only be represented by a combination of multiple type definitions in TypeScript.
-}
mapTypeDefinition : Name -> AccessControlled (Documented (Type.Definition ta)) -> List TS.TypeDef
mapTypeDefinition name typeDef =
    let
        doc =
            typeDef.value.doc

        privacy =
            typeDef.access |> mapPrivacy
    in
    case typeDef.value.value of
        Type.TypeAliasDefinition variables typeExp ->
            [ TS.TypeAlias
                { name = name |> Name.toTitleCase
                , privacy = privacy
                , doc = doc
                , variables = variables |> List.map Name.toTitleCase |> List.map (\var -> TS.Variable var)
                , typeExpression = typeExp |> mapTypeExp
                , decoder = Just (generateDecoderFunction variables name typeDef.access typeExp)
                , encoder = Just (generateEncoderFunction variables name typeDef.access typeExp)
                }
            ]

        Type.CustomTypeDefinition variables accessControlledConstructors ->
            let
                constructorDetails : List (ConstructorDetail ta)
                constructorDetails =
                    accessControlledConstructors.value
                        |> Dict.toList
                        |> List.map (getConstructorDetails privacy)

                constructorInterfaces =
                    constructorDetails
                        |> List.map mapConstructor

                tsVariables : List TS.TypeExp
                tsVariables =
                    variables |> List.map (Name.toTitleCase >> TS.Variable)

                constructorNames =
                    accessControlledConstructors.value
                        |> Dict.keys

                union =
                    if List.all ((==) name) constructorNames then
                        []

                    else
                        List.singleton
                            (TS.TypeAlias
                                { name = name |> Name.toTitleCase
                                , privacy = privacy
                                , doc = doc
                                , variables = tsVariables
                                , typeExpression =
                                    TS.Union
                                        (constructorDetails
                                            |> List.map
                                                (\constructor ->
                                                    TS.TypeRef
                                                        (FQName.fQName [] [] constructor.name)
                                                        (constructor.typeVariableNames |> List.map (Name.toTitleCase >> TS.Variable))
                                                )
                                        )
                                , decoder = Just (generateUnionDecoderFunction privacy variables name constructorNames)
                                , encoder = Nothing
                                }
                            )
            in
            union ++ constructorInterfaces


mapPrivacy : Access -> TS.Privacy
mapPrivacy privacy =
    case privacy of
        Public ->
            TS.Public

        Private ->
            TS.Private


{-| Map a Morphir Constructor (A tuple of Name and Constructor Args) to a Typescript AST Interface
-}
mapConstructor : ConstructorDetail ta -> TS.TypeDef
mapConstructor constructor =
    let
        kindField : ( String, TS.TypeExp )
        kindField =
            ( "kind", TS.LiteralString (constructor.name |> Name.toTitleCase) )

        otherFields : List ( String, TS.TypeExp )
        otherFields =
            constructor.args
                |> List.map
                    (\( argName, argType ) ->
                        ( argName |> Name.toCamelCase, mapTypeExp argType )
                    )
    in
    TS.Interface
        { name = constructor.name |> Name.toTitleCase
        , privacy = constructor.privacy
        , variables = constructor.typeVariableNames |> List.map (Name.toTitleCase >> TS.Variable)
        , fields = kindField :: otherFields
        , decoder = Just (generateConstructorDecoderFunction constructor)
        , encoder = Nothing
        }


{-| Map a Morphir type expression into a TypeScript type expression.
-}
mapTypeExp : Type.Type ta -> TS.TypeExp
mapTypeExp tpe =
    case tpe of
        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) [] ->
            TS.Boolean

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) [] ->
            TS.Number

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) [] ->
            TS.Number

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "char" ] ], [ "char" ] ) [] ->
            TS.String

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) [] ->
            TS.String

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "dict" ] ], [ "dict" ] ) [ dictKeyType, dictValType ] ->
            TS.List (TS.Tuple [ mapTypeExp dictKeyType, mapTypeExp dictValType ])

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ listType ] ->
            TS.List (mapTypeExp listType)

        Type.Record _ fieldList ->
            TS.Object
                (fieldList
                    |> List.map
                        (\field ->
                            ( field.name |> Name.toCamelCase, mapTypeExp field.tpe )
                        )
                )

        Type.Tuple _ tupleTypesList ->
            TS.Tuple (List.map mapTypeExp tupleTypesList)

        Type.Reference _ fQName typeList ->
            TS.TypeRef fQName (typeList |> List.map mapTypeExp)

        Type.Unit _ ->
            TS.Tuple []

        Type.Variable _ name ->
            TS.Variable (Name.toTitleCase name)

        Type.ExtensibleRecord _ _ _ ->
            TS.UnhandledType "ExtensibleRecord"

        Type.Function _ _ _ ->
            TS.UnhandledType "Function"


genericCodec : String -> TS.Expression
genericCodec function =
    TS.MemberExpression
        { object = TS.Identifier "codecs"
        , member = TS.Identifier function
        }



{--
referenceCodec : FQName -> TS.Expression
referenceCodec ( packageName, moduleName, typeName ) =
    let
        dereferencePathComponent : String -> TS.Expression
        dereferencePathComponent name =
            TS.MemberExpression
                { object = name --}


arrayToMap : TS.Expression -> TS.Expression
arrayToMap array =
    TS.NewExpression
        { constructor = "Map"
        , arguments = [ array ]
        }


decoderExpression : TypeVariablesList -> Type.Type a -> TS.CallExpression
decoderExpression typeVars typeExp =
    case typeExp of
        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) [] ->
            { function = genericCodec "decodeBoolean", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) [] ->
            { function = genericCodec "decodeFloat", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) [] ->
            { function = genericCodec "decodeInt", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "char" ] ], [ "char" ] ) [] ->
            { function = genericCodec "decodeChar", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) [] ->
            { function = genericCodec "decodeString", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "dict" ] ], [ "dict" ] ) [ dictKeyType, dictValType ] ->
            { function = genericCodec "decodeDict"
            , params =
                {--decodeKey --}
                [ TS.Call (bindDecoderExpression typeVars dictKeyType)

                {--decodeValue --}
                , TS.Call (bindDecoderExpression typeVars dictValType)
                ]
            }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ listType ] ->
            { function = genericCodec "decodeList"
            , params = [ TS.Call (bindDecoderExpression typeVars listType) ]
            }

        Type.Record _ fieldList ->
            { function = genericCodec "decodeRecord"
            , params =
                {--fieldDecoders --}
                [ (fieldList
                    |> List.map
                        (\field ->
                            TS.ArrayLiteralExpression
                                [ TS.StringLiteralExpression (Name.toCamelCase field.name)
                                , TS.Call (bindDecoderExpression typeVars field.tpe)
                                ]
                        )
                  )
                    |> TS.ArrayLiteralExpression
                    |> arrayToMap
                ]
            }

        Type.Tuple _ tupleTypesList ->
            { function = genericCodec "decodeTuple"
            , params =
                {--elementDecoders --}
                [ TS.ArrayLiteralExpression
                    (List.map (\item -> TS.Call (bindDecoderExpression typeVars item)) tupleTypesList)
                ]
            }

        Type.Variable _ varName ->
            { function =
                TS.MemberExpression
                    { object = TS.Identifier "varDecoders"
                    , member = TS.Identifier (varName |> Name.toTitleCase)
                    }
            , params = []
            }

        Type.Unit _ ->
            { function = genericCodec "decodeUnit"
            , params = []
            }

        _ ->
            { function = genericCodec "decodeUnit"
            , params = []
            }


bindDecoderExpression : TypeVariablesList -> Type.Type ta -> TS.CallExpression
bindDecoderExpression variables typeExp =
    let
        expression =
            decoderExpression variables typeExp
    in
    { function =
        TS.MemberExpression
            { object = expression.function
            , member = TS.Identifier "bind"
            }
    , params = TS.NullLiteral :: expression.params
    }


generateDecoderFunction : TypeVariablesList -> Name -> Access -> Type.Type ta -> TS.Statement
generateDecoderFunction variables typeName access typeExp =
    let
        call : TS.CallExpression
        call =
            decoderExpression variables typeExp

        addInputParameter : TS.CallExpression -> TS.CallExpression
        addInputParameter oldcall =
            { oldcall | params = call.params ++ [ TS.Identifier "input" ] }
    in
    TS.FunctionDeclaration
        { name = "decode" ++ (typeName |> Name.toTitleCase)
        , parameters = [ "varDecoders", "input" ]
        , privacy = access |> mapPrivacy
        , body = [ TS.ReturnStatement (call |> addInputParameter |> TS.Call) ]
        }


generateConstructorDecoderFunction : ConstructorDetail ta -> TS.Statement
generateConstructorDecoderFunction constructor =
    let
        kindParam =
            TS.StringLiteralExpression (constructor.name |> Name.toTitleCase)

        argNamesParam =
            TS.ArrayLiteralExpression
                (constructor.args
                    |> List.map (Tuple.first >> Name.toCamelCase >> TS.StringLiteralExpression)
                )

        argDecodersParam =
            TS.ArrayLiteralExpression
                (constructor.args
                    |> List.map Tuple.second
                    |> List.map (bindDecoderExpression constructor.typeVariableNames)
                    |> List.map TS.Call
                )

        inputParam =
            TS.Identifier "input"

        call : TS.Expression
        call =
            TS.Call
                { function = genericCodec "decodeCustomTypeVariant"
                , params =
                    [ kindParam
                    , argNamesParam
                    , argDecodersParam
                    , inputParam
                    ]
                }
    in
    TS.FunctionDeclaration
        { name = "decode" ++ (constructor.name |> Name.toTitleCase)
        , privacy = constructor.privacy
        , parameters = [ "varDecoders", "input" ]
        , body = [ TS.ReturnStatement call ]
        }


generateUnionDecoderFunction : TS.Privacy -> TypeVariablesList -> Name -> List Name -> TS.Statement
generateUnionDecoderFunction privacy variables typeName constructorNames =
    let
        letStatement : TS.Statement
        letStatement =
            TS.LetStatement "decoderMap" (TS.NewExpression { constructor = "Map", arguments = [] })

        getMapSetStatement : Name -> TS.Statement
        getMapSetStatement name =
            TS.ExpressionStatement
                (TS.Call
                    { function =
                        TS.MemberExpression
                            { object = TS.Identifier "decoderMap"
                            , member = TS.Identifier "set"
                            }
                    , params =
                        [ TS.StringLiteralExpression (name |> Name.toTitleCase)
                        , TS.Call
                            { function =
                                TS.MemberExpression
                                    { object = TS.Identifier ("decode" ++ (name |> Name.toTitleCase))
                                    , member = TS.Identifier "bind"
                                    }
                            , params = [ TS.Identifier "varDecoders" ]
                            }
                        ]
                    }
                )

        mapSetStatements : List TS.Statement
        mapSetStatements =
            constructorNames |> List.map getMapSetStatement

        finalStatement : TS.Statement
        finalStatement =
            TS.ReturnStatement
                (TS.Call
                    { function =
                        TS.MemberExpression
                            { object = TS.Identifier "codecs"
                            , member = TS.Identifier "decodeCustomType"
                            }
                    , params =
                        [ TS.Identifier "decoderMap"
                        , TS.Identifier "input"
                        ]
                    }
                )
    in
    TS.FunctionDeclaration
        { name = "decode" ++ (typeName |> Name.toTitleCase)
        , privacy = privacy
        , parameters = [ "varDecoders", "input" ]
        , body =
            List.concat
                [ [ letStatement ]
                , mapSetStatements
                , [ finalStatement ]
                ]
        }


encoderExpression : TypeVariablesList -> Type.Type a -> TS.CallExpression
encoderExpression typeVars typeExp =
    case typeExp of
        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) [] ->
            { function = genericCodec "encodeBoolean", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) [] ->
            { function = genericCodec "encodeFloat", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) [] ->
            { function = genericCodec "encodeInt", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "char" ] ], [ "char" ] ) [] ->
            { function = genericCodec "encodeChar", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) [] ->
            { function = genericCodec "encodeString", params = [] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "dict" ] ], [ "dict" ] ) [ dictKeyType, dictValType ] ->
            { function = genericCodec "encodeDict"
            , params =
                {--encodeKey --}
                [ TS.Call (bindEncoderExpression typeVars dictKeyType)

                {--encodeValue --}
                , TS.Call (bindEncoderExpression typeVars dictValType)
                ]
            }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ listType ] ->
            { function = genericCodec "encodeList"
            , params = [ TS.Call (bindEncoderExpression typeVars listType) ]
            }

        Type.Record _ fieldList ->
            { function = genericCodec "encodeRecord"
            , params =
                {--fieldEncoders --}
                [ (fieldList
                    |> List.map
                        (\field ->
                            TS.ArrayLiteralExpression
                                [ TS.StringLiteralExpression (Name.toCamelCase field.name)
                                , TS.Call (bindEncoderExpression typeVars field.tpe)
                                ]
                        )
                  )
                    |> TS.ArrayLiteralExpression
                    |> arrayToMap
                ]
            }

        Type.Tuple _ tupleTypesList ->
            { function = genericCodec "encodeTuple"
            , params =
                {--elementEncoders --}
                [ TS.ArrayLiteralExpression
                    (List.map (\item -> TS.Call (bindEncoderExpression typeVars item)) tupleTypesList)
                ]
            }

        Type.Variable _ varName ->
            { function =
                TS.MemberExpression
                    { object = TS.Identifier "varEncoders"
                    , member = TS.Identifier (varName |> Name.toCamelCase)
                    }
            , params = []
            }

        Type.Unit _ ->
            { function = genericCodec "encodeUnit"
            , params = []
            }

        {--Unhandled types are treated as Unit --}
        _ ->
            { function = genericCodec "encodeUnit"
            , params = []
            }


bindEncoderExpression : TypeVariablesList -> Type.Type ta -> TS.CallExpression
bindEncoderExpression variables typeExp =
    let
        expression =
            encoderExpression variables typeExp
    in
    { function =
        TS.MemberExpression
            { object = expression.function
            , member = TS.Identifier "bind"
            }
    , params = TS.NullLiteral :: expression.params
    }


generateEncoderFunction : TypeVariablesList -> Name -> Access -> Type.Type ta -> TS.Statement
generateEncoderFunction variables typeName access typeExp =
    let
        call =
            encoderExpression variables typeExp

        addValueParameter : TS.CallExpression -> TS.CallExpression
        addValueParameter oldcall =
            { oldcall | params = call.params ++ [ TS.Identifier "value" ] }
    in
    TS.FunctionDeclaration
        { name = "encode" ++ (typeName |> Name.toTitleCase)
        , parameters = [ "varEncoders", "value" ]
        , privacy = access |> mapPrivacy
        , body = [ TS.ReturnStatement (call |> addValueParameter |> TS.Call) ]
        }
