module Morphir.TypeScript.Backend.MapTypes exposing (mapPrivacy, mapTypeDefinition)

{-| This module contains the TypeScript backend that translates the Morphir IR Types
into TypeScript.
-}

import Dict
import Maybe exposing (withDefault)
import Morphir.IR.AccessControlled exposing (Access(..), AccessControlled)
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName as FQName exposing (FQName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.TypeScript.AST as TS
import Set exposing (Set)


type alias TypeVariablesList =
    List Name


type alias ConstructorDetail a =
    { name : Name
    , privacy : TS.Privacy
    , args : List ( Name, Type a )
    , typeVariables : List (Type a)
    , typeVariableNames : List Name
    }


nameToDecodeString : Name -> String
nameToDecodeString name =
    ("decode" :: name) |> Name.toCamelCase


nameToEncodeString : Name -> String
nameToEncodeString name =
    ("encode" :: name) |> Name.toCamelCase


getConstructorDetails : TS.Privacy -> ( Name, List ( Name, Type a ) ) -> ConstructorDetail a
getConstructorDetails privacy ( ctorName, ctorArgs ) =
    let
        typeVariables : List (Type a)
        typeVariables =
            ctorArgs
                |> List.map Tuple.second
                |> List.concatMap collectTypeVariables
                |> deduplicateTypeVariables
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


collectTypeVariables : Type.Type a -> List (Type.Type a)
collectTypeVariables typeExp =
    case typeExp of
        Type.Variable _ _ ->
            [ typeExp ]

        Type.Reference _ _ argTypes ->
            argTypes |> List.concatMap collectTypeVariables

        Type.Tuple _ valueTypes ->
            valueTypes |> List.concatMap collectTypeVariables

        Type.Record _ fieldTypes ->
            fieldTypes |> List.concatMap (\field -> field.tpe |> collectTypeVariables)

        Type.ExtensibleRecord _ _ fieldTypes ->
            fieldTypes |> List.concatMap (\field -> field.tpe |> collectTypeVariables)

        Type.Function _ argumentType returnType ->
            [ argumentType, returnType ] |> List.concatMap collectTypeVariables

        Type.Unit _ ->
            []


type alias TypeList a =
    List (Type.Type a)


deduplicateTypeVariables : TypeList a -> TypeList a
deduplicateTypeVariables list =
    let
        compareAndReturn : Set String -> TypeList a -> TypeList a -> TypeList a
        compareAndReturn seen remaining result =
            case remaining of
                [] ->
                    result

                item :: rest ->
                    case item of
                        Type.Variable _ name ->
                            if Set.member (Name.toTitleCase name) seen then
                                compareAndReturn seen rest result

                            else
                                item
                                    :: compareAndReturn
                                        (Set.insert (Name.toTitleCase name) seen)
                                        remaining
                                        result

                        _ ->
                            []
    in
    compareAndReturn Set.empty list []


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

                unionExpressionFromConstructorDetails : List (ConstructorDetail a) -> TS.TypeExp
                unionExpressionFromConstructorDetails constructors =
                    TS.Union
                        (constructors
                            |> List.map
                                (\constructor ->
                                    TS.TypeRef
                                        (FQName.fQName [] [] constructor.name)
                                        (constructor.typeVariableNames |> List.map (Name.toTitleCase >> TS.Variable))
                                )
                        )

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
                                , typeExpression = unionExpressionFromConstructorDetails constructorDetails
                                , decoder = Just (generateUnionDecoderFunction name privacy variables constructorDetails)
                                , encoder = Just (generateUnionEncoderFunction name privacy variables constructorDetails)
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
        assignKind : TS.Statement
        assignKind =
            TS.AssignmentStatement
                (TS.Identifier "kind")
                (Just (TS.LiteralString (constructor.name |> Name.toTitleCase)))
                (TS.StringLiteralExpression (constructor.name |> Name.toTitleCase))

        typeExpressions : List TS.TypeExp
        typeExpressions =
            constructor.args
                |> List.map (Tuple.second >> mapTypeExp)
    in
    TS.VariantClass
        { name = constructor.name |> Name.toTitleCase
        , privacy = constructor.privacy
        , variables = constructor.typeVariableNames |> List.map (Name.toTitleCase >> TS.Variable)
        , body = [ assignKind ]
        , constructor = Just (generateConstructorConstructorFunction constructor)
        , decoder = Just (generateConstructorDecoderFunction constructor)
        , encoder = Just (generateConstructorEncoderFunction constructor)
        , typeExpressions = typeExpressions
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
            TS.Map (mapTypeExp dictKeyType) (mapTypeExp dictValType)

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


{-- Reference a symbol in the Morphir.Internal.Codecs module. --}
codecsModule : String -> TS.Expression
codecsModule function =
    TS.MemberExpression
        { object = TS.Identifier "codecs"
        , member = TS.Identifier function
        }


referenceCodec : FQName -> String -> TS.Expression
referenceCodec ( packageName, moduleName, _ ) codecName =
    TS.MemberExpression
        { object = TS.Identifier (TS.namespaceNameFromPackageAndModule packageName moduleName)
        , member = TS.Identifier codecName
        }


buildCodecMap : TS.Expression -> TS.Expression
buildCodecMap array =
    TS.Call
        { function = codecsModule "buildCodecMap"
        , params = [ array ]
        }


decoderExpression : TypeVariablesList -> Type.Type a -> TS.CallExpression
decoderExpression customTypeVars typeExp =
    let
        inputParam =
            TS.Identifier "input"
    in
    case typeExp of
        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) [] ->
            { function = codecsModule "decodeBoolean", params = [ inputParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) [] ->
            { function = codecsModule "decodeFloat", params = [ inputParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) [] ->
            { function = codecsModule "decodeInt", params = [ inputParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "char" ] ], [ "char" ] ) [] ->
            { function = codecsModule "decodeChar", params = [ inputParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) [] ->
            { function = codecsModule "decodeString", params = [ inputParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "dict" ] ], [ "dict" ] ) [ dictKeyType, dictValType ] ->
            { function = codecsModule "decodeDict"
            , params =
                {--decodeKey --}
                [ specificDecoderForType customTypeVars dictKeyType

                {--decodeValue --}
                , specificDecoderForType customTypeVars dictValType
                , inputParam
                ]
            }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ listType ] ->
            { function = codecsModule "decodeList"
            , params =
                [ specificDecoderForType customTypeVars listType
                , inputParam
                ]
            }

        Type.Record _ fieldList ->
            { function = codecsModule "decodeRecord"
            , params =
                {--fieldDecoders --}
                [ (fieldList
                    |> List.map
                        (\field ->
                            TS.ArrayLiteralExpression
                                [ TS.StringLiteralExpression (Name.toCamelCase field.name)
                                , specificDecoderForType customTypeVars field.tpe
                                ]
                        )
                  )
                    |> TS.ArrayLiteralExpression
                    |> buildCodecMap
                , inputParam
                ]
            }

        Type.Tuple _ tupleTypesList ->
            { function = codecsModule "decodeTuple"
            , params =
                {--elementDecoders --}
                [ TS.ArrayLiteralExpression
                    (List.map (specificDecoderForType customTypeVars) tupleTypesList)
                , inputParam
                ]
            }

        Type.Variable _ varName ->
            { function =
                TS.Identifier (nameToDecodeString varName)
            , params = [ inputParam ]
            }

        Type.Reference _ fQName argTypes ->
            let
                decoderName =
                    "decode" ++ (FQName.getLocalName fQName |> Name.toTitleCase)

                varDecoders =
                    argTypes |> List.map (specificDecoderForType customTypeVars)
            in
            { function = referenceCodec fQName decoderName
            , params = varDecoders ++ [ inputParam ]
            }

        Type.Unit _ ->
            { function = codecsModule "decodeUnit"
            , params = [ inputParam ]
            }

        {--Unhandled types are treated as Unit --}
        _ ->
            { function = codecsModule "decodeUnit"
            , params = [ inputParam ]
            }


bindArgumentsToFunction : TS.Expression -> List TS.Expression -> TS.Expression
bindArgumentsToFunction function args =
    if List.isEmpty args then
        function
    else
        TS.Call
            { function =
                TS.MemberExpression
                    { object = function
                    , member = TS.Identifier "bind"
                    }
            , params = TS.NullLiteral :: args
            }


specificDecoderForType : TypeVariablesList -> Type.Type ta -> TS.Expression
specificDecoderForType customTypeVars typeExp =
    let
        expression =
            decoderExpression customTypeVars typeExp

        removeInputParam params =
            params |> List.take (List.length params - 1)

        arguments = removeInputParam expression.params
    in
    bindArgumentsToFunction expression.function arguments


generateDecoderFunction : TypeVariablesList -> Name -> Access -> Type.Type ta -> TS.Statement
generateDecoderFunction variables typeName access typeExp =
    let
        call : TS.CallExpression
        call =
            decoderExpression variables typeExp

        variableParameters : List TS.Parameter
        variableParameters =
            variables
                |> List.map
                    (\var ->
                        TS.parameter [] (nameToDecodeString var) Nothing
                    )

        inputParameter : TS.Parameter
        inputParameter =
            TS.parameter [] "input" Nothing
    in
    TS.FunctionDeclaration
        { name = "decode" ++ (typeName |> Name.toTitleCase)
        , scope = TS.ModuleFunction
        , parameters = variableParameters ++ [ inputParameter ]
        , privacy = access |> mapPrivacy
        , body = [ TS.ReturnStatement (TS.Call call) ]
        }


generateConstructorDecoderFunction : ConstructorDetail ta -> TS.Statement
generateConstructorDecoderFunction constructor =
    let
        decoderParams : List TS.Parameter
        decoderParams =
            constructor.typeVariableNames
                |> List.map
                    (\var ->
                        TS.parameter [] (nameToDecodeString var) Nothing
                    )

        inputParameter : TS.Parameter
        inputParameter =
            TS.parameter [] "input" Nothing

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
                    |> List.map (specificDecoderForType constructor.typeVariableNames)
                )

        inputParam =
            TS.Identifier "input"

        call : TS.Expression
        call =
            TS.Call
                { function = codecsModule "decodeCustomTypeVariant"
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
        , scope = TS.ModuleFunction
        , privacy = constructor.privacy
        , parameters = decoderParams ++ [ inputParameter ]
        , body = [ TS.ReturnStatement call ]
        }


generateUnionDecoderFunction : Name -> TS.Privacy -> List Name -> List (ConstructorDetail ta) -> TS.Statement
generateUnionDecoderFunction typeName privacy typeVariables constructors =
    let
        decoderParams : List TS.Parameter
        decoderParams =
            typeVariables
                |> List.map
                    (\var ->
                        TS.parameter [] (nameToDecodeString var) Nothing
                    )

        inputParameter : TS.Parameter
        inputParameter =
            TS.parameter [] "input" Nothing

        getCodecMapEntry : ConstructorDetail ta -> TS.Expression
        getCodecMapEntry constructor =
            TS.ArrayLiteralExpression
                [ TS.StringLiteralExpression (constructor.name |> Name.toTitleCase)
                , bindArgumentsToFunction
                    (constructor.name |> nameToDecodeString |> TS.Identifier)
                    (constructor.typeVariableNames |> List.map (nameToDecodeString >> TS.Identifier))
                ]

        codecMap : TS.Expression
        codecMap =
            constructors |> List.map getCodecMapEntry |>  TS.ArrayLiteralExpression |> buildCodecMap

        call : TS.Expression
        call =
            TS.Call
                { function =
                    TS.MemberExpression
                        { object = TS.Identifier "codecs"
                        , member = TS.Identifier "decodeCustomType"
                        }
                , params =
                    [ codecMap
                    , TS.Identifier "input"
                    ]
                }

    in
    TS.FunctionDeclaration
        { name = "decode" ++ (typeName |> Name.toTitleCase)
        , scope = TS.ModuleFunction
        , privacy = privacy
        , parameters = decoderParams ++ [ inputParameter ]
        , body = [ TS.ReturnStatement call ]
        }


encoderExpression : TypeVariablesList -> Type.Type a -> TS.CallExpression
encoderExpression customTypeVars typeExp =
    let
        valueParam =
            TS.Identifier "value"
    in
    case typeExp of
        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) [] ->
            { function = codecsModule "encodeBoolean", params = [ valueParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) [] ->
            { function = codecsModule "encodeFloat", params = [ valueParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) [] ->
            { function = codecsModule "encodeInt", params = [ valueParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "char" ] ], [ "char" ] ) [] ->
            { function = codecsModule "encodeChar", params = [ valueParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) [] ->
            { function = codecsModule "encodeString", params = [ valueParam ] }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "dict" ] ], [ "dict" ] ) [ dictKeyType, dictValType ] ->
            { function = codecsModule "encodeDict"
            , params =
                {--encodeKey --}
                [ specificEncoderForType customTypeVars dictKeyType

                {--encodeValue --}
                , specificEncoderForType customTypeVars dictValType
                , valueParam
                ]
            }

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ listType ] ->
            { function = codecsModule "encodeList"
            , params =
                [ specificEncoderForType customTypeVars listType
                , valueParam
                ]
            }

        Type.Record _ fieldList ->
            { function = codecsModule "encodeRecord"
            , params =
                {--fieldEncoders --}
                [ (fieldList
                    |> List.map
                        (\field ->
                            TS.ArrayLiteralExpression
                                [ TS.StringLiteralExpression (Name.toCamelCase field.name)
                                , specificEncoderForType customTypeVars field.tpe
                                ]
                        )
                  )
                    |> TS.ArrayLiteralExpression
                    |> buildCodecMap
                , valueParam
                ]
            }

        Type.Tuple _ tupleTypesList ->
            { function = codecsModule "encodeTuple"
            , params =
                {--elementEncoders --}
                [ TS.ArrayLiteralExpression
                    (List.map (specificEncoderForType customTypeVars) tupleTypesList)
                , valueParam
                ]
            }

        Type.Variable _ varName ->
            { function =
                TS.Identifier (nameToEncodeString varName)
            , params = [ valueParam ]
            }

        Type.Reference _ fQName argTypes ->
            let
                decoderName =
                    "encode" ++ (FQName.getLocalName fQName |> Name.toTitleCase)

                varEncoders =
                    argTypes |> List.map (specificEncoderForType customTypeVars)
            in
            { function = referenceCodec fQName decoderName
            , params = varEncoders ++ [ valueParam ]
            }

        Type.Unit _ ->
            { function = codecsModule "encodeUnit"
            , params = [ valueParam ]
            }

        {--Unhandled types are treated as Unit --}
        _ ->
            { function = codecsModule "encodeUnit"
            , params = [ valueParam ]
            }


specificEncoderForType : TypeVariablesList -> Type.Type ta -> TS.Expression
specificEncoderForType customTypeVars typeExp =
    let
        expression =
            encoderExpression customTypeVars typeExp

        removeValueParam params =
            params |> List.take (List.length params - 1)

        arguments = removeValueParam expression.params
    in
    bindArgumentsToFunction expression.function arguments


generateEncoderFunction : TypeVariablesList -> Name -> Access -> Type.Type ta -> TS.Statement
generateEncoderFunction variables typeName access typeExp =
    let
        call =
            encoderExpression variables typeExp

        variableParameters : List TS.Parameter
        variableParameters =
            variables
                |> List.map
                    (\var ->
                        TS.parameter [] (nameToEncodeString var) Nothing
                    )

        valueparameter : TS.Parameter
        valueparameter =
            TS.parameter [] "value" Nothing
    in
    TS.FunctionDeclaration
        { name = "encode" ++ (typeName |> Name.toTitleCase)
        , scope = TS.ModuleFunction
        , parameters = variableParameters ++ [ valueparameter ]
        , privacy = access |> mapPrivacy
        , body = [ TS.ReturnStatement (call |> TS.Call) ]
        }


generateConstructorEncoderFunction : ConstructorDetail ta -> TS.Statement
generateConstructorEncoderFunction constructor =
    let
        encoderParams : List TS.Parameter
        encoderParams =
            constructor.typeVariableNames
                |> List.map
                    (\var ->
                        TS.parameter [] (nameToEncodeString var) Nothing
                    )

        valueParameter : TS.Parameter
        valueParameter =
            TS.parameter [] "value" Nothing

        argNamesParam =
            TS.ArrayLiteralExpression
                (constructor.args
                    |> List.map (Tuple.first >> Name.toCamelCase >> TS.StringLiteralExpression)
                )

        argEncodersParam =
            TS.ArrayLiteralExpression
                (constructor.args
                    |> List.map Tuple.second
                    |> List.map (specificEncoderForType constructor.typeVariableNames)
                )

        valueParam =
            TS.Identifier "value"

        call : TS.Expression
        call =
            TS.Call
                { function = codecsModule "encodeCustomTypeVariant"
                , params =
                    [ argNamesParam
                    , argEncodersParam
                    , valueParam
                    ]
                }
    in
    TS.FunctionDeclaration
        { name = "encode" ++ (constructor.name |> Name.toTitleCase)
        , scope = TS.ModuleFunction
        , privacy = constructor.privacy
        , parameters = encoderParams ++ [ valueParameter ]
        , body = [ TS.ReturnStatement call ]
        }


generateUnionEncoderFunction : Name -> TS.Privacy -> List Name -> List (ConstructorDetail ta) -> TS.Statement
generateUnionEncoderFunction typeName privacy typeVariables constructors =
    let
        encoderParams : List TS.Parameter
        encoderParams =
            typeVariables
                |> List.map
                    (\var ->
                        TS.parameter [] (nameToEncodeString var) Nothing
                    )

        valueParameter : TS.Parameter
        valueParameter =
            TS.parameter [] "value" Nothing

        getCodecMapEntry : ConstructorDetail ta -> TS.Expression
        getCodecMapEntry constructor =
            TS.ArrayLiteralExpression
                [ TS.StringLiteralExpression (constructor.name |> Name.toTitleCase)
                , bindArgumentsToFunction
                    (constructor.name |> nameToEncodeString |> TS.Identifier)
                    (constructor.typeVariableNames |> List.map (nameToEncodeString >> TS.Identifier))
                ]

        codecMap : TS.Expression
        codecMap =
            constructors |> List.map getCodecMapEntry |>  TS.ArrayLiteralExpression |> buildCodecMap

        call : TS.Expression
        call =
            TS.Call
                { function =
                    TS.MemberExpression
                        { object = TS.Identifier "codecs"
                        , member = TS.Identifier "encodeCustomType"
                        }
                , params =
                    [ codecMap
                    , TS.Identifier "value"
                    ]
                }

    in
    TS.FunctionDeclaration
        { name = "encode" ++ (typeName |> Name.toTitleCase)
        , scope = TS.ModuleFunction
        , privacy = privacy
        , parameters = encoderParams ++ [ valueParameter ]
        , body = [ TS.ReturnStatement call ]
        }


generateConstructorConstructorFunction : ConstructorDetail ta -> TS.Statement
generateConstructorConstructorFunction { name, privacy, args, typeVariables, typeVariableNames } =
    let
        argParams : List TS.Parameter
        argParams =
            args
                |> List.map
                    (\( argName, argType ) ->
                        TS.parameter [ "public" ] (argName |> Name.toCamelCase) (Just (mapTypeExp argType))
                    )
    in
    TS.FunctionDeclaration
        { name = "constructor"
        , scope = TS.ClassMemberFunction
        , privacy = privacy
        , parameters = argParams
        , body = []
        }
