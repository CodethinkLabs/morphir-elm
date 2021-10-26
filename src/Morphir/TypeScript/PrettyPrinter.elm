module Morphir.TypeScript.PrettyPrinter exposing (mapCompilationUnit, mapTypeDef)

{-| This module contains a pretty-printer that takes a TypeScript AST as an input and returns a formatted text
representation.

@docs Options, mapCompilationUnit, mapTypeDef, mapTypeExp

-}

import Morphir.File.SourceCode exposing (Doc, concat, indentLines, newLine)
import Morphir.IR.Path exposing (Path)
import Morphir.TypeScript.AST exposing (CompilationUnit, Expression(..), FunctionScope(..), ImportDeclaration, NamespacePath, Parameter, Privacy(..), Statement(..), TypeDef(..), TypeExp(..))
import Morphir.TypeScript.PrettyPrinter.MapExpressions exposing (..)


{-| -}
mapCompilationUnit : Options -> CompilationUnit -> Doc
mapCompilationUnit opt cu =
    case cu of
        { dirPath, imports, typeDefs } ->
            concat
                [ "// Generated by morphir-elm"
                , newLine ++ newLine
                , imports
                    |> List.map mapImportDeclaration
                    |> String.join newLine
                , newLine ++ newLine
                , typeDefs
                    |> List.map (mapTypeDef opt)
                    |> List.map (\mappedTypeDef -> mappedTypeDef ++ newLine ++ newLine)
                    |> String.join newLine
                ]


mapImportDeclaration : ImportDeclaration -> String
mapImportDeclaration { importClause, moduleSpecifier } =
    concat
        [ "import "
        , importClause
        , " from "
        , "\"" ++ moduleSpecifier ++ "\""
        ]


exportIfPublic : Privacy -> String
exportIfPublic privacy =
    case privacy of
        Public ->
            "export "

        Private ->
            ""


{-| Map a type definition to text.
-}
mapTypeDef : Options -> TypeDef -> Doc
mapTypeDef opt typeDef =
    case typeDef of
        Namespace { name, privacy, content } ->
            concat
                [ privacy |> exportIfPublic
                , "namespace "
                , name
                , " {" ++ newLine
                , content
                    |> List.map (mapTypeDef opt)
                    |> List.map (\mappedTypeDef -> mappedTypeDef ++ newLine)
                    |> indentLines opt.indentDepth
                , newLine ++ "}"
                ]

        TypeAlias { name, privacy, doc, variables, typeExpression, decoder, encoder } ->
            let
                docstring =
                    if String.length doc > 0 then
                        String.concat [ "/*", doc, "*/" ]

                    else
                        ""
            in
            concat
                [ docstring
                , newLine
                , privacy |> exportIfPublic
                , "type "
                , name
                , mapGenericVariables opt variables
                , " = "
                , mapTypeExp opt typeExpression
                , newLine
                , newLine
                , mapMaybeStatement opt decoder
                , newLine
                , newLine
                , mapMaybeStatement opt encoder
                ]

        VariantClass { name, privacy, variables, body, constructor, decoder, encoder } ->
            let
                preface : String
                preface =
                    concat
                        [ privacy |> exportIfPublic
                        , "class "
                        , name
                        , mapGenericVariables opt variables
                        , " {"
                        ]

                mainbody : List String
                mainbody =
                    [ body |> List.map (mapStatement opt) >> String.join newLine
                    , newLine
                    , mapMaybeStatement opt constructor
                    ]
            in
            concat
                [ preface
                , newLine
                , mainbody |> indentLines opt.indentDepth
                , "}"
                , newLine
                , newLine
                , mapMaybeStatement opt decoder
                , newLine
                , newLine
                , mapMaybeStatement opt encoder
                , newLine
                ]

        ImportAlias { name, privacy, namespacePath } ->
            concat
                [ privacy |> exportIfPublic
                , "import "
                , name
                , " = "
                , namespaceNameFromPackageAndModule (Tuple.first namespacePath) (Tuple.second namespacePath)
                ]


mapExpression : Expression -> String
mapExpression expression =
    case expression of
        ArrayLiteralExpression values ->
            concat
                [ "["
                , String.join ", " (values |> List.map mapExpression)
                , "]"
                ]

        Call { function, arguments } ->
            concat
                [ mapExpression function
                , "("
                , String.join ", " (arguments |> List.map mapExpression)
                , ")"
                ]

        Identifier name ->
            name

        MemberExpression { object, member } ->
            concat
                [ mapExpression object
                , "."
                , mapExpression member
                ]

        NewExpression { constructor, arguments } ->
            concat
                [ "new "
                , constructor
                , "("
                , arguments |> List.map mapExpression |> String.join ", "
                , ")"
                ]

        NullLiteral ->
            "null"

        ObjectLiteralExpression { properties } ->
            let
                mapObjectField : ( String, Expression ) -> String
                mapObjectField ( fieldName, fieldValue ) =
                    concat
                        [ fieldName
                        , ": "
                        , fieldValue |> mapExpression
                        ]
            in
            concat
                [ "{"
                , String.join ", " (properties |> List.map mapObjectField)
                , "}"
                ]

        StringLiteralExpression string ->
            concat
                [ "\""
                , string
                , "\""
                ]


mapMaybeStatement : Options -> Maybe Statement -> String
mapMaybeStatement opt maybeStatement =
    case maybeStatement of
        Just statement ->
            mapStatement opt statement

        Nothing ->
            ""


mapStatement : Options -> Statement -> String
mapStatement opt statement =
    case statement of
        FunctionDeclaration { name, scope, parameters, body, privacy } ->
            let
                prefaceKeywords : String
                prefaceKeywords =
                    case scope of
                        ModuleFunction ->
                            concat
                                [ privacy |> exportIfPublic
                                , "function "
                                ]

                        ClassStaticFunction ->
                            concat
                                [ "static "
                                ]

                        _ ->
                            ""
            in
            concat
                [ prefaceKeywords
                , name
                , "("
                , String.join ", " (parameters |> List.map (mapParameter opt))
                , ") {"
                , newLine
                , body |> List.map (mapStatement opt) |> indentLines opt.indentDepth
                , newLine
                , "}"
                ]

        ReturnStatement expression ->
            concat [ "return ", mapExpression expression, ";" ]

        LetStatement lhsExpression maybeAnnotation rhsExpression ->
            concat
                [ "let "
                , mapExpression lhsExpression
                , mapMaybeAnnotation opt maybeAnnotation
                , " = "
                , mapExpression rhsExpression
                , ";"
                ]

        AssignmentStatement lhsExpression maybeAnnotation rhsExpression ->
            concat
                [ mapExpression lhsExpression
                , mapMaybeAnnotation opt maybeAnnotation
                , " = "
                , mapExpression rhsExpression
                , ";"
                ]

        ExpressionStatement expression ->
            concat [ mapExpression expression, ";" ]


mapParameter : Options -> Parameter -> String
mapParameter opt { modifiers, name, typeAnnotation } =
    concat
        [ modifiers |> String.join " "
        , " "
        , name
        , mapMaybeAnnotation opt typeAnnotation
        ]


mapMaybeAnnotation : Options -> Maybe TypeExp -> String
mapMaybeAnnotation opt maybeTypeExp =
    case maybeTypeExp of
        Nothing ->
            ""

        Just typeExp ->
            ": " ++ mapTypeExp opt typeExp
