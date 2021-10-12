module Morphir.TypeScript.PrettyPrinter exposing (mapCompilationUnit, mapTypeDef)

{-| This module contains a pretty-printer that takes a TypeScript AST as an input and returns a formatted text
representation.

@docs Options, mapCompilationUnit, mapTypeDef, mapTypeExp

-}

import Morphir.File.SourceCode exposing (Doc, concat, indentLines, newLine)
import Morphir.IR.Path exposing (Path)
import Morphir.TypeScript.AST exposing (CompilationUnit, Expression(..), ImportDeclaration, NamespacePath, Privacy(..), Statement(..), TypeDef(..), TypeExp(..))
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
                , mapMaybeStatement decoder
                , newLine
                , newLine
                , mapMaybeStatement encoder
                ]

        Interface { name, privacy, variables, fields, decoder, encoder } ->
            concat
                [ privacy |> exportIfPublic
                , "interface "
                , name
                , mapGenericVariables opt variables
                , " "
                , mapObjectExp opt fields
                , newLine
                , newLine
                , mapMaybeStatement decoder
                , newLine
                , newLine
                , mapMaybeStatement encoder
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

        Call { function, params } ->
            concat
                [ mapExpression function
                , "("
                , String.join ", " (params |> List.map mapExpression)
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


mapMaybeStatement : Maybe Statement -> String
mapMaybeStatement maybeStatement =
    case maybeStatement of
        Just statement ->
            mapStatement statement

        Nothing ->
            ""


mapStatement : Statement -> String
mapStatement statement =
    case statement of
        FunctionDeclaration { name, parameters, body, privacy } ->
            concat
                [ privacy |> exportIfPublic
                , "function "
                , name
                , "("
                , String.join ", " parameters
                , ") {"
                , newLine
                , String.join newLine (List.map mapStatement body)
                , newLine
                , "}"
                ]

        ReturnStatement expression ->
            concat [ "return ", mapExpression expression, ";" ]

        LetStatement lhsString rhsExpression ->
            concat [ "let ", lhsString, " = ", mapExpression rhsExpression, ";" ]

        ExpressionStatement expression ->
            concat [ mapExpression expression, ";" ]
