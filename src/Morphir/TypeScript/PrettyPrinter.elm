module Morphir.TypeScript.PrettyPrinter exposing (Options, mapCompilationUnit, mapTypeDef, mapTypeExp)

{-| This module contains a pretty-printer that takes a TypeScript AST as an input and returns a formatted text
representation.

@docs Options, mapCompilationUnit, mapTypeDef, mapTypeExp

-}

import Morphir.File.SourceCode exposing (Doc, concat, empty, indentLines, newLine)
import Morphir.TypeScript.AST exposing (CompilationUnit, TypeDef(..), TypeExp(..))


{-| Formatting options.
-}
type alias Options =
    { indentDepth : Int
    }


{-| -}
mapCompilationUnit : Options -> CompilationUnit -> Doc
mapCompilationUnit opt cu =
    concat
        [ "// Generated by morphir-elm"
        , newLine
        , cu.typeDefs
            |> List.map (mapTypeDef opt)
            |> String.join (newLine ++ newLine)
        , newLine
        ]

{-| Map a type definition to text.
-}
mapTypeDef : Options -> TypeDef -> Doc
mapTypeDef opt typeDef =
    case typeDef of
        TypeAlias name typeExp ->
            concat [ "type ", name, " = ", mapTypeExp opt typeExp ]

        Interface name [] ->
            concat [ "interface ", name, " {}" ]

        Interface name fields ->
            concat
                [ "interface "
                , name
                , " {"
                , newLine
                , fields
                    |> List.map
                        (\( fieldName, fieldType ) ->
                            concat [ fieldName, ": ", mapTypeExp opt fieldType, ";" ]
                        )
                    |> indentLines opt.indentDepth
                , newLine
                , "}"
                ]


{-| Map a type expression to text.
-}
mapTypeExp : Options -> TypeExp -> Doc
mapTypeExp opt typeExp =
    case typeExp of
        LiteralString stringval ->
            "\"" ++ stringval ++ "\""

        String ->
            "string"

        Number ->
            "number"

        Boolean ->
            "boolean"

        List listType ->
            "Array<" ++ mapTypeExp opt listType ++ ">"

        Union types ->
            types
                |> List.map (mapTypeExp opt)
                |> String.join " | "

        TypeRef name ->
            name

        Any ->
            "any"

        UnhandledType tpe ->
            concat [ "any"
                   , " /* Unhandled type: "
                   , tpe
                   , " */"
                   ]
