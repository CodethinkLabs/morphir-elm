module Morphir.TypeScript.PrettyPrinter exposing
    ( mapCompilationUnit, mapTypeDef
    , getTypeScriptPackagePathAndModuleName
    )

{-| This module contains a pretty-printer that takes a TypeScript AST as an input and returns a formatted text
representation.

@docs Options, mapCompilationUnit, mapTypeDef, mapTypeExp

-}

import Morphir.File.SourceCode exposing (Doc, concat, indentLines, newLine)
import Morphir.IR.FQName as FQName
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Path exposing (Path)
import Morphir.TypeScript.AST exposing (CompilationUnit, NamespacePath, Privacy(..), TypeDef(..), TypeExp(..))
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
                    |> renderImports dirPath
                    |> String.join newLine
                , newLine ++ newLine
                , typeDefs
                    |> List.map (mapTypeDef opt)
                    |> List.map (\mappedTypeDef -> mappedTypeDef ++ newLine ++ newLine)
                    |> String.join newLine
                ]


createDecoderBoolean : Name -> String
createDecoderBoolean name =
    "const decode" ++ (Name.toTitleCase name) ++ " = decodeBoolean;"


createDecoderUnion : Name -> List TypeExp -> String
createDecoderUnion name fields =
    let
        nameString = (Name.toTitleCase name)
        decodeField = (\field ->
            case field of
                TypeRef fQName contents ->
                    [ "    if (input == '" ++ (fQName |> FQName.getLocalName |> Name.toSnakeCase) ++ "') {"
                    , "        return " ++ (fQName |> FQName.getLocalName |> Name.toTitleCase) ++ ";"
                    , "    }"
                    ]
                _ ->
                    [ "// Error!!! expected typeref" ]
            )
        lines =
            [ "function decode" ++ nameString ++ "(input: any): " ++ nameString ++ " {" ] ++
            (fields |> List.concatMap decodeField) ++
            [ "}" ]
    in
        String.join newLine lines


createDecoder : TypeDef -> String
createDecoder typeDef =
    case typeDef of
        TypeAlias { name, variables, typeExpression } ->
            case typeExpression of
                Boolean ->
                    createDecoderBoolean name
                Union fields ->
                    createDecoderUnion name fields
                _ ->
                    "// No decoder for " ++ (Name.toTitleCase name)
        Interface { name, variables, fields } ->
            "// No decoder for " ++ (Name.toTitleCase name)
        Namespace { } -> ""
        ImportAlias { } -> ""


renderImports : List String -> List NamespacePath -> List String
renderImports dirPath importNamespacePaths =
    let
        filePathPrefix : String
        filePathPrefix =
            dirPath
                |> List.map (\_ -> "..")
                |> (\list -> "." :: list)
                |> String.join "/"

        filePathFromTop : ( Path, Path ) -> String
        filePathFromTop ( packagePath, modulePath ) =
            getTypeScriptPackagePathAndModuleName packagePath modulePath
                |> (\( typeScriptPackagePath, moduleName ) ->
                        concat
                            [ typeScriptPackagePath |> String.join "/"
                            , "/"
                            , moduleName |> Name.toTitleCase
                            ]
                   )
    in
    importNamespacePaths
        |> List.map
            (\( packagePath, modulePath ) ->
                concat
                    [ "import { "
                    , namespaceNameFromPackageAndModule packagePath modulePath
                    , " } from \""
                    , filePathPrefix ++ "/"
                    , ( packagePath, modulePath ) |> filePathFromTop
                    , "\""
                    ]
            )


{-| Extracts a directory path (as a sequence of folder name string) and a Module filename (as a
Name object), given a Morphir Package Path and a Morphir Module Path.
-}
getTypeScriptPackagePathAndModuleName : Path -> Path -> ( List String, Name )
getTypeScriptPackagePathAndModuleName packagePath modulePath =
    case modulePath |> List.reverse of
        [] ->
            ( [], [] )

        lastName :: reverseModulePath ->
            ( List.append
                (packagePath |> List.map (Name.toCamelCase >> String.toLower))
                (reverseModulePath |> List.reverse |> List.map (Name.toCamelCase >> String.toLower))
            , lastName
            )


{-| Map a type definition to text.
-}
mapTypeDef : Options -> TypeDef -> Doc
mapTypeDef opt typeDef =
    let
        exportIfPublic : Privacy -> String
        exportIfPublic privacy =
            case privacy of
                Public ->
                    "export "

                Private ->
                    ""
    in
    case typeDef of
        Namespace { name, privacy, content } ->
            concat
                [ privacy |> exportIfPublic
                , "namespace "
                , name |> Name.toTitleCase
                , " {" ++ newLine
                , content
                    |> List.map (mapTypeDef opt)
                    |> List.map (\mappedTypeDef -> mappedTypeDef ++ newLine)
                    |> indentLines opt.indentDepth
                , newLine
                , content
                    |> List.map (createDecoder)
                    |> List.map (\decoder -> decoder ++ newLine)
                    |> indentLines opt.indentDepth
                , newLine ++ "}"
                ]

        TypeAlias { name, privacy, doc, variables, typeExpression } ->
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
                , name |> Name.toTitleCase
                , mapGenericVariables opt variables
                , " = "
                , mapTypeExp opt typeExpression
                ]

        Interface { name, privacy, variables, fields } ->
            concat
                [ privacy |> exportIfPublic
                , "interface "
                , name |> Name.toTitleCase
                , mapGenericVariables opt variables
                , " "
                , mapObjectExp opt fields
                ]

        ImportAlias { name, privacy, namespacePath } ->
            concat
                [ privacy |> exportIfPublic
                , "import "
                , name |> Name.toTitleCase
                , " = "
                , namespaceNameFromPackageAndModule (Tuple.first namespacePath) (Tuple.second namespacePath)
                ]
