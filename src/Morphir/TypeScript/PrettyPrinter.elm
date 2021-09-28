module Morphir.TypeScript.PrettyPrinter exposing
    ( Options, mapCompilationUnit, mapTypeDef, mapTypeExp
    , getTypeScriptPackagePathAndModuleName, mapObjectExp
    )

{-| This module contains a pretty-printer that takes a TypeScript AST as an input and returns a formatted text
representation.

@docs Options, mapCompilationUnit, mapTypeDef, mapTypeExp

-}

import Elm.Syntax.ModuleName exposing (ModuleName)
import Html exposing (sub)
import Morphir.File.SourceCode exposing (Doc, concat, empty, indentLines, newLine)
import Morphir.IR.FQName as FQName exposing (FQName, fQName, getModulePath)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Path as Path exposing (Path)
import Morphir.TypeScript.AST exposing (CompilationUnit, ObjectExp, Privacy(..), TypeDef(..), TypeExp(..))


{-| Formatting options.
-}
type alias Options =
    { indentDepth : Int
    }


nameSpaceNameFromPackageAndModule : Path -> Path -> String
nameSpaceNameFromPackageAndModule packagePath modulePath =
    case packagePath ++ modulePath of
        firstName :: restOfNames ->
            List.concat
                [ [ firstName |> Name.toCamelCase ]
                , restOfNames |> List.map Name.toTitleCase
                ]
                |> String.join "_"

        [] ->
            ""


{-| -}
mapCompilationUnit : Options -> CompilationUnit -> Doc
mapCompilationUnit opt cu =
    case cu of
        { dirPath, fileName, packagePath, modulePath, typeDefs } ->
            let
                namespaceName =
                    nameSpaceNameFromPackageAndModule packagePath modulePath
            in
            concat
                [ "// Generated by morphir-elm"
                , newLine ++ newLine
                , typeDefs
                    |> List.concatMap collectRefsFromTypeDef
                    |> renderImports packagePath modulePath
                    |> String.join newLine
                , newLine ++ newLine
                , "export namespace " ++ namespaceName ++ " {" ++ newLine
                , typeDefs
                    |> List.map (mapTypeDef opt)
                    |> List.map (\mappedTypeDef -> mappedTypeDef ++ newLine ++ newLine)
                    |> indentLines opt.indentDepth
                , newLine ++ "}"
                ]


collectRefsFromTypeDef : TypeDef -> List FQName
collectRefsFromTypeDef typeDef =
    case typeDef of
        TypeAlias typeAlias ->
            List.concat
                [ typeAlias.variables |> List.concatMap collectRefsFromTypeExpression
                , typeAlias.typeExpression |> collectRefsFromTypeExpression
                ]

        Interface interface ->
            List.concat
                [ interface.variables |> List.concatMap collectRefsFromTypeExpression
                , interface.fields |> List.concatMap (\( _, typeExp ) -> collectRefsFromTypeExpression typeExp)
                ]


collectRefsFromTypeExpression : TypeExp -> List FQName
collectRefsFromTypeExpression typeExp =
    case typeExp of
        List subTypeExp ->
            subTypeExp |> collectRefsFromTypeExpression

        Tuple subTypeExpList ->
            subTypeExpList |> List.concatMap collectRefsFromTypeExpression

        Union subTypeExpList ->
            subTypeExpList |> List.concatMap collectRefsFromTypeExpression

        Object fieldList ->
            fieldList |> List.concatMap (\( _, subTypeExp ) -> collectRefsFromTypeExpression subTypeExp)

        TypeRef fQName subTypeExpList ->
            List.concat
                [ [ fQName ]
                , subTypeExpList |> List.concatMap collectRefsFromTypeExpression
                ]

        _ ->
            []


renderImports : Path -> Path -> List FQName -> List String
renderImports currentPackagePath currentModulePath importFQNames =
    let
        expandImportName : FQName -> FQName
        expandImportName importName =
            case importName of
                ( [], [], localName ) ->
                    FQName.fQName currentPackagePath currentModulePath localName

                _ ->
                    importName

        uniqueNames : List FQName
        uniqueNames =
            importFQNames
                |> List.map expandImportName
                |> List.filter
                    (\( refPackagePath, refModulePath, _ ) ->
                        (refPackagePath /= currentPackagePath)
                            || (refModulePath /= currentModulePath)
                    )
                |> List.sort
                |> filterUnique

        filePathPrefix : String
        filePathPrefix =
            List.concat [ currentModulePath, currentPackagePath ]
                |> List.map (\_ -> "..")
                |> (\list ->
                        case list of
                            _ :: sublist ->
                                "." :: sublist

                            [] ->
                                list
                   )
                |> String.join "/"

        filePathFromTop : FQName -> String
        filePathFromTop ( packagePath, modulePath, _ ) =
            getTypeScriptPackagePathAndModuleName packagePath modulePath
                |> (\( typeScriptPackagePath, moduleName ) ->
                        concat
                            [ typeScriptPackagePath |> String.join "/"
                            , "/"
                            , moduleName |> Name.toTitleCase
                            ]
                   )
    in
    uniqueNames
        |> List.map
            (\( packagePath, modulePath, localName ) ->
                concat
                    [ "import { "
                    , nameSpaceNameFromPackageAndModule packagePath modulePath
                    , " } from \""
                    , filePathPrefix ++ "/"
                    , ( packagePath, modulePath, localName ) |> filePathFromTop
                    , "\""
                    ]
            )
        |> filterUnique


filterUnique : List a -> List a
filterUnique inputList =
    let
        incrementalFilterUnique : a -> List a -> List a
        incrementalFilterUnique element shorterList =
            if List.member element shorterList then
                shorterList

            else
                element :: shorterList
    in
    List.foldr incrementalFilterUnique [] inputList


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
mapGenericVariables : Options -> List TypeExp -> String
mapGenericVariables opt variables =
    case List.length variables of
        0 ->
            ""

        _ ->
            concat
                [ "<"
                , String.join ", " (variables |> List.map (mapTypeExp opt))
                , ">"
                ]


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


{-| Map an object expression or interface definiton to text
-}
mapObjectExp : Options -> ObjectExp -> Doc
mapObjectExp opt objectExp =
    let
        mapField : ( String, TypeExp ) -> Doc
        mapField ( fieldName, fieldType ) =
            concat [ fieldName, ": ", mapTypeExp opt fieldType, ";" ]
    in
    concat
        [ "{"
        , newLine
        , objectExp
            |> List.map mapField
            |> indentLines opt.indentDepth
        , newLine
        , "}"
        ]


{-| Map a type expression to text.
-}
mapTypeExp : Options -> TypeExp -> Doc
mapTypeExp opt typeExp =
    case typeExp of
        Any ->
            "any"

        Boolean ->
            "boolean"

        List listType ->
            "Array<" ++ mapTypeExp opt listType ++ ">"

        LiteralString stringval ->
            "\"" ++ stringval ++ "\""

        Number ->
            "number"

        Object fieldList ->
            mapObjectExp opt fieldList

        String ->
            "string"

        Tuple tupleTypesList ->
            concat
                [ "["
                , tupleTypesList
                    |> List.map (mapTypeExp opt)
                    |> String.join ", "
                , "]"
                ]

        TypeRef fQName variables ->
            let
                processed_name : String
                processed_name =
                    case fQName of
                        ( [], [], localName ) ->
                            concat
                                [ localName |> Name.toTitleCase
                                ]

                        ( packagePath, modulePath, localName ) ->
                            concat
                                [ nameSpaceNameFromPackageAndModule packagePath modulePath
                                , "."
                                , localName |> Name.toTitleCase
                                ]
            in
            concat
                [ processed_name
                , mapGenericVariables opt variables
                ]

        Union types ->
            types |> List.map (mapTypeExp opt) |> String.join " | "

        Variable name ->
            name

        UnhandledType tpe ->
            concat
                [ "any"
                , " /* Unhandled type: "
                , tpe
                , " */"
                ]
