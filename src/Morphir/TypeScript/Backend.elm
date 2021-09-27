module Morphir.TypeScript.Backend exposing
    ( Options
    , mapDistribution
    , mapTypeDefinition
    )

{-| This module contains the TypeScript backend that translates the Morphir IR into TypeScript.
-}

import Dict
import Morphir.File.FileMap exposing (FileMap)
import Morphir.IR.AccessControlled exposing (Access(..), AccessControlled)
import Morphir.IR.Distribution as Distribution exposing (Distribution(..))
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName as FQName exposing (FQName)
import Morphir.IR.Module as Module
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package as Package
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.TypeScript.AST as TS exposing (TypeDef)
import Morphir.TypeScript.PrettyPrinter as PrettyPrinter


sampleImportDefs : List TS.ImportDef
sampleImportDefs =
    [ { importRef = "firstImport", sourceFile = "./FirstImport" }
    , { importRef = "secondImport", sourceFile = "./SecondImport" }
    , { importRef = "thirdImport", sourceFile = "./ThirdImport" }
    ]


{-| Placeholder for code generator options. Currently empty.
-}
type alias Options =
    {}


{-| Entry point for the TypeScript backend. It takes the Morphir IR as the input and returns an in-memory
representation of files generated.
-}
mapDistribution : Options -> Distribution -> FileMap
mapDistribution opt distro =
    case distro of
        Distribution.Library packagePath dependencies packageDef ->
            mapPackageDefinition opt distro packagePath packageDef


mapPackageDefinition : Options -> Distribution -> Package.PackageName -> Package.Definition ta (Type ()) -> FileMap
mapPackageDefinition opt distribution packagePath packageDef =
    packageDef.modules
        |> Dict.toList
        |> List.concatMap
            (\( modulePath, moduleImpl ) ->
                mapModuleDefinition opt distribution packagePath modulePath moduleImpl
                    |> List.map
                        (\compilationUnit ->
                            let
                                fileContent =
                                    compilationUnit
                                        |> PrettyPrinter.mapCompilationUnit (PrettyPrinter.Options 2)
                            in
                            ( ( compilationUnit.dirPath, compilationUnit.fileName ), fileContent )
                        )
            )
        |> Dict.fromList


mapModuleDefinition : Options -> Distribution -> Package.PackageName -> Path -> AccessControlled (Module.Definition ta (Type ())) -> List TS.CompilationUnit
mapModuleDefinition opt distribution currentPackagePath currentModulePath accessControlledModuleDef =
    let
        ( typeScriptPackagePath, moduleName ) =
            case currentModulePath |> List.reverse of
                [] ->
                    ( [], [] )

                lastName :: reverseModulePath ->
                    ( List.append (currentPackagePath |> List.map (Name.toCamelCase >> String.toLower)) (reverseModulePath |> List.reverse |> List.map (Name.toCamelCase >> String.toLower)), lastName )

        typeDefsAndImportDefs : ( List TS.TypeDef, List TS.ImportDef )
        typeDefsAndImportDefs =
            accessControlledModuleDef.value.types
                |> Dict.toList
                |> List.map (\( typeName, typeDef ) -> mapTypeDefinition typeName typeDef)
                |> (\resultsList ->
                        ( resultsList |> List.map (\( typeDefList, _ ) -> typeDefList) |> List.concat
                        , resultsList |> List.map (\( _, importDefList ) -> importDefList) |> List.concat
                        )
                   )

        moduleUnit : TS.CompilationUnit
        moduleUnit =
            { dirPath = typeScriptPackagePath
            , fileName = (moduleName |> Name.toTitleCase) ++ ".ts"
            , imports = Tuple.second typeDefsAndImportDefs
            , typeDefs = Tuple.first typeDefsAndImportDefs
            }
    in
    [ moduleUnit ]


{-| Map a Morphir Constructor (A tuple of Name and Constructor Args) to a Typescript AST Interface
-}
mapConstructor : TS.Privacy -> List TS.TypeExp -> ( Name, List ( Name, Type.Type ta ) ) -> ( TS.TypeDef, List TS.ImportDef )
mapConstructor privacy variables ( ctorName, ctorArgs ) =
    let
        nameInTitleCase =
            ctorName |> Name.toTitleCase

        kindField =
            ( "kind", TS.LiteralString nameInTitleCase )

        ( otherFields, importDefList ) =
            ctorArgs
                |> List.map
                    (\( argName, argType ) ->
                        ( argName |> Name.toCamelCase, mapTypeExp argType )
                    )
                |> (\resultsList ->
                        ( resultsList |> List.map (\( argNameinCamelCase, ( mappedArgType, _ ) ) -> ( argNameinCamelCase, mappedArgType ))
                        , resultsList |> List.map (\( _, ( _, importDefSubList ) ) -> importDefSubList) |> List.concat
                        )
                   )

        interface : TS.TypeDef
        interface =
            TS.Interface
                { name = nameInTitleCase
                , privacy = privacy
                , variables = variables
                , fields = kindField :: otherFields
                }
    in
    ( interface, importDefList )


{-| Map a Morphir type definition into a list of TypeScript type definitions. The reason for returning a list is that
some Morphir type definitions can only be represented by a combination of multiple type definitions in TypeScript.
-}
mapTypeDefinition : Name -> AccessControlled (Documented (Type.Definition ta)) -> ( List TS.TypeDef, List TS.ImportDef )
mapTypeDefinition name typeDef =
    let
        doc =
            typeDef.value.doc

        privacy =
            case typeDef.access of
                Public ->
                    TS.Public

                Private ->
                    TS.Private
    in
    case typeDef.value.value of
        Type.TypeAliasDefinition variables typeExp ->
            let
                ( mappedTypeExpression, importDefs ) =
                    mapTypeExp typeExp

                typeDefs =
                    [ TS.TypeAlias
                        { name = name |> Name.toTitleCase
                        , privacy = privacy
                        , doc = doc
                        , variables = variables |> List.map Name.toCamelCase |> List.map (\var -> TS.Variable var)
                        , typeExpression = mappedTypeExpression
                        }
                    ]
            in
            ( typeDefs, importDefs )

        Type.CustomTypeDefinition variables accessControlledConstructors ->
            let
                typeName =
                    name |> Name.toTitleCase

                tsVariables =
                    variables |> List.map Name.toCamelCase |> List.map (\var -> TS.Variable var)

                constructors =
                    accessControlledConstructors.value
                        |> Dict.toList

                constructorNames =
                    accessControlledConstructors.value
                        |> Dict.keys
                        |> List.map Name.toTitleCase

                ( constructorInterfaces, importDefs ) =
                    constructors
                        |> List.map (mapConstructor privacy tsVariables)
                        |> (\resultsList ->
                                ( resultsList
                                    |> List.map (\( interface, _ ) -> interface)
                                , resultsList
                                    |> List.map (\( _, importDefList ) -> importDefList)
                                    |> List.concat
                                )
                           )

                union =
                    if List.all ((==) typeName) constructorNames then
                        []

                    else
                        List.singleton
                            (TS.TypeAlias
                                { name = typeName
                                , privacy = privacy
                                , doc = doc
                                , variables = tsVariables
                                , typeExpression =
                                    TS.Union
                                        (constructors
                                            |> List.map
                                                (\( ctorName, _ ) ->
                                                    TS.TypeRef (ctorName |> Name.toTitleCase) tsVariables
                                                )
                                        )
                                }
                            )

                typeDefs =
                    union ++ constructorInterfaces
            in
            ( typeDefs, importDefs )


{-| Map a Morphir type expression into a TypeScript type expression.
-}
mapTypeExp : Type.Type ta -> ( TS.TypeExp, List TS.ImportDef )
mapTypeExp tpe =
    case tpe of
        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) [] ->
            ( TS.Boolean, sampleImportDefs )

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "dict" ] ], [ "dict" ] ) [ dictKeyType, dictValType ] ->
            let
                ( mappedDictType, dictImportDefs ) =
                    mapTypeExp dictKeyType

                ( mappedValType, valImportDefs ) =
                    mapTypeExp dictValType
            in
            ( TS.List (TS.Tuple [ mappedDictType, mappedValType ])
            , dictImportDefs ++ valImportDefs
            )

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ listType ] ->
            let
                ( mappedTypeExpression, importDefs ) =
                    mapTypeExp listType
            in
            ( TS.List mappedTypeExpression
            , importDefs
            )

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) [] ->
            ( TS.Number
            , sampleImportDefs
            )

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) [] ->
            ( TS.Number
            , sampleImportDefs
            )

        Type.Record _ fieldList ->
            let
                getFieldAndImportDefs : Type.Field a -> ( ( String, TS.TypeExp ), List TS.ImportDef )
                getFieldAndImportDefs field =
                    let
                        ( fieldType, fieldImportDefs ) =
                            mapTypeExp field.tpe
                    in
                    ( ( field.name |> Name.toTitleCase, fieldType ), fieldImportDefs )

                ( mappedFieldList, importDefs ) =
                    fieldList
                        |> List.map getFieldAndImportDefs
                        |> (\resultsList ->
                                ( resultsList |> List.map (\( mappedField, _ ) -> mappedField)
                                , resultsList |> List.map (\( _, fieldImportDefs ) -> fieldImportDefs) |> List.concat
                                )
                           )
            in
            ( TS.Object mappedFieldList
            , importDefs
            )

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) [] ->
            ( TS.String
            , sampleImportDefs
            )

        Type.Tuple _ tupleTypesList ->
            let
                ( mappedTypeExpressions, importDefList ) =
                    tupleTypesList
                        |> List.map mapTypeExp
                        |> (\resultsList ->
                                ( resultsList |> List.map (\( mappedTypeExp, _ ) -> mappedTypeExp)
                                , resultsList |> List.map (\( _, importDefSubList ) -> importDefSubList) |> List.concat
                                )
                           )
            in
            ( TS.Tuple mappedTypeExpressions
            , importDefList
            )

        Type.Reference _ ( packageName, moduleName, localName ) typeList ->
            let
                ( mappedTypeList, importDefList ) =
                    typeList
                        |> List.map mapTypeExp
                        |> (\resultsList ->
                                ( resultsList |> List.map (\( mappedTypeExp, _ ) -> mappedTypeExp)
                                , resultsList |> List.map (\( _, importDefSubList ) -> importDefSubList) |> List.concat
                                )
                           )
            in
            ( TS.TypeRef (localName |> Name.toTitleCase) mappedTypeList
            , { importRef = localName |> Name.toTitleCase
              , sourceFile = "./" ++ Path.toString Name.toTitleCase "/" moduleName
              }
                :: importDefList
            )

        Type.Unit _ ->
            ( TS.Tuple []
            , sampleImportDefs
            )

        Type.Variable _ name ->
            ( TS.Variable (Name.toCamelCase name)
            , sampleImportDefs
            )

        Type.ExtensibleRecord a argName fields ->
            ( TS.UnhandledType "ExtensibleRecord"
            , sampleImportDefs
            )

        Type.Function a argType returnType ->
            ( TS.UnhandledType "Function"
            , sampleImportDefs
            )
