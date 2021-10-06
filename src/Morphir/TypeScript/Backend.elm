module Morphir.TypeScript.Backend exposing
    ( Options
    , mapDistribution
    )

{-| This module contains the TypeScript backend that translates the Morphir IR into TypeScript.
-}

import Dict
import Morphir.File.FileMap exposing (FileMap)
import Morphir.File.SourceCode exposing (newLine)
import Morphir.IR.AccessControlled exposing (Access(..), AccessControlled)
import Morphir.IR.Distribution as Distribution exposing (Distribution(..))
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName as FQName exposing (FQName)
import Morphir.IR.Module as Module
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package as Package
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.TypeScript.AST as TS
import Morphir.TypeScript.Backend.MapTypes exposing (mapPrivacy, mapTypeDefinition)
import Morphir.TypeScript.NamespaceMerger exposing (mergeNamespaces)
import Morphir.TypeScript.PrettyPrinter as PrettyPrinter exposing (getTypeScriptPackagePathAndModuleName)


standardPrettyPrinterOptions : PrettyPrinter.Options
standardPrettyPrinterOptions =
    { indentDepth = 2 }


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


{-| Represents one element of a FileMap,
ie the file path and the contents of file that needs to be created in the backend output.
The structure is ( (directoryPath, Filename), fileContent)
-}
type alias FileMapElement =
    ( ( List String, String ), String )


mapPackageDefinition : Options -> Distribution -> Package.PackageName -> Package.Definition ta (Type ()) -> FileMap
mapPackageDefinition opt distribution packagePath packageDef =
    let
        topLevelNamespaceModule : TS.CompilationUnit
        topLevelNamespaceModule =
            mapTopLevelNamespaceModule packagePath packageDef

        individualModules : List TS.CompilationUnit
        individualModules =
            packageDef.modules
                |> Dict.toList
                |> List.concatMap
                    (\( modulePath, moduleImpl ) ->
                        mapModuleDefinition opt distribution packagePath modulePath moduleImpl
                    )

        compilationUnitToFileMapElement : TS.CompilationUnit -> FileMapElement
        compilationUnitToFileMapElement compilationUnit =
            let
                fileContent =
                    compilationUnit
                        |> PrettyPrinter.mapCompilationUnit standardPrettyPrinterOptions
            in
            ( ( compilationUnit.dirPath, compilationUnit.fileName ), fileContent )
    in
    (topLevelNamespaceModule :: individualModules)
        |> List.map compilationUnitToFileMapElement
        |> Dict.fromList


mapTopLevelNamespaceModule : Package.PackageName -> Package.Definition ta (Type ()) -> TS.CompilationUnit
mapTopLevelNamespaceModule packagePath packageDef =
    let
        topLevelPackageName : String
        topLevelPackageName =
            case packagePath of
                firstName :: _ ->
                    (firstName |> Name.toTitleCase) ++ ".ts"

                _ ->
                    ".ts"

        typeDefs : List TS.TypeDef
        typeDefs =
            mapModuleNamespacesForTopLevelFile packagePath packageDef
    in
    { dirPath = []
    , fileName = topLevelPackageName
    , imports = typeDefs |> List.concatMap (getUniqueImportRefs [] [])
    , typeDefs = typeDefs
    }


mapModuleNamespacesForTopLevelFile : Package.PackageName -> Package.Definition ta (Type ()) -> List TS.TypeDef
mapModuleNamespacesForTopLevelFile packagePath packageDef =
    packageDef.modules
        |> Dict.toList
        |> List.map
            (\( modulePath, moduleImpl ) ->
                ( moduleImpl.access |> mapPrivacy
                , modulePath
                )
            )
        |> List.concatMap
            (\( privacy, modulePath ) ->
                case packagePath ++ modulePath |> List.reverse of
                    [] ->
                        []

                    lastName :: restOfPath ->
                        let
                            importAlias =
                                TS.ImportAlias
                                    { name = lastName
                                    , privacy = privacy
                                    , namespacePath = ( packagePath, modulePath )
                                    }

                            step : Name -> TS.TypeDef -> TS.TypeDef
                            step name state =
                                TS.Namespace
                                    { name = name
                                    , privacy = privacy
                                    , content = List.singleton state
                                    }
                        in
                        [ restOfPath |> List.foldl step importAlias ]
            )
        |> mergeNamespaces


mapModuleDefinition : Options -> Distribution -> Package.PackageName -> Path -> AccessControlled (Module.Definition ta (Type ())) -> List TS.CompilationUnit
mapModuleDefinition opt distribution currentPackagePath currentModulePath accessControlledModuleDef =
    let
        ( typeScriptPackagePath, moduleName ) =
            getTypeScriptPackagePathAndModuleName currentPackagePath currentModulePath

        typeDefs : List TS.TypeDef
        typeDefs =
            accessControlledModuleDef.value.types
                |> Dict.toList
                |> List.concatMap
                    (\( typeName, typeDef ) -> mapTypeDefinition typeName typeDef)

        namespace : TS.TypeDef
        namespace =
            TS.Namespace
                { name =
                    (currentPackagePath ++ currentModulePath)
                        |> Path.toString Name.toTitleCase "_"
                        |> List.singleton
                , privacy = TS.Public
                , content = typeDefs
                }

        {--Collect references from inside the module,
        filter out references to current module
        then sort references and get a list of unique references-}
        moduleUnit : TS.CompilationUnit
        moduleUnit =
            { dirPath = typeScriptPackagePath
            , fileName = (moduleName |> Name.toTitleCase) ++ ".ts"
            , imports = namespace |> getUniqueImportRefs currentPackagePath currentModulePath
            , typeDefs = List.singleton namespace
            }
    in
    [ moduleUnit ]


getUniqueImportRefs : Path -> Path -> TS.TypeDef -> List TS.NamespacePath
getUniqueImportRefs currentPackagePath currentModulePath typeDef =
    typeDef
        |> collectRefsFromTypeDef
        |> List.filter
            (\( packagePath, modulePath ) ->
                packagePath /= currentPackagePath || modulePath /= currentModulePath
            )
        |> List.filter
            (\( packagePath, modulePath ) ->
                packagePath /= [] || modulePath /= []
            )
        |> List.sort
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


collectRefsFromTypeDef : TS.TypeDef -> List TS.NamespacePath
collectRefsFromTypeDef typeDef =
    case typeDef of
        TS.Namespace namespace ->
            namespace.content |> List.concatMap collectRefsFromTypeDef

        TS.TypeAlias typeAlias ->
            List.concat
                [ typeAlias.variables |> List.concatMap collectRefsFromTypeExpression
                , typeAlias.typeExpression |> collectRefsFromTypeExpression
                ]

        TS.Interface interface ->
            List.concat
                [ interface.variables |> List.concatMap collectRefsFromTypeExpression
                , interface.fields |> List.concatMap (\( _, typeExp ) -> collectRefsFromTypeExpression typeExp)
                ]

        TS.ImportAlias importAlias ->
            [ importAlias.namespacePath ]


collectRefsFromTypeExpression : TS.TypeExp -> List TS.NamespacePath
collectRefsFromTypeExpression typeExp =
    case typeExp of
        TS.List subTypeExp ->
            subTypeExp |> collectRefsFromTypeExpression

        TS.Tuple subTypeExpList ->
            subTypeExpList |> List.concatMap collectRefsFromTypeExpression

        TS.Union subTypeExpList ->
            subTypeExpList |> List.concatMap collectRefsFromTypeExpression

        TS.Object fieldList ->
            fieldList |> List.concatMap (\( _, subTypeExp ) -> collectRefsFromTypeExpression subTypeExp)

        TS.TypeRef ( packagePath, modulePath, _ ) subTypeExpList ->
            List.concat
                [ [ ( packagePath, modulePath ) ]
                , subTypeExpList |> List.concatMap collectRefsFromTypeExpression
                ]

        _ ->
            []
