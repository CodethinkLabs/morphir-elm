module Morphir.TypeScript.NamespacePath exposing (NamespacePath, namespaceNameFromPackageAndModule)

{-| Helper type to internally track module namespaces.

@docs NamespacePath, namespaceNameFromPackageAndModule
-}

import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Path exposing (Path)


{-| (packagePath, modulePath).

Represents the path to a module. Used in various ways to produce either a path
to a module file, or a reference to that module's namespace. (eg in imports)

This has two components, the package path and the module path.
(Note: this is different from a Morphir Fully Qualified Name, which has three
components: package path, module path AND a local name).

-}
type alias NamespacePath =
    ( Path, Path )


{-| Generate a unique identifier for the given namespace, for private use.
-}
namespaceNameFromPackageAndModule : Path -> Path -> String
namespaceNameFromPackageAndModule packagePath modulePath =
    (packagePath ++ modulePath)
        |> List.map Name.toTitleCase
        |> String.join "_"
