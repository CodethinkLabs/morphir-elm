module Morphir.TypeScript.NamespaceMerger exposing (mergeNamespaces)

{-| The mergeNamespaces function takes a list of TypeDefs, and returns an
equivalent list where Namespaces that have the same name will be merged together.
-}

import Morphir.IR.Name exposing (Name)
import Morphir.TypeScript.AST exposing (Privacy(..), TypeDef(..))


mergeNamespaces : List TypeDef -> List TypeDef
mergeNamespaces inputList =
    let
        {--Considers a TypeDef (needle) and a list of TypeDefs (haystack). Returns true
        if the needle is a Namespace, and the haystack contains another Namespace with
        the same name as the needle. Returns False otherwise --}
        hasMatch : TypeDef -> List TypeDef -> Bool
        hasMatch needle haystack =
            haystack
                |> List.any
                    (\candidate ->
                        case ( needle, candidate ) of
                            ( Namespace ns1, Namespace ns2 ) ->
                                ns1.name == ns2.name

                            _ ->
                                False
                    )

        {--Given two typedefs that are both NameSpaces, will merge them together if
        they have the same name. If they have different names or are not both namespaces,
        then the function simply outputs the second TypeDef
        --}
        conditionallyMergeTwoNamespaces : TypeDef -> TypeDef -> TypeDef
        conditionallyMergeTwoNamespaces td1 td2 =
            case ( td1, td2 ) of
                ( Namespace ns1, Namespace ns2 ) ->
                    if ns1.name == ns2.name then
                        Namespace
                            { name = ns2.name
                            , privacy =
                                if (ns1.privacy == Public) || (ns2.privacy == Public) then
                                    Public

                                else
                                    Private
                            , content = (ns2.content ++ ns1.content) |> mergeNamespaces
                            }

                    else
                        td2

                _ ->
                    td2

        {--Inserts a new typeDef into a list of TypeDefs
        If the new typeDef is a namespace, with the same name as an existing namespace
        that is in the list, then the two namespaces will be merged. Otherwise the new
        TypeDef is just appended to the list, --}
        insertNamespaceIntoList : TypeDef -> List TypeDef -> List TypeDef
        insertNamespaceIntoList typeDef targetList =
            case ( typeDef, hasMatch typeDef targetList ) of
                ( Namespace _, True ) ->
                    targetList |> List.map (conditionallyMergeTwoNamespaces typeDef)

                _ ->
                    targetList ++ [ typeDef ]
    in
    inputList |> List.foldl insertNamespaceIntoList []
