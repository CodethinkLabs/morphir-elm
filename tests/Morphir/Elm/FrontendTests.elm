module Morphir.Elm.FrontendTests exposing (..)

import Dict
import Expect
import Morphir.Elm.Frontend as Frontend exposing (SourceLocation)
import Morphir.IR.AccessControlled exposing (AccessControlled, private, public)
import Morphir.IR.Advanced.Package as Package
import Morphir.IR.Advanced.Type as Type
import Morphir.IR.FQName exposing (fQName)
import Morphir.IR.Path as Path
import Set
import Test exposing (..)


frontendTest : Test
frontendTest =
    let
        sourceA =
            { path = "A.elm"
            , content =
                unindent """
module A exposing (..)

type Foo = Foo Int

type alias Bar = Foo

type alias Rec =
    { field1 : Foo
    , field2 : Bar
    }
                """
            }

        sourceB =
            { path = "B.elm"
            , content =
                unindent """
module B exposing (..)
                """
            }

        expected : Package.Definition ()
        expected =
            { dependencies = Dict.empty
            , modules =
                Dict.fromList
                    [ ( [ [ "a" ] ]
                      , public
                            { types =
                                Dict.fromList
                                    [ ( [ "bar" ]
                                      , public
                                            (Type.typeAliasDefinition []
                                                (Type.reference (fQName [] [] [ "foo" ]) [] ())
                                            )
                                      )
                                    , ( [ "foo" ]
                                      , public
                                            (Type.customTypeDefinition []
                                                (public
                                                    [ ( [ "foo" ]
                                                      , [ ( [ "arg", "1" ], Type.reference (fQName [] [] [ "int" ]) [] () )
                                                        ]
                                                      )
                                                    ]
                                                )
                                            )
                                      )
                                    , ( [ "rec" ]
                                      , public
                                            (Type.typeAliasDefinition []
                                                (Type.record
                                                    [ Type.field [ "field", "1" ]
                                                        (Type.reference (fQName [] [] [ "foo" ]) [] ())
                                                    , Type.field [ "field", "2" ]
                                                        (Type.reference (fQName [] [] [ "bar" ]) [] ())
                                                    ]
                                                    ()
                                                )
                                            )
                                      )
                                    ]
                            , values =
                                Dict.empty
                            }
                      )
                    , ( [ [ "b" ] ]
                      , private
                            { types =
                                Dict.empty
                            , values =
                                Dict.empty
                            }
                      )
                    ]
            }

        packageInfo =
            { name =
                Path.fromString "my/package"
            , exposedModules =
                Set.fromList
                    [ Path.fromString "A"
                    ]
            }
    in
    test "first" <|
        \_ ->
            Frontend.packageDefinitionFromSource packageInfo [ sourceA, sourceB ]
                |> Result.map Package.eraseDefinitionExtra
                |> Expect.equal (Ok expected)


unindent : String -> String
unindent text =
    text