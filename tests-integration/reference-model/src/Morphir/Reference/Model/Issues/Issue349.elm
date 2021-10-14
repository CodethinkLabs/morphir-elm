module Morphir.Reference.Model.Issues.Issue349 exposing (..)


type MyCustom dog cat pigeon
    = Foo Int dog
    | Bar Int cat
    | Baz Int pigeon


type MyOtherCustom dog cat pigeon
    = OtherFoo Int dog
    | OtherBar (List cat)
    | OtherBaz { somefield : dog, otherfield : pigeon }


type alias Result a b c =
    { foo : a
    , bar : List b
    , baz : c
    , qux : Int
    }


type alias StringResultIntBool =
    Result String Int Bool


f : Result Bool a Bool
f =
    { foo = True
    , bar = []
    , baz = False
    , qux = 2
    }
