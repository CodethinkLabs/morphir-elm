module Morphir.TypeScript.PrettyPrinter.Options exposing (Options, default)

{-| Formatting options.
-}


type alias Options =
    { indentDepth : Int
    }


default : Options
default =
    { indentDepth = 2 }
