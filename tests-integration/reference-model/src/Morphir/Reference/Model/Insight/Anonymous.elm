module Morphir.Reference.Model.Insight.Anonymous exposing (..)


type Species
    = Dog
    | Cat
    | Rabbit


type alias LicenseList =
    List
        { year : Int
        , petName : String
        , species : Species
        }
