{-
   Copyright 2022 Morgan Stanley

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module GenerateAntiqueTestData exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list



{-
   Takes a list of values as a first argument and takes
   an input list of strings as a second argument.

   The function returns a new list of strings, where each of
   the strings in the value list is appended to each of the
   strings in the input list, to create a new longer list.

   For instance:

   testGen =
      generator [ "one", "two", "three" ]

   testGenOut =
      testGen [ "dog", "cat" ]

   Will output:

   ["dog,one","dog,two","dog,three","cat,one","cat,two","cat,three"]
-}


generator : List String -> List String -> List String
generator values input =
    input
        |> List.map
            (\item ->
                values
                    |> List.map
                        (\value ->
                            item ++ "," ++ value
                        )
            )
        |> flatten


testDataGeneration : Test
testDataGeneration =
    let
        columnNames =
            "category,product,priceValue,ageOfItem,handMade,requiresExpert,expertFeedBack,report"

        generateCategory =
            generator [ "PaintCollections", "HouseHoldCollection", "SimpleToolCollection", "Diary", "" ]

        generateProduct =
            generator [ "Paintings", "Knife", "Plates", "Furniture", "HistoryWritings" ]

        generatePriceValue =
            generator [ "0.0", "1.0", "100.0", "1000.0", "1000000.0" ]

        generateAgeOfItem =
            generator [ "-1", "0", "19", "20", "21", "99", "100", "101" ]

        generateHandMade =
            generator [ "True", "False" ]

        generateRequiresExpert =
            generator [ "True", "False" ]

        generateExpertFeedBack =
            generator [ "Genuine", "Fake", "" ]

        dataList =
            generateCategory [ "" ]
                |> generateProduct
                |> generatePriceValue
                |> generateAgeOfItem
                |> generateHandMade
                |> generateRequiresExpert
                |> generateExpertFeedBack
                |> List.indexedMap
                    (\index item ->
                        let
                            itemWithReport =
                                item ++ ",Report #" ++ String.fromInt (index + 1)

                            itemWithNoReport =
                                item ++ ","
                        in
                        [ itemWithReport, itemWithNoReport ]
                    )
                |> flatten

        csvData =
            columnNames :: dataList

        _ =
            Debug.log "antiques_data.csv" csvData
    in
    test "Testing generation of test data" (\_ -> Expect.equal (List.length csvData) 24001)
