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


module TestItems exposing (..)

import AntiqueCsvEncoder exposing (antiqueEncoder)
import AntiquesDataSource exposing (antiquesDataSource)
import Csv.Decode as Decode exposing (..)
import Expect exposing (Expectation)
import SparkTests.DataDefinition.Persistence.Income.AntiqueShop exposing (Antique, Product(..))
import SparkTests.Rules.Income.Antique exposing (..)
import Test exposing (..)
import String exposing (..)


testAll : String -> Int -> ( Antique -> Bool) -> Test
testAll  fileName compInteger testType =
    let
        matchingAntiques : Result Error (List Antique)
        matchingAntiques =
            antiquesDataSource
                |> Result.map
                    (\itemsList ->
                        itemsList
                            |> List.filter
                                (\item ->
                                    testType item
                                )
                    )

        csvResults =
            matchingAntiques
                |> Result.map
                    (\result ->
                        result |> antiqueEncoder
                    )

        _ =
            Debug.log ("antiques_expected_results_" ++ fileName ++ ".csv")  csvResults
    in
    test ("Testing" ++ fileName ++ "antique shop rule") (\_ -> Expect.equal (matchingAntiques |> Result.map (\list -> List.length list)) (compInteger |> Ok))

testIsItemVintage : Test
testIsItemVintage = testAll "is_item_vintage" 1200 is_item_vintage

testIsItemWorthMillions : Test
testIsItemWorthMillions = testAll "is_item_worth_millions" 120 is_item_worth_millions

testIsItemWorthThousands : Test
testIsItemWorthThousands = testAll "is_item_worth_thousands" 2160 is_item_worth_thousands


testIsItemAntique : Test
testIsItemAntique = testAll "is_item_antique" 2400 is_item_antique


testSeizeItem : Test
testSeizeItem = testAll "seize_item" 12200 seize_item

