module TestChecklist exposing (suite)

import Checklist exposing (Checklist)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Checklist"
        [ fuzz2 checklistFuzzer int "inserts and retrieves item by id" <|
            \checklist item ->
                Checklist.insert item checklist
                    |> (\( id, cl ) ->
                            Checklist.get id cl
                       )
                    |> Expect.equal (Just item)
        ]


checklistFuzzer : Fuzzer (Checklist Int)
checklistFuzzer =
    Fuzz.map
        Checklist.fromList
        (list int)
