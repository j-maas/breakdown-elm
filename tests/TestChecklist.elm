module TestChecklist exposing (suite)

import Checklist exposing (Checklist)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float, int, list, percentage, string)
import List.Extra as List
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
        , fuzz3 (nonemptyListFuzzer int) percentage int "updates item by id" <|
            \items percentage newItem ->
                Checklist.fromList items
                    |> (\checklist ->
                            getAtIndex percentage
                                (\index id item ->
                                    let
                                        newItems =
                                            List.setAt index newItem items

                                        newChecklist =
                                            Checklist.fromList newItems
                                    in
                                    Checklist.update (\_ -> newItem) id checklist
                                        |> Expect.equal (Just newChecklist)
                                )
                                checklist
                       )
        , fuzz2 (nonemptyListFuzzer int) percentage "removes item by id" <|
            \items percentage ->
                Checklist.fromList items
                    |> (\checklist ->
                            getAtIndex percentage
                                (\index id item ->
                                    let
                                        newItems =
                                            List.removeAt index items

                                        newChecklist =
                                            Checklist.fromList newItems
                                    in
                                    Checklist.remove id checklist
                                        |> Expect.equal (Just newChecklist)
                                )
                                checklist
                       )
        ]


checklistFuzzer : Fuzzer (Checklist Int)
checklistFuzzer =
    Fuzz.map
        Checklist.fromList
        (list int)


nonemptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonemptyListFuzzer itemFuzzer =
    Fuzz.map2 (\item list -> [ item ] ++ list)
        itemFuzzer
        (list itemFuzzer)


getAtIndex : Float -> (Int -> Checklist.Id -> a -> Expectation) -> Checklist a -> Expectation
getAtIndex percentage aTest checklist =
    let
        list =
            Checklist.map Tuple.pair checklist
    in
    case List.length list of
        0 ->
            Expect.fail "The provided list was empty. Make sure that the list is not empty."

        n ->
            let
                chosenIndex =
                    (toFloat (n - 1) * percentage)
                        |> Basics.floor
            in
            List.getAt chosenIndex list
                |> Maybe.map (\( id, item ) -> aTest chosenIndex id item)
                |> Maybe.withDefault (Expect.fail "Expected index to be valid, but received Nothing.")
