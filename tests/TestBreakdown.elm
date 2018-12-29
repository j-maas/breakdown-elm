module TestBreakdown exposing (suite)

import Array
import Breakdown
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Todo
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "Breakdown"
        [ test "adds new todo to current" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'A' "dd me")
                in
                Breakdown.put todo Breakdown.empty
                    |> Breakdown.currentTodos
                    |> List.map Tuple.second
                    |> Expect.equal [ todo ]
        , test "moves a todo from current to done" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'A' "dd me")

                    ( id, inCurrent ) =
                        Breakdown.insert todo Breakdown.empty
                in
                Breakdown.moveToDone id inCurrent
                    |> Maybe.map
                        (\( _, breakdown ) ->
                            Expect.all
                                [ \b ->
                                    Breakdown.currentTodos b
                                        |> List.map Tuple.second
                                        |> Expect.equal []
                                , \b ->
                                    Breakdown.doneTodos b
                                        |> List.map Tuple.second
                                        |> Expect.equal [ todo ]
                                ]
                                breakdown
                        )
                    |> Maybe.withDefault (Expect.fail "Expected move to be successfull.")
        , test "moves a todo from done to current" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'A' "dd me")

                    ( id, inCurrent ) =
                        Breakdown.insert todo Breakdown.empty

                    maybeMoved =
                        Breakdown.moveToDone id inCurrent
                in
                case maybeMoved of
                    Just ( newId, inDone ) ->
                        Breakdown.moveToCurrent newId inDone
                            |> Maybe.map
                                (\( _, breakdown ) ->
                                    Expect.all
                                        [ \b ->
                                            Breakdown.currentTodos b
                                                |> List.map Tuple.second
                                                |> Expect.equal [ todo ]
                                        , \b ->
                                            Breakdown.doneTodos b
                                                |> List.map Tuple.second
                                                |> Expect.equal []
                                        ]
                                        breakdown
                                )
                            |> Maybe.withDefault (Expect.fail "Expected move to be successful.")

                    Nothing ->
                        Expect.fail "Expected move to done to be successful."
        ]
