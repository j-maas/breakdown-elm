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
                Breakdown.insert todo Breakdown.empty
                    |> Breakdown.currentTodos
                    |> Expect.equal (Array.fromList [ todo ])
        , test "moves a todo from current to done" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'A' "dd me")

                    inCurrent =
                        Breakdown.insert todo Breakdown.empty
                in
                Breakdown.moveToDone 0 inCurrent
                    |> Maybe.map
                        (\breakdown ->
                            Expect.all
                                [ \b ->
                                    Expect.equal
                                        (Array.fromList [])
                                        (Breakdown.currentTodos b)
                                , \b ->
                                    Expect.equal
                                        (Array.fromList [ todo ])
                                        (Breakdown.doneTodos b)
                                ]
                                breakdown
                        )
                    |> Maybe.withDefault (Expect.fail "Expected move to be successfull.")
        , test "moves a todo from done to current" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'A' "dd me")

                    inCurrent =
                        Breakdown.insert todo Breakdown.empty

                    maybeInDone =
                        Breakdown.moveToDone 0 inCurrent
                in
                case maybeInDone of
                    Just inDone ->
                        Breakdown.moveToCurrent 0 inDone
                            |> Maybe.map
                                (\breakdown ->
                                    Expect.all
                                        [ \b ->
                                            Expect.equal
                                                (Array.fromList [ todo ])
                                                (Breakdown.currentTodos b)
                                        , \b ->
                                            Expect.equal
                                                (Array.fromList [])
                                                (Breakdown.doneTodos b)
                                        ]
                                        breakdown
                                )
                            |> Maybe.withDefault (Expect.fail "Expected move to be successful.")

                    Nothing ->
                        Expect.fail "Expected move to done to be successful."
        ]
