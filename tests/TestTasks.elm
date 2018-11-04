module TestTasks exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra as List
import StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)
import Tasks exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Tasks"
        [ fuzz actionFuzzer "always creates valid action" <|
            \action ->
                stringFromAction action
                    |> String.trim
                    |> String.isEmpty
                    |> Expect.false "Action was not empty after trimming."
        , describe "Actions"
            [ fuzz nonblankStringFuzzer "creates valid action" <|
                \validAction ->
                    case actionFromString validAction of
                        Just action ->
                            stringFromAction action
                                |> Expect.equal validAction

                        Nothing ->
                            Expect.fail "Should create action."
            , test "does not create empty actions" <|
                \_ ->
                    actionFromString ""
                        |> Expect.equal Nothing
            , fuzz whitespaceStringFuzzer "does not create actions with only whitespace" <|
                \invalid ->
                    actionFromString invalid
                        |> Expect.equal Nothing
            ]
        ]
