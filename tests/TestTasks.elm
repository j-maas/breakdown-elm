module TestTasks exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra as List
import StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)
import Tasks exposing (..)
import Test exposing (..)


type Collection
    = Current
    | Done


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
        , describe "Editing"
            [ fuzz2 actionFuzzer nonblankStringFuzzer "applies edit" <|
                \action nonblankString ->
                    let
                        task =
                            Tasks.taskFromAction action

                        editing =
                            Tasks.startEdit task
                                |> Tasks.edit nonblankString
                    in
                    Tasks.applyEdit editing task
                        |> Maybe.map
                            (\t ->
                                Tasks.getTaskInfo t
                                    |> .action
                                    |> Tasks.stringFromAction
                                    |> Expect.equal nonblankString
                            )
                        |> Maybe.withDefault (Expect.fail "Expected task to not be Nothing.")
            , fuzz2 actionFuzzer whitespaceStringFuzzer "does not apply illegal edit" <|
                \action whitespaceString ->
                    let
                        task =
                            Tasks.taskFromAction action

                        editing =
                            Tasks.startEdit task
                                |> Tasks.edit whitespaceString
                    in
                    Tasks.applyEdit editing task
                        |> Expect.equal Nothing
            ]
        ]


testWithTask : String -> (Task -> Expectation) -> Expectation
testWithTask rawAction test =
    case actionFromString rawAction of
        Just action ->
            test (Tasks.taskFromAction action)

        Nothing ->
            Expect.fail ("Expected '" ++ rawAction ++ "' to be a valid action.")
