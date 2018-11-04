module TestEditing exposing (suite)

import Editing
import Expect exposing (Expectation)
import Fuzz exposing (..)
import StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)
import Tasks
import Test exposing (..)


suite : Test
suite =
    describe "Editing"
        [ fuzz2 Tasks.actionFuzzer nonblankStringFuzzer "applies edit" <|
            \action nonblankString ->
                let
                    task =
                        Tasks.taskFromAction action

                    editing =
                        Editing.startEdit task
                            |> Editing.edit nonblankString
                in
                Editing.applyEdit editing task
                    |> Maybe.map
                        (\t ->
                            Tasks.getTaskInfo t
                                |> .action
                                |> Tasks.stringFromAction
                                |> Expect.equal nonblankString
                        )
                    |> Maybe.withDefault (Expect.fail "Expected task to not be Nothing.")
        , fuzz2 Tasks.actionFuzzer whitespaceStringFuzzer "does not apply illegal edit" <|
            \action whitespaceString ->
                let
                    task =
                        Tasks.taskFromAction action

                    editing =
                        Editing.startEdit task
                            |> Editing.edit whitespaceString
                in
                Editing.applyEdit editing task
                    |> Expect.equal Nothing
        ]
