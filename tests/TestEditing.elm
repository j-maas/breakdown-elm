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
        [ describe "Editing Type"
            [ fuzz Tasks.actionFuzzer "started edits are always unchanged" <|
                \action ->
                    let
                        task =
                            Tasks.taskFromAction action

                        editing =
                            Editing.startEdit task
                    in
                    Editing.isUnchanged editing
                        |> Expect.true "Expected editing to be unchanged."
            , fuzz2 Tasks.actionFuzzer nonblankStringFuzzer "edits are changed iff the action is different" <|
                \action nonblankString ->
                    let
                        task =
                            Tasks.taskFromAction action

                        editing =
                            Editing.startEdit task
                                |> Editing.edit nonblankString
                    in
                    Editing.isUnchanged editing
                        |> Expect.equal (Tasks.stringFromAction action == nonblankString)
            , fuzz Tasks.actionFuzzer "edits with the same action are unchanged" <|
                \action ->
                    let
                        task =
                            Tasks.taskFromAction action

                        editing =
                            Editing.startEdit task
                                |> Editing.edit (Tasks.stringFromAction action)
                    in
                    Editing.isUnchanged editing
                        |> Expect.true "Expected editing to be unchanged."
            ]
        , fuzz2 Tasks.actionFuzzer nonblankStringFuzzer "applies edit" <|
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
