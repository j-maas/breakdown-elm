module TestMain exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Main exposing (GlobalTaskId(..), Model, Msg(..), initModel, update)
import Tasks
import Test exposing (..)


suite : Test
suite =
    describe "messages"
        [ test "NoOp keeps model the same" <|
            \_ ->
                update NoOp initModel
                    |> expectModelEquals initModel
        , test "UpdateNewTask saves the new task" <|
            \_ ->
                update (UpdateNewTask "i am an edit") initModel
                    |> expectModelEquals { initModel | newTask = "i am an edit" }
        , describe "AddNewTask"
            [ test "saves valid task and resets new task" <|
                \_ ->
                    let
                        rawAction =
                            "I am a valid action."
                    in
                    testWithAction rawAction
                        (\action ->
                            update AddNewTask { initModel | newTask = rawAction }
                                |> expectModelEquals
                                    { initModel
                                        | newTask = ""
                                        , currentTasks = Tasks.appendTask action initModel.currentTasks
                                    }
                        )
            , test "does not save valid task, but resets new task" <|
                \_ ->
                    update AddNewTask { initModel | newTask = "   " }
                        |> expectModelEquals { initModel | newTask = "" }
            ]
        , test "DoTask moves task from current collection to done" <|
            \_ ->
                testWithAction "Do me"
                    (\action ->
                        let
                            ( task, currentTasks ) =
                                Tasks.appendAndGetTask action initModel.currentTasks

                            init =
                                { initModel | currentTasks = currentTasks }
                        in
                        update (DoTask <| Tasks.getId task) init
                            |> Expect.all
                                [ \( model, _ ) -> Expect.equal initModel.currentTasks model.currentTasks
                                , \( model, _ ) -> Expect.equal (Tasks.appendTask action initModel.doneTasks) model.doneTasks
                                ]
                    )
        , test "UndoTask moves task from done collection to current" <|
            \_ ->
                testWithAction "Undo me"
                    (\action ->
                        let
                            ( task, doneTasks ) =
                                Tasks.appendAndGetTask action initModel.doneTasks

                            init =
                                { initModel | doneTasks = doneTasks }
                        in
                        update (UndoTask <| Tasks.getId task) init
                            |> Tuple.first
                            |> Expect.all
                                [ \model -> expectEquivalentCollections initModel.doneTasks model.doneTasks
                                , \model -> expectEquivalentCollections (Tasks.appendTask action initModel.currentTasks) model.currentTasks
                                ]
                    )
        , test "DeleteTask removes a task" <|
            \_ ->
                testWithAction "Delete me"
                    (\action ->
                        let
                            ( task, currentTasks ) =
                                Tasks.appendAndGetTask action initModel.currentTasks

                            init =
                                { initModel | currentTasks = currentTasks }
                        in
                        update (DeleteTask <| CurrentId <| Tasks.getId task) init
                            |> expectModelEquals initModel
                    )
        , test "ApplyEdit applies the edit in progress" <|
            \_ ->
                testWithAction "Apply the edit to me"
                    (\action ->
                        let
                            ( task, currentTasks ) =
                                Tasks.appendAndGetTask action initModel.currentTasks

                            globalId =
                                CurrentId <| Tasks.getId task

                            init =
                                { initModel
                                    | currentTasks = currentTasks
                                }
                        in
                        update (StartEdit globalId) init
                            |> Tuple.first
                            |> update (EditTask globalId "I am edited")
                            |> Tuple.first
                            |> update (ApplyEdit globalId)
                            |> Tuple.first
                            |> .currentTasks
                            |> toActionList
                            |> Expect.equal [ "I am edited" ]
                    )
        , test "CancelEdit restores state before edit" <|
            \_ ->
                testWithAction "Cancel my edit"
                    (\action ->
                        let
                            ( task, currentTasks ) =
                                Tasks.appendAndGetTask action initModel.currentTasks

                            globalId =
                                CurrentId <| Tasks.getId task

                            init =
                                { initModel
                                    | currentTasks = currentTasks
                                }
                        in
                        update (CancelEdit globalId) init
                            |> Tuple.first
                            >> .currentTasks
                            >> toActionList
                            |> Expect.equal [ "Cancel my edit" ]
                    )
        , test "BackgroundClicked applies the current edit and stops editing" <|
            \_ ->
                testWithAction "Edit me"
                    (\action ->
                        let
                            ( task, currentTasks ) =
                                Tasks.appendAndGetTask action initModel.currentTasks

                            globalId =
                                CurrentId <| Tasks.getId task

                            init =
                                { initModel
                                    | currentTasks = currentTasks
                                }
                        in
                        update (StartEdit globalId) init
                            |> Tuple.first
                            |> update (EditTask globalId "I am edited")
                            |> Tuple.first
                            |> update BackgroundClicked
                            |> Tuple.first
                            |> .currentTasks
                            |> toActionList
                            |> Expect.equal [ "I am edited" ]
                    )
        ]


expectModelEquals : Model -> ( Model, Cmd Msg ) -> Expectation
expectModelEquals expected =
    Tuple.first >> Expect.equal expected


testWithAction : String -> (Tasks.Action -> Expectation) -> Expectation
testWithAction rawAction test =
    case Tasks.actionFromString rawAction of
        Just action ->
            test action

        Nothing ->
            Expect.fail "Expected test string to be valid action."


expectEquivalentCollections : Tasks.Collection a -> Tasks.Collection b -> Expectation
expectEquivalentCollections first second =
    Expect.equalLists (toActionList first) (toActionList second)


{-| Used for comparing whether two collections compare the same actions, disregarding their ids.
-}
toActionList : Tasks.Collection c -> List String
toActionList =
    Tasks.toList >> List.map Tasks.readAction
