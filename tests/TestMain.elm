module TestMain exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Main exposing (Model, Msg(..), initModel, update)
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
