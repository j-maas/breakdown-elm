module TestMain exposing (suite)

import Expect
import Fuzz exposing (..)
import Main exposing (Model, Msg(..), initModel, update)
import Test exposing (..)


suite : Test
suite =
    let
        expectModelEquals a =
            Tuple.first >> Expect.equal a
    in
    describe "messages"
        [ test "NoOp keeps model the same" <|
            \_ ->
                update NoOp initModel
                    |> expectModelEquals initModel
        , test "UpdateNewTask saves the new task" <|
            \_ ->
                update (UpdateNewTask "i am an edit") initModel
                    |> expectModelEquals { initModel | newTask = "i am an edit" }
        ]
