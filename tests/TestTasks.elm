module TestTasks exposing (suite)

import Expect
import Main exposing (addTask)
import Test exposing (..)


suite : Test
suite =
    describe "Tasks"
        [ test "does not add empty tasks" <|
            \_ ->
                let
                    originalTasks =
                        [ "One", "Two" ]
                in
                addTask "" originalTasks
                    |> Expect.equal originalTasks
        , test "does not add tasks with only whitespace" <|
            \_ ->
                let
                    originalTasks =
                        [ "One", "Two" ]
                in
                addTask " " originalTasks
                    |> Expect.equal originalTasks
        ]
