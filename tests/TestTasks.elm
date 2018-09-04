module TestTasks exposing (suite)

import Expect
import Tasks exposing (..)
import Test exposing (..)


type Collection
    = Current


suite : Test
suite =
    describe "Tasks"
        [ let
            readActionsList =
                toList >> List.map readAction
          in
          describe "Build"
            [ test "adds new task to list" <|
                \_ ->
                    addTask "Add me" (empty Current)
                        |> readActionsList
                        |> Expect.equal [ "Add me" ]
            , test "does not add empty actions" <|
                \_ ->
                    addTask "" (empty Current)
                        |> readActionsList
                        |> Expect.equal []
            , test "does not add actions with only whitespace" <|
                \_ ->
                    addTask "  \t" (empty Current)
                        |> readActionsList
                        |> Expect.equal []
            ]
        , describe "Editing"
            [ todo "edits task's action"
            , todo "moves task between lists"
            ]
        , describe "Ids"
            [ todo "tasks in same list have different ids"
            , todo "tasks across different lists have different ids"
            ]
        ]
