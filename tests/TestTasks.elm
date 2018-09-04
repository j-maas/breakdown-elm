module TestTasks exposing (suite)

import Expect
import List.Extra as List
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
        , let
            fromList =
                List.foldl addTask (empty Current)
          in
          describe "Ids"
            [ test "tasks in same list have different ids" <|
                \_ ->
                    fromList [ "Same", "Different", "Same" ]
                        |> toList
                        |> List.allDifferentBy (getId >> idToComparable)
                        |> Expect.true "Detected duplicate ids."
            , todo "tasks across different lists have different ids"
            ]
        , describe "Editing"
            [ todo "edits task's action"
            , todo "moves task between lists"
            ]
        ]
