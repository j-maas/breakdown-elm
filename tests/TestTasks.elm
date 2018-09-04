module TestTasks exposing (suite)

import Expect
import List.Extra as List
import Tasks exposing (..)
import Test exposing (..)


type Collection
    = Current
    | Done


suite : Test
suite =
    let
        fromList =
            List.foldl addTask (empty Current)

        readActionsList =
            toList >> List.map readAction
    in
    describe "Tasks"
        [ describe "Build"
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
        , test "tasks have different ids" <|
            \_ ->
                fromList [ "Same", "Different", "Same" ]
                    |> toList
                    |> List.allDifferentBy (getId >> idToComparable)
                    |> Expect.true "Detected duplicate ids."
        , describe "Editing"
            [ test "edits task's action" <|
                \_ ->
                    let
                        tasks =
                            fromList [ "One", "Too" ]

                        mayBeTask =
                            List.getAt 1 (toList tasks)

                        action =
                            unsafeActionFromString "Two"
                    in
                    case mayBeTask of
                        Just task ->
                            editTask (getId task) action tasks
                                |> readActionsList
                                |> Expect.equal [ "One", "Two" ]

                        Nothing ->
                            Expect.fail "Did not find task in list."
            , test "remove from list" <|
                \_ ->
                    let
                        tasks =
                            fromList [ "Keep me", "Remove me" ]

                        mayBeTask =
                            List.getAt 1 (toList tasks)
                    in
                    case mayBeTask of
                        Just task ->
                            removeTask (getId task) tasks
                                |> readActionsList
                                |> Expect.equal [ "Keep me" ]

                        Nothing ->
                            Expect.fail "Did not find task in list."
            , test "moves task between lists" <|
                \_ ->
                    let
                        current =
                            fromList [ "One", "Two", "Three" ]

                        done =
                            empty Done

                        mayBeTask =
                            List.getAt 1 (toList current)
                    in
                    case mayBeTask of
                        Just task ->
                            moveTask (getId task) current done
                                |> Tuple.mapBoth readActionsList readActionsList
                                |> Expect.equal ( [ "One", "Three" ], [ "Two" ] )

                        Nothing ->
                            Expect.fail "Did not find task in list."
            ]
        ]
