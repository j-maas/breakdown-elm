module TestTasks exposing (suite)

import Expect
import Fuzz exposing (..)
import List.Extra as List
import Tasks exposing (..)
import Test exposing (..)


type Collection
    = Current
    | Done


suite : Test
suite =
    let
        fromList collection =
            List.filterMap actionFromString >> List.foldl addTask (empty collection)

        length =
            toList >> List.length

        readActionsList =
            toList >> List.map readAction

        indexFromShift shift max =
            if shift == 1 then
                0

            else
                max |> toFloat |> (*) shift |> floor
    in
    describe "Tasks"
        [ describe "Build"
            [ fuzz string "adds new task to list" <|
                \rawAction ->
                    let
                        mayBeAction =
                            actionFromString rawAction
                    in
                    case mayBeAction of
                        Just action ->
                            addTask action (empty Current)
                                |> readActionsList
                                |> Expect.equal [ stringFromAction action ]

                        Nothing ->
                            -- The fuzzer might generate invalid actions, which is not the fault of the module.
                            Expect.pass
            , fuzz string "stores action in new task" <|
                \rawAction ->
                    let
                        mayBeAction =
                            actionFromString rawAction
                    in
                    case mayBeAction of
                        Just action ->
                            addTask action (empty Current)
                                |> toList
                                |> List.map getAction
                                |> Expect.equal [ action ]

                        Nothing ->
                            -- The fuzzer might generate invalid actions, which is not the fault of the module.
                            Expect.pass
            , describe "Actions"
                [ test "creates valid action" <|
                    \_ ->
                        case actionFromString "I'm valid" of
                            Just action ->
                                stringFromAction action
                                    |> Expect.equal "I'm valid"

                            Nothing ->
                                Expect.fail "Should create action."
                , test "does not create empty actions" <|
                    \_ ->
                        actionFromString ""
                            |> Expect.equal Nothing
                , test "does not create actions with only whitespace" <|
                    \_ ->
                        actionFromString "  \t"
                            |> Expect.equal Nothing
                ]
            ]
        , let
            hasUniqueIds =
                toList >> List.allDifferentBy (getId >> idToComparable)
          in
          describe "IDs"
            [ fuzz (list string) "tasks have different ids" <|
                \rawActions ->
                    fromList Current rawActions
                        |> hasUniqueIds
                        |> Expect.true "Detected duplicate ids."
            , fuzz3 (list string) (list string) percentage "moving tasks keeps unique ids" <|
                \rawActionsCurrent rawActionsDone shift ->
                    let
                        current =
                            fromList Current rawActionsCurrent

                        done =
                            fromList Done rawActionsDone

                        mayBeTask =
                            List.getAt (indexFromShift shift <| length current) (toList current)
                    in
                    case mayBeTask of
                        Just task ->
                            moveTask (getId task) current done
                                |> Tuple.mapBoth hasUniqueIds hasUniqueIds
                                |> Expect.equal ( True, True )

                        Nothing ->
                            -- current collection might be empty, then task can legitimately not be found.
                            if length current == 0 then
                                Expect.pass

                            else
                                Expect.fail "Did not find task in list."
            ]
        , describe "Editing"
            [ fuzz3 (list string) string percentage "edits task's action" <|
                \rawActions rawNewAction shift ->
                    let
                        tasks =
                            fromList Current rawActions

                        index =
                            indexFromShift shift <| length tasks

                        mayBeTask =
                            List.getAt index (toList tasks)

                        mayBeAction =
                            actionFromString rawNewAction

                        expectedActions newAction =
                            List.setAt index (stringFromAction newAction) (readActionsList tasks)
                    in
                    case mayBeTask of
                        Just task ->
                            case mayBeAction of
                                Just action ->
                                    editTask (getId task) action tasks
                                        |> readActionsList
                                        |> Expect.equal (expectedActions action)

                                Nothing ->
                                    -- The fuzzer might generate invalid actions, which is not the fault of the module.
                                    Expect.pass

                        Nothing ->
                            -- The fuzzer might generate empty collections, which is not the fault of the module.
                            Expect.pass
            , fuzz2 (list string) percentage "removes from list" <|
                \rawActions shift ->
                    let
                        tasks =
                            fromList Current rawActions

                        index =
                            indexFromShift shift <| length tasks

                        mayBeTask =
                            List.getAt index (toList tasks)
                    in
                    case mayBeTask of
                        Just task ->
                            removeTask (getId task) tasks
                                |> readActionsList
                                |> Expect.equal (List.removeAt index <| readActionsList tasks)

                        Nothing ->
                            -- The fuzzer might generate empty collections, which is not the fault of the module.
                            Expect.pass
            , fuzz3 (list string) (list string) percentage "moves task between lists" <|
                \rawActionsCurrent rawActionsDone shift ->
                    let
                        current =
                            fromList Current rawActionsCurrent

                        done =
                            fromList Done rawActionsDone

                        index =
                            indexFromShift shift <| length current

                        mayBeTask =
                            List.getAt index (toList current)

                        expectedActions movedAction =
                            ( readActionsList current |> List.removeAt index
                            , readActionsList done ++ [ movedAction ]
                            )
                    in
                    case mayBeTask of
                        Just task ->
                            moveTask (getId task) current done
                                |> Tuple.mapBoth readActionsList readActionsList
                                |> Expect.equal (expectedActions <| readAction task)

                        Nothing ->
                            -- The fuzzer might generate empty collections, which is not the fault of the module.
                            Expect.pass
            ]
        ]
