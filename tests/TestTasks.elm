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
            List.filterMap actionFromString >> List.foldl appendTask (empty collection)

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
        [ describe "Actions"
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
