module TestTasks exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra as List
import Tasks exposing (..)
import Test exposing (..)


type Collection
    = Current
    | Done


suite : Test
suite =
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
        , describe "Editing"
            [ test "cancels edit" <|
                \_ ->
                    testWithTask "Do not edit me"
                        (\task ->
                            Tasks.startEdit task
                                |> Tasks.edit "I have been edited"
                                |> Tasks.cancelEdit
                                |> Expect.equal task
                        )
            , test "applies edit" <|
                \_ ->
                    testWithTask "Edit me"
                        (Tasks.startEdit
                            >> Tasks.edit "I have been edited"
                            >> Tasks.applyEdit
                            >> Maybe.map
                                (Expect.all
                                    [ .action >> stringFromAction >> Expect.equal "I have been edited"
                                    , .editing >> Expect.equal notEditing
                                    ]
                                )
                            >> Maybe.withDefault (Expect.fail "Expected task to not be Nothing.")
                        )
            , test "does not apply illegal edit" <|
                \_ ->
                    testWithTask "Edit me"
                        (Tasks.startEdit
                            >> Tasks.edit ""
                            >> Tasks.applyEdit
                            >> Expect.equal Nothing
                        )
            , todo "ensures edit only on tasks being edited"
            ]
        ]


testWithTask : String -> (Task -> Expectation) -> Expectation
testWithTask rawAction test =
    case actionFromString rawAction of
        Just action ->
            test (Tasks.taskFromAction action)

        Nothing ->
            Expect.fail ("Expected '" ++ rawAction ++ "' to be a valid action.")
