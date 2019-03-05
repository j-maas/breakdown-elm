module TestTodoTree exposing (suite)

import Checklist
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, percentage, string)
import List.Extra as List
import Test exposing (..)
import Todo exposing (Todo)
import TodoTree exposing (TodoNode(..), TodoTree)
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "TodoTree"
        [ test "returns TodoNode by id" <|
            \_ ->
                let
                    testTree =
                        todoTreeForTest
                            { current = [ L "Hi", N "Children" { current = [], done = [ L "Other hi" ] } ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { current =
                                [ L "Hi"
                                , N "Children" { current = [], done = [ L "Other hi" ] }
                                , N "Inserted" { current = [ L "Inserted, too" ], done = [] }
                                ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }
                in
                TodoTree.insertCurrent
                    (CompositTodo
                        (makeTodo 'I' "nserted")
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            TodoTree.insertCurrentAt id (SimpleTodo (makeTodo 'I' "nserted, too")) tree
                       )
                    |> Maybe.map
                        (\( id, tree ) ->
                            Expect.equal expectedTree tree
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "updates TodoNode by id" <|
            \_ ->
                let
                    testTree =
                        todoTreeForTest
                            { current = [ L "Hi", N "Children" { current = [], done = [ L "Other hi" ] } ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { current =
                                [ L "Hi"
                                , N "Children" { current = [], done = [ L "Other hi" ] }
                                , N "Inserted" { current = [ L "Updated" ], done = [] }
                                ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }
                in
                TodoTree.insertCurrent
                    (CompositTodo
                        (makeTodo 'I' "nserted")
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            TodoTree.insertCurrentAt id (SimpleTodo (makeTodo 'I' "nserted, too")) tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            TodoTree.update (\_ -> SimpleTodo (makeTodo 'U' "pdated"))
                                id
                                tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "removes TodoNode by id" <|
            \_ ->
                let
                    testTree =
                        todoTreeForTest
                            { current = [ L "Hi", N "Children" { current = [], done = [ L "Other hi" ] } ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { current =
                                [ L "Hi"
                                , N "Children" { current = [], done = [ L "Other hi" ] }
                                , N "Inserted" { current = [], done = [] }
                                ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }
                in
                TodoTree.insertCurrent
                    (CompositTodo
                        (makeTodo 'I' "nserted")
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            TodoTree.insertCurrentAt id (SimpleTodo (makeTodo 'I' "nserted, too")) tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            TodoTree.remove id tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "moves to current by id" <|
            \_ ->
                let
                    testTree =
                        todoTreeForTest
                            { current = [ L "Hi", N "Children" { current = [], done = [ L "Other hi" ] } ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { current =
                                [ L "Hi"
                                , N "Children" { current = [], done = [ L "Other hi" ] }
                                , N "Inserted" { current = [ L "Inserted, too" ], done = [] }
                                ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }
                in
                TodoTree.insertCurrent
                    (CompositTodo
                        (makeTodo 'I' "nserted")
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            TodoTree.insertDoneAt id (SimpleTodo (makeTodo 'I' "nserted, too")) tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            TodoTree.moveToCurrent id tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "moves to done by id" <|
            \_ ->
                let
                    testTree =
                        todoTreeForTest
                            { current = [ L "Hi", N "Children" { current = [], done = [ L "Other hi" ] } ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { current =
                                [ L "Hi"
                                , N "Children" { current = [], done = [ L "Other hi" ] }
                                , N "Inserted" { current = [], done = [ L "Inserted, too" ] }
                                ]
                            , done = [ N "More children" { current = [ L "Hi" ], done = [] } ]
                            }
                in
                TodoTree.insertCurrent
                    (CompositTodo
                        (makeTodo 'I' "nserted")
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            TodoTree.insertCurrentAt id (SimpleTodo (makeTodo 'I' "nserted, too")) tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            TodoTree.moveToDone id tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        ]


type TestTree
    = N String { current : List TestTree, done : List TestTree }
    | L String


makeTodo : Char -> String -> Todo
makeTodo first rest =
    NonEmptyString.build first rest
        |> Todo.fromAction


todoTreeForTest : { current : List TestTree, done : List TestTree } -> TodoTree
todoTreeForTest init =
    let
        mapTree tree =
            case tree of
                N string children ->
                    NonEmptyString.fromString string
                        |> Maybe.map
                            (\action ->
                                CompositTodo (Todo.fromAction action)
                                    (Checklist.fromItems
                                        { current = List.filterMap mapTree children.current
                                        , done = List.filterMap mapTree children.done
                                        }
                                    )
                            )

                L string ->
                    NonEmptyString.fromString string
                        |> Maybe.map (\action -> SimpleTodo (Todo.fromAction action))
    in
    TodoTree.fromItems
        { current = List.filterMap mapTree init.current
        , done = List.filterMap mapTree init.done
        }
