module TestTodoTree exposing (suite)

import Checklist
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, percentage, string)
import Json.Decode as Decode
import List.Extra as List
import Random
import Random.Extra as Random
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
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [ L "Inserted, too" ], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
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
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [ L "Updated" ], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
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
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
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
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [ L "Inserted, too" ], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
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
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        todoTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [], d = [ L "Inserted, too" ] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
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
        , fuzz todoNodeFuzzer "encoding and decoding results in same node" <|
            \node ->
                TodoTree.encodeNode node
                    |> Decode.decodeValue TodoTree.nodeDecoder
                    |> Expect.equal (Ok node)
        ]


todoNodeFuzzer : Fuzzer TodoNode
todoNodeFuzzer =
    let
        shortList fuzzer =
            Fuzz.map3
                (\a b c -> List.filterMap identity [ a, b, c ])
                (maybe fuzzer)
                (maybe fuzzer)
                (maybe fuzzer)

        todoFuzzer =
            Fuzz.map Todo.fromAction
                NonEmptyString.fuzzer

        simpleTodoFuzzer =
            todoFuzzer |> Fuzz.map SimpleTodo

        compositTodoFuzzer levels =
            case levels of
                0 ->
                    simpleTodoFuzzer

                n ->
                    Fuzz.map3
                        (\todo current done ->
                            CompositTodo todo (Checklist.fromItems { current = current, done = done })
                        )
                        todoFuzzer
                        (shortList <| compositTodoFuzzer (n - 1))
                        (shortList <| compositTodoFuzzer (n - 1))
    in
    Fuzz.oneOf
        [ simpleTodoFuzzer
        , compositTodoFuzzer 2
        ]


type TestTree
    = N String { c : List TestTree, d : List TestTree }
    | L String


makeTodo : Char -> String -> Todo
makeTodo first rest =
    NonEmptyString.build first rest
        |> Todo.fromAction


todoTreeForTest : { c : List TestTree, d : List TestTree } -> TodoTree
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
                                        { current = List.filterMap mapTree children.c
                                        , done = List.filterMap mapTree children.d
                                        }
                                    )
                            )

                L string ->
                    NonEmptyString.fromString string
                        |> Maybe.map (\action -> SimpleTodo (Todo.fromAction action))
    in
    TodoTree.fromItems
        { current = List.filterMap mapTree init.c
        , done = List.filterMap mapTree init.d
        }
