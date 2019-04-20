module TestCheckTree exposing (suite)

import CheckTree exposing (CheckTree, Node(..))
import Checklist
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, percentage, string)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Random
import Random.Extra as Random
import Test exposing (..)
import Todo exposing (Todo)
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "CheckTree"
        [ test "returns Node by id" <|
            \_ ->
                let
                    testTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [ L "Inserted, too" ], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }
                in
                CheckTree.insertCurrent
                    (CheckTree.makeCompositNode
                        "Inserted"
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            CheckTree.insertCurrentAt id
                                (SimpleNode "Inserted, too")
                                identity
                                tree
                       )
                    |> Maybe.map
                        (\( id, tree ) ->
                            Expect.equal expectedTree tree
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "updates Node by id" <|
            \_ ->
                let
                    testTree =
                        checkTreeForTest
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [ L "Updated" ], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }
                in
                CheckTree.insertCurrent
                    (CheckTree.makeCompositNode
                        "Inserted"
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            CheckTree.insertCurrentAt id
                                (SimpleNode "Inserted, too")
                                identity
                                tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            CheckTree.update (\_ -> SimpleNode "Updated")
                                id
                                tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "removes Node by id" <|
            \_ ->
                let
                    testTree =
                        checkTreeForTest
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }
                in
                CheckTree.insertCurrent
                    (CheckTree.makeCompositNode
                        "Inserted"
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            CheckTree.insertCurrentAt id
                                (SimpleNode "Inserted, too")
                                identity
                                tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            CheckTree.remove id tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "moves to current by id" <|
            \_ ->
                let
                    testTree =
                        checkTreeForTest
                            { c = [ L "Hi", N "Children" { c = [], d = [ L "Other hi" ] } ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [ L "Inserted, too" ], d = [] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }
                in
                CheckTree.insertCurrent
                    (CheckTree.makeCompositNode
                        "Inserted"
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            CheckTree.insertDoneAt id
                                (SimpleNode "Inserted, too")
                                identity
                                tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            CheckTree.moveToCurrent id tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , test "moves to done by id" <|
            \_ ->
                let
                    testTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }

                    expectedTree =
                        checkTreeForTest
                            { c =
                                [ L "Hi"
                                , N "Children" { c = [], d = [ L "Other hi" ] }
                                , N "Inserted" { c = [], d = [ L "Inserted, too" ] }
                                ]
                            , d = [ N "More children" { c = [ L "Hi" ], d = [] } ]
                            }
                in
                CheckTree.insertCurrent
                    (CheckTree.makeCompositNode
                        "Inserted"
                        (Checklist.fromItems { current = [], done = [] })
                    )
                    testTree
                    |> (\( id, tree ) ->
                            CheckTree.insertCurrentAt id
                                (SimpleNode "Inserted, too")
                                identity
                                tree
                       )
                    |> Maybe.andThen
                        (\( id, tree ) ->
                            CheckTree.moveToDone id tree
                        )
                    |> Maybe.map (Expect.equal expectedTree)
                    |> Maybe.withDefault (Expect.fail "Expected to find id.")
        , fuzz nodeFuzzer "encoding and decoding results in same node" <|
            \node ->
                CheckTree.encodeNode Encode.int Encode.int node
                    |> Decode.decodeValue (CheckTree.nodeDecoder Decode.int Decode.int)
                    |> Expect.equal (Ok node)
        ]


nodeFuzzer : Fuzzer (Node Int Int)
nodeFuzzer =
    let
        shortList fuzzer =
            Fuzz.map3
                (\a b c -> List.filterMap identity [ a, b, c ])
                (maybe fuzzer)
                (maybe fuzzer)
                (maybe fuzzer)

        simpleNodeFuzzer =
            Fuzz.map SimpleNode int

        compositNodeFuzzer levels =
            case levels of
                0 ->
                    simpleNodeFuzzer

                n ->
                    Fuzz.map3
                        (\node current done ->
                            CheckTree.makeCompositNode
                                node
                                (Checklist.fromItems { current = current, done = done })
                        )
                        int
                        (shortList <| compositNodeFuzzer (n - 1))
                        (shortList <| compositNodeFuzzer (n - 1))
    in
    Fuzz.oneOf
        [ simpleNodeFuzzer
        , compositNodeFuzzer 2
        ]


type TestTree
    = N String { c : List TestTree, d : List TestTree }
    | L String


checkTreeForTest : { c : List TestTree, d : List TestTree } -> CheckTree String String
checkTreeForTest init =
    let
        mapTree : TestTree -> Node String String
        mapTree tree =
            case tree of
                N string children ->
                    CheckTree.makeCompositNode string
                        (Checklist.fromItems
                            { current = List.map mapTree children.c
                            , done = List.map mapTree children.d
                            }
                        )

                L string ->
                    SimpleNode string
    in
    CheckTree.fromItems
        { current = List.map mapTree init.c
        , done = List.map mapTree init.d
        }
