module TestSelectCollection exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import IdList exposing (IdList)
import List.Zipper as Zipper
import SelectCollection exposing (SelectCollection)
import Test exposing (..)
import Todo exposing (Todo)
import Utils.NonEmptyString as NonEmptyString


type alias TodoList =
    IdList Todo


suite : Test
suite =
    describe "TodoCollection"
        [ test "inits empty collection" <|
            \_ ->
                SelectCollection.empty
                    |> Expect.all
                        [ getTodos SelectCollection.Current >> Expect.equal []
                        , getTodos SelectCollection.Done >> Expect.equal []
                        ]
        , test "inits from lists" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'I' "nsert me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'I' "nsert me, too!")

                    todo3 =
                        Todo.fromAction (NonEmptyString.build 'A' "nd me, too!")
                in
                SelectCollection.init { current = [ todo1 ], done = [ todo2, todo3 ] }
                    |> Expect.all
                        [ getTodos SelectCollection.Current >> Expect.equal [ todo1 ]
                        , getTodos SelectCollection.Done >> Expect.equal [ todo2, todo3 ]
                        ]
        , test "puts todos from list into one collection and retrieves them" <|
            \_ ->
                let
                    todo =
                        Todo.fromAction (NonEmptyString.build 'I' "nsert me!")
                in
                SelectCollection.empty
                    |> SelectCollection.fromList SelectCollection.Current [ todo ]
                    |> SelectCollection.mapToList SelectCollection.Current (\_ t -> t)
                    |> Expect.equal [ todo ]
        , test "todos added to one collection are not retrieved with another" <|
            \_ ->
                let
                    todo =
                        Todo.fromAction (NonEmptyString.build 'D' "o not get me!")
                in
                SelectCollection.empty
                    |> SelectCollection.fromList SelectCollection.Current [ todo ]
                    |> SelectCollection.mapToList SelectCollection.Done (\_ t -> t)
                    |> Expect.equal []
        , test "finds todo by id across collections" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.fromAction (NonEmptyString.build 'G' "et me!")
                in
                SelectCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> SelectCollection.insert SelectCollection.Current todo3
                    |> (\( id, collection ) ->
                            SelectCollection.find id collection
                       )
                    |> Maybe.map
                        (SelectCollection.current
                            >> Expect.equal todo3
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        , test "updates todo by id, leaving id the same" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.fromAction (NonEmptyString.build 'C' "hange me!")
                in
                SelectCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> SelectCollection.insert SelectCollection.Current todo3
                    |> (\( id, collection ) ->
                            SelectCollection.find id collection
                                |> Maybe.map
                                    (\zipper ->
                                        let
                                            newAction =
                                                NonEmptyString.build 'C' "hanged."

                                            changedTodo =
                                                Todo.fromAction newAction
                                        in
                                        SelectCollection.mapItem (Todo.setAction newAction) zipper
                                            |> getTodos SelectCollection.Current
                                            |> Expect.equal [ todo1, changedTodo ]
                                    )
                       )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        , test "removes from correct collection" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.fromAction (NonEmptyString.build 'R' "emove me!")
                in
                SelectCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> SelectCollection.insert SelectCollection.Current todo3
                    |> (\( id, collection ) ->
                            SelectCollection.find id collection
                       )
                    |> Maybe.map
                        (\zipper ->
                            SelectCollection.remove zipper
                                |> Expect.all
                                    [ getTodos SelectCollection.Current >> Expect.equal [ todo1 ]
                                    , getTodos SelectCollection.Done >> Expect.equal [ todo2 ]
                                    ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        , test "moves todo between collections" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.fromAction (NonEmptyString.build 'M' "ove me!")
                in
                SelectCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> SelectCollection.insert SelectCollection.Current todo3
                    |> (\( id, collection ) ->
                            SelectCollection.find id collection
                       )
                    |> Maybe.map
                        (\zipper ->
                            SelectCollection.move zipper
                                |> Expect.all
                                    [ getTodos SelectCollection.Current >> Expect.equal [ todo1 ]
                                    , getTodos SelectCollection.Done >> Expect.equal [ todo2, todo3 ]
                                    ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        ]


getTodos : SelectCollection.Selector -> SelectCollection Todo -> List Todo
getTodos selector collection =
    SelectCollection.mapToList selector (\_ todo -> todo) collection
