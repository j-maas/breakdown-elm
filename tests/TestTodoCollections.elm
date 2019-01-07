module TestTodoCollections exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import IdList exposing (IdList)
import List.Zipper as Zipper
import Test exposing (..)
import Todo exposing (Todo)
import TodoCollection exposing (TodoCollection)
import Utils.NonEmptyString as NonEmptyString


type alias TodoList =
    IdList Todo


suite : Test
suite =
    describe "TodoCollection"
        [ test "inits empty collection" <|
            \_ ->
                TodoCollection.empty
                    |> Expect.all
                        [ getTodos TodoCollection.Current >> Expect.equal []
                        , getTodos TodoCollection.Done >> Expect.equal []
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
                TodoCollection.init { current = [ todo1 ], done = [ todo2, todo3 ] }
                    |> Expect.all
                        [ getTodos TodoCollection.Current >> Expect.equal [ todo1 ]
                        , getTodos TodoCollection.Done >> Expect.equal [ todo2, todo3 ]
                        ]
        , test "puts todos from list into one collection and retrieves them" <|
            \_ ->
                let
                    todo =
                        Todo.fromAction (NonEmptyString.build 'I' "nsert me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo ]
                    |> TodoCollection.mapToList TodoCollection.Current (\_ t -> t)
                    |> Expect.equal [ todo ]
        , test "todos added to one collection are not retrieved with another" <|
            \_ ->
                let
                    todo =
                        Todo.fromAction (NonEmptyString.build 'D' "o not get me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo ]
                    |> TodoCollection.mapToList TodoCollection.Done (\_ t -> t)
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
                TodoCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                       )
                    |> Maybe.map
                        (TodoCollection.current
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
                TodoCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                                |> Maybe.map
                                    (\zipper ->
                                        let
                                            newAction =
                                                NonEmptyString.build 'C' "hanged."

                                            changedTodo =
                                                Todo.fromAction newAction
                                        in
                                        TodoCollection.mapItem (Todo.setAction newAction) zipper
                                            |> getTodos TodoCollection.Current
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
                TodoCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                       )
                    |> Maybe.map
                        (\zipper ->
                            TodoCollection.remove zipper
                                |> Expect.all
                                    [ getTodos TodoCollection.Current >> Expect.equal [ todo1 ]
                                    , getTodos TodoCollection.Done >> Expect.equal [ todo2 ]
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
                TodoCollection.init { current = [ todo1 ], done = [ todo2 ] }
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                       )
                    |> Maybe.map
                        (\zipper ->
                            TodoCollection.move zipper
                                |> Expect.all
                                    [ getTodos TodoCollection.Current >> Expect.equal [ todo1 ]
                                    , getTodos TodoCollection.Done >> Expect.equal [ todo2, todo3 ]
                                    ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        ]


getTodos : TodoCollection.Selector -> TodoCollection -> List Todo
getTodos selector collection =
    TodoCollection.mapToList selector (\_ todo -> todo) collection
