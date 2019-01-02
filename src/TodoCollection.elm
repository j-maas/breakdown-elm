module TodoCollection exposing
    ( Id
    , Selector(..)
    , TodoCollection
    , Zipper
    , current
    , empty
    , find
    , fromList
    , init
    , insert
    , mapToList
    , mapTodo
    , move
    , put
    , remove
    , selectorFromId
    )

import Dict exposing (Dict)
import List.Zipper as GenericZipper
import Todo exposing (Todo)
import TodoList exposing (TodoList)
import Utils.ZipperUtils as GenericZipper


type TodoCollection
    = TodoCollection
        { current : TodoList
        , done : TodoList
        }


type Selector
    = Current
    | Done


getTodoListInCollection : Selector -> TodoCollection -> TodoList
getTodoListInCollection selector (TodoCollection collection) =
    case selector of
        Current ->
            collection.current

        Done ->
            collection.done


mapTodoListInCollection : Selector -> (TodoList -> TodoList) -> TodoCollection -> TodoCollection
mapTodoListInCollection selector map (TodoCollection collection) =
    case selector of
        Current ->
            TodoCollection { collection | current = map collection.current }

        Done ->
            TodoCollection { collection | done = map collection.done }


type Id
    = Id Selector TodoList.Id


selectorFromId : Id -> Selector
selectorFromId (Id selector _) =
    selector


empty : TodoCollection
empty =
    TodoCollection
        { current = TodoList.fromList []
        , done = TodoList.fromList []
        }


init : { current : List Todo, done : List Todo } -> TodoCollection
init lists =
    empty
        |> fromList Current lists.current
        |> fromList Done lists.done


fromList : Selector -> List Todo -> TodoCollection -> TodoCollection
fromList selector list collection =
    let
        newTodos =
            TodoList.fromList list
    in
    mapTodoListInCollection selector (\_ -> newTodos) collection


insert : Selector -> Todo -> TodoCollection -> ( Id, TodoCollection )
insert selector todo collection =
    getTodoListInCollection selector collection
        |> TodoList.insert todo
        |> (\( id, newList ) ->
                let
                    collectionId =
                        Id selector id

                    newCollection =
                        mapTodoListInCollection selector (\_ -> newList) collection
                in
                ( collectionId, newCollection )
           )


put : Selector -> Todo -> TodoCollection -> TodoCollection
put selector todo collection =
    insert selector todo collection |> Tuple.second


mapToList : Selector -> (Id -> Todo -> a) -> TodoCollection -> List a
mapToList selector map collection =
    getTodoListInCollection selector collection
        |> TodoList.mapToList (\listId todo -> map (Id selector listId) todo)


find : Id -> TodoCollection -> Maybe Zipper
find (Id selector listId) collection =
    getTodoListInCollection selector collection
        |> TodoList.find listId
        |> Maybe.map (Zipper selector collection)


mapTodo : (Todo -> Todo) -> Zipper -> TodoCollection
mapTodo map (Zipper selector collection zipper) =
    mapTodoListInCollection selector (\_ -> TodoList.mapTodo map zipper) collection


remove : Zipper -> TodoCollection
remove (Zipper selector collection zipper) =
    mapTodoListInCollection selector (\_ -> TodoList.remove zipper) collection


move : Zipper -> TodoCollection
move (Zipper selector collection zipper) =
    let
        todo =
            GenericZipper.current zipper

        toList =
            case selector of
                Current ->
                    Done

                Done ->
                    Current
    in
    mapTodoListInCollection selector (\_ -> TodoList.remove zipper) collection
        |> mapTodoListInCollection toList (\list -> TodoList.put todo list)



-- ZIPPER


type Zipper
    = Zipper Selector TodoCollection (GenericZipper.Zipper Todo)


current : Zipper -> Todo
current (Zipper _ _ zipper) =
    GenericZipper.current zipper
