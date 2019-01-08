module SelectCollection exposing
    ( Id
    , SelectCollection
    , Selector(..)
    , Zipper
    , current
    , empty
    , find
    , fromList
    , init
    , insert
    , mapItem
    , mapToList
    , move
    , put
    , remove
    , selectorFromId
    )

import Dict exposing (Dict)
import IdList exposing (IdList)
import List.Zipper as GenericZipper
import Utils.ZipperUtils as GenericZipper


type SelectCollection a
    = SelectCollection
        { current : IdList a
        , done : IdList a
        }


type Selector
    = Current
    | Done


getTodoListInCollection : Selector -> SelectCollection a -> IdList a
getTodoListInCollection selector (SelectCollection collection) =
    case selector of
        Current ->
            collection.current

        Done ->
            collection.done


mapTodoListInCollection : Selector -> (IdList a -> IdList a) -> SelectCollection a -> SelectCollection a
mapTodoListInCollection selector map (SelectCollection collection) =
    case selector of
        Current ->
            SelectCollection { collection | current = map collection.current }

        Done ->
            SelectCollection { collection | done = map collection.done }


type Id
    = Id Selector IdList.Id


selectorFromId : Id -> Selector
selectorFromId (Id selector _) =
    selector


empty : SelectCollection a
empty =
    SelectCollection
        { current = IdList.fromList []
        , done = IdList.fromList []
        }


init : { current : List a, done : List a } -> SelectCollection a
init lists =
    empty
        |> fromList Current lists.current
        |> fromList Done lists.done


fromList : Selector -> List a -> SelectCollection a -> SelectCollection a
fromList selector list collection =
    let
        newItems =
            IdList.fromList list
    in
    mapTodoListInCollection selector (\_ -> newItems) collection


insert : Selector -> a -> SelectCollection a -> ( Id, SelectCollection a )
insert selector item collection =
    getTodoListInCollection selector collection
        |> IdList.insert item
        |> (\( id, newList ) ->
                let
                    collectionId =
                        Id selector id

                    newCollection =
                        mapTodoListInCollection selector (\_ -> newList) collection
                in
                ( collectionId, newCollection )
           )


put : Selector -> a -> SelectCollection a -> SelectCollection a
put selector item collection =
    insert selector item collection |> Tuple.second


mapToList : Selector -> (Id -> a -> b) -> SelectCollection a -> List b
mapToList selector map collection =
    getTodoListInCollection selector collection
        |> IdList.mapToList (\listId item -> map (Id selector listId) item)


find : Id -> SelectCollection a -> Maybe (Zipper a)
find (Id selector listId) collection =
    getTodoListInCollection selector collection
        |> IdList.find listId
        |> Maybe.map (Zipper selector collection)


mapItem : (a -> a) -> Zipper a -> SelectCollection a
mapItem map (Zipper selector collection zipper) =
    mapTodoListInCollection selector (\_ -> IdList.mapItem map zipper) collection


remove : Zipper a -> SelectCollection a
remove (Zipper selector collection zipper) =
    mapTodoListInCollection selector (\_ -> IdList.remove zipper) collection


move : Zipper a -> SelectCollection a
move (Zipper selector collection zipper) =
    let
        item =
            GenericZipper.current zipper

        toList =
            case selector of
                Current ->
                    Done

                Done ->
                    Current
    in
    mapTodoListInCollection selector (\_ -> IdList.remove zipper) collection
        |> mapTodoListInCollection toList (\list -> IdList.put item list)



-- ZIPPER


type Zipper a
    = Zipper Selector (SelectCollection a) (GenericZipper.Zipper a)


current : Zipper a -> a
current (Zipper _ _ zipper) =
    GenericZipper.current zipper
