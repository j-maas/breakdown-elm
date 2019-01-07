module IdList exposing
    ( Id
    , IdList
    , find
    , fromList
    , insert
    , mapItem
    , mapToList
    , put
    , remove
    )

import List.Zipper as Zipper exposing (Zipper)
import Utils.ZipperUtils as Zipper


type IdList a
    = IdList (List a)


type Id
    = Id Int


fromList : List a -> IdList a
fromList list =
    IdList list


insert : a -> IdList a -> ( Id, IdList a )
insert item (IdList items) =
    let
        newId =
            Id (List.length items)
    in
    ( newId, IdList (items ++ [ item ]) )


put : a -> IdList a -> IdList a
put item list =
    insert item list |> Tuple.second


find : Id -> IdList a -> Maybe (Zipper a)
find id (IdList items) =
    let
        index =
            case id of
                Id i ->
                    i
    in
    Zipper.fromList items
        |> Maybe.andThen (Zipper.focusIndex index)


mapItem : (a -> a) -> Zipper a -> IdList a
mapItem map zipper =
    IdList (Zipper.mapCurrent map zipper |> Zipper.toList)


mapToList : (Id -> a -> b) -> IdList a -> List b
mapToList map (IdList items) =
    List.indexedMap (\id item -> map (Id id) item) items


remove : Zipper a -> IdList a
remove zipper =
    IdList (Zipper.remove zipper)
