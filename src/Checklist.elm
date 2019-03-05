module Checklist exposing (Checklist, Id, fromList, get, insert, map, remove, update)

import List.Extra as List


type Checklist a
    = Checklist (List a)


type Id
    = Id Int


fromList : List a -> Checklist a
fromList items =
    Checklist items


insert : a -> Checklist a -> ( Id, Checklist a )
insert item (Checklist existing) =
    let
        newItems =
            existing ++ [ item ]

        index =
            List.length existing
    in
    ( Id index, Checklist newItems )


get : Id -> Checklist a -> Maybe a
get (Id index) (Checklist items) =
    List.getAt index items


map : (Id -> a -> b) -> Checklist a -> List b
map mapping (Checklist items) =
    List.indexedMap (\index item -> mapping (Id index) item) items


update : (a -> a) -> Id -> Checklist a -> Maybe (Checklist a)
update mapping id checklist =
    change (\a -> Just (mapping a)) id checklist


remove : Id -> Checklist a -> Maybe (Checklist a)
remove id checklist =
    change (\a -> Nothing) id checklist


change : (a -> Maybe a) -> Id -> Checklist a -> Maybe (Checklist a)
change mapping (Id index) (Checklist items) =
    if index < 0 then
        Nothing

    else
        let
            head =
                List.take index items

            tail =
                List.drop index items
        in
        case tail of
            x :: rest ->
                let
                    changed =
                        case mapping x of
                            Just a ->
                                [ a ]

                            Nothing ->
                                []
                in
                Just (Checklist <| head ++ changed ++ rest)

            _ ->
                Nothing
