module Checklist exposing (Checklist, fromList, get, insert)

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
