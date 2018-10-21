module IdCollection exposing (Entry, Id, IdCollection, append, appendAndGetEntry, empty, fromList, get, idToComparable, move, remove, removeAndGet, set, toList, update)

import List.Extra as List


type IdCollection tag item
    = IdCollection (List (Entry tag item))


type alias Entry tag item =
    { id : Id tag
    , item : item
    }


type Id tag
    = Id Int


idToComparable : Id tag -> Int
idToComparable (Id id) =
    id


empty : tag -> IdCollection tag item
empty _ =
    IdCollection []


append : item -> IdCollection tag item -> IdCollection tag item
append item collection =
    appendAndGetEntry item collection |> Tuple.second


appendAndGetEntry : item -> IdCollection tag item -> ( Entry tag item, IdCollection tag item )
appendAndGetEntry item (IdCollection collection) =
    let
        nextId =
            List.length collection + 1

        newEntry =
            { id = Id nextId, item = item }
    in
    ( newEntry, IdCollection (collection ++ [ newEntry ]) )


remove : Id tag -> IdCollection tag item -> IdCollection tag item
remove id collection =
    removeAndGet id collection |> Tuple.second


removeAndGet : Id tag -> IdCollection tag item -> ( Maybe item, IdCollection tag item )
removeAndGet id (IdCollection collection) =
    let
        toRemove =
            List.find (\entry -> entry.id == id) collection

        removed =
            Maybe.map .item toRemove

        newCollection =
            case toRemove of
                Just entry ->
                    List.remove entry collection

                Nothing ->
                    collection
    in
    ( removed, newCollection |> IdCollection )


set : item -> Id tag -> IdCollection tag item -> IdCollection tag item
set newItem =
    update (\_ -> newItem)


update : (item -> item) -> Id tag -> IdCollection tag item -> IdCollection tag item
update newItem id (IdCollection collection) =
    let
        updateEntry entry =
            { entry | item = newItem entry.item }
    in
    List.updateIf (\entry -> entry.id == id) updateEntry collection |> IdCollection


fromList : tag -> List item -> IdCollection tag item
fromList tag list =
    List.foldl (\item collection -> append item collection) (empty tag) list


get : Id tag -> IdCollection tag item -> Maybe item
get id (IdCollection collection) =
    List.find (\entry -> entry.id == id) collection |> Maybe.map .item


toList : IdCollection tag item -> List (Entry tag item)
toList (IdCollection collection) =
    collection


move : Id from -> IdCollection from item -> IdCollection to item -> ( IdCollection from item, IdCollection to item )
move id from to =
    let
        ( maybeEntry, newFrom ) =
            removeAndGet id from

        newTo =
            case maybeEntry of
                Just entry ->
                    append entry to

                Nothing ->
                    to
    in
    ( newFrom, newTo )
