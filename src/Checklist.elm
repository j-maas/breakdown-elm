module Checklist exposing
    ( Checklist
    , Id
    , empty
    , fromItems
    , get
    , insertCurrent
    , insertDone
    , mapCurrent
    , mapDone
    , moveToCurrent
    , moveToDone
    , remove
    , update
    )

import List.Extra as List


type Checklist a
    = Checklist (Items a)


type alias Items a =
    { current : List a, done : List a }


type Id
    = Id Selector Int


type Selector
    = Current
    | Done


getList : Selector -> Items a -> List a
getList selector items =
    case selector of
        Current ->
            items.current

        Done ->
            items.done



-- BUILD


empty : Checklist a
empty =
    fromItems { current = [], done = [] }


fromItems : { current : List a, done : List a } -> Checklist a
fromItems items =
    Checklist items


insertCurrent : a -> Checklist a -> ( Id, Checklist a )
insertCurrent item (Checklist existing) =
    let
        newItems =
            existing.current ++ [ item ]

        index =
            List.length existing.current
    in
    ( Id Current index, Checklist { current = newItems, done = existing.done } )


insertDone : a -> Checklist a -> ( Id, Checklist a )
insertDone item (Checklist existing) =
    let
        newItems =
            existing.done ++ [ item ]

        index =
            List.length existing.done
    in
    ( Id Done index, Checklist { current = existing.current, done = newItems } )



-- READ


get : Id -> Checklist a -> Maybe a
get (Id selector index) (Checklist items) =
    getList selector items
        |> List.getAt index


mapCurrent : (Id -> a -> b) -> Checklist a -> List b
mapCurrent mapping checklist =
    map mapping Current checklist


mapDone : (Id -> a -> b) -> Checklist a -> List b
mapDone mapping checklist =
    map mapping Done checklist


map : (Id -> a -> b) -> Selector -> Checklist a -> List b
map mapping selector (Checklist items) =
    let
        list =
            getList selector items

        idMapping index item =
            mapping (Id selector index) item
    in
    List.indexedMap idMapping list



-- MODIFY


update : (a -> a) -> Id -> Checklist a -> Maybe (Checklist a)
update mapping id checklist =
    change (\a -> Just (mapping a)) id checklist


remove : Id -> Checklist a -> Maybe (Checklist a)
remove id checklist =
    change (\a -> Nothing) id checklist


moveToCurrent : Id -> Checklist a -> Maybe (Checklist a)
moveToCurrent (Id selector index) (Checklist items) =
    case selector of
        Current ->
            Just (Checklist items)

        Done ->
            let
                maybeItem =
                    List.getAt index items.done

                newDone =
                    List.removeAt index items.done
            in
            Maybe.map
                (\item ->
                    let
                        newCurrent =
                            items.current ++ [ item ]
                    in
                    Checklist { current = newCurrent, done = newDone }
                )
                maybeItem


moveToDone : Id -> Checklist a -> Maybe (Checklist a)
moveToDone (Id selector index) (Checklist items) =
    case selector of
        Done ->
            Just (Checklist items)

        Current ->
            let
                maybeItem =
                    List.getAt index items.current

                newCurrent =
                    List.removeAt index items.current
            in
            Maybe.map
                (\item ->
                    let
                        newDone =
                            items.done ++ [ item ]
                    in
                    Checklist { current = newCurrent, done = newDone }
                )
                maybeItem


change : (a -> Maybe a) -> Id -> Checklist a -> Maybe (Checklist a)
change mapping (Id selector index) (Checklist items) =
    if index < 0 then
        Nothing

    else
        let
            list =
                getList selector items

            head =
                List.take index list

            tail =
                List.drop index list

            wrap aList =
                case selector of
                    Current ->
                        Checklist
                            { current = aList
                            , done = items.done
                            }

                    Done ->
                        Checklist
                            { current = items.current
                            , done = aList
                            }
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
                Just (wrap <| head ++ changed ++ rest)

            _ ->
                Nothing
