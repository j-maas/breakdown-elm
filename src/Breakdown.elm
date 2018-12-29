module Breakdown exposing
    ( Current
    , Done
    , Id
    , currentTodos
    , doneTodos
    , empty
    , insert
    , moveToCurrent
    , moveToDone
    )

import Array exposing (Array)
import Array.Extra as Array
import Todo exposing (Todo)


type Breakdown
    = Breakdown
        { current : Array Todo
        , done : Array Todo
        }


empty : Breakdown
empty =
    Breakdown
        { current = Array.empty
        , done = Array.empty
        }


type Id collection
    = Id Int


type Current
    = Current


type Done
    = Done


insert : Todo -> Breakdown -> ( Id Current, Breakdown )
insert todo (Breakdown breakdown) =
    let
        ( newIndex, newCurrent ) =
            pushWithIndex todo breakdown.current
    in
    ( Id newIndex, Breakdown { breakdown | current = newCurrent } )


currentTodos : Breakdown -> List ( Id Current, Todo )
currentTodos (Breakdown breakdown) =
    Array.toIndexedList breakdown.current
        |> List.map (Tuple.mapFirst Id)


doneTodos : Breakdown -> List ( Id Done, Todo )
doneTodos (Breakdown breakdown) =
    Array.toIndexedList breakdown.done
        |> List.map (Tuple.mapFirst Id)


moveToDone : Id Current -> Breakdown -> Maybe ( Id Done, Breakdown )
moveToDone (Id index) (Breakdown breakdown) =
    Array.get index breakdown.current
        |> Maybe.map
            (\todo ->
                let
                    newCurrent =
                        Array.removeAt index breakdown.current

                    ( newId, newDone ) =
                        pushWithIndex todo breakdown.done
                in
                ( Id newId, Breakdown { breakdown | current = newCurrent, done = newDone } )
            )


moveToCurrent : Id Done -> Breakdown -> Maybe ( Id Current, Breakdown )
moveToCurrent (Id index) (Breakdown breakdown) =
    Array.get index breakdown.done
        |> Maybe.map
            (\todo ->
                let
                    newDone =
                        Array.removeAt index breakdown.done

                    ( newId, newCurrent ) =
                        pushWithIndex todo breakdown.current
                in
                ( Id newId, Breakdown { breakdown | current = newCurrent, done = newDone } )
            )



-- HELPERS


pushWithIndex : a -> Array a -> ( Int, Array a )
pushWithIndex item array =
    let
        index =
            Array.length array
    in
    ( index, Array.push item array )
