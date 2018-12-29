module Breakdown exposing
    ( Current
    , Done
    , Id
    , Model
    , currentTodos
    , doneTodos
    , empty
    , insert
    , moveToCurrent
    , moveToDone
    , put
    )

import Array exposing (Array)
import Array.Extra as Array
import Todo exposing (Todo)


type Model
    = Model
        { current : Array Todo
        , done : Array Todo
        }


empty : Model
empty =
    Model
        { current = Array.empty
        , done = Array.empty
        }


type Id collection
    = Id Int


type Current
    = Current


type Done
    = Done


insert : Todo -> Model -> ( Id Current, Model )
insert todo (Model breakdown) =
    let
        ( newIndex, newCurrent ) =
            pushWithIndex todo breakdown.current
    in
    ( Id newIndex, Model { breakdown | current = newCurrent } )


{-| Similar to `insert`, but ignores `Id` of inserted `Todo`.
-}
put : Todo -> Model -> Model
put todo model =
    insert todo model |> Tuple.second


currentTodos : Model -> List ( Id Current, Todo )
currentTodos (Model breakdown) =
    Array.toIndexedList breakdown.current
        |> List.map (Tuple.mapFirst Id)


doneTodos : Model -> List ( Id Done, Todo )
doneTodos (Model breakdown) =
    Array.toIndexedList breakdown.done
        |> List.map (Tuple.mapFirst Id)


moveToDone : Id Current -> Model -> Maybe ( Id Done, Model )
moveToDone (Id index) (Model breakdown) =
    Array.get index breakdown.current
        |> Maybe.map
            (\todo ->
                let
                    newCurrent =
                        Array.removeAt index breakdown.current

                    ( newId, newDone ) =
                        pushWithIndex todo breakdown.done
                in
                ( Id newId, Model { breakdown | current = newCurrent, done = newDone } )
            )


moveToCurrent : Id Done -> Model -> Maybe ( Id Current, Model )
moveToCurrent (Id index) (Model breakdown) =
    Array.get index breakdown.done
        |> Maybe.map
            (\todo ->
                let
                    newDone =
                        Array.removeAt index breakdown.done

                    ( newId, newCurrent ) =
                        pushWithIndex todo breakdown.current
                in
                ( Id newId, Model { breakdown | current = newCurrent, done = newDone } )
            )



-- HELPERS


pushWithIndex : a -> Array a -> ( Int, Array a )
pushWithIndex item array =
    let
        index =
            Array.length array
    in
    ( index, Array.push item array )
