module Breakdown exposing (currentTodos, doneTodos, empty, insert, moveToCurrent, moveToDone)

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


insert : Todo -> Breakdown -> Breakdown
insert todo (Breakdown breakdown) =
    Breakdown { breakdown | current = Array.push todo breakdown.current }


currentTodos : Breakdown -> Array Todo
currentTodos (Breakdown breakdown) =
    breakdown.current


doneTodos : Breakdown -> Array Todo
doneTodos (Breakdown breakdown) =
    breakdown.done


moveToDone : Int -> Breakdown -> Maybe Breakdown
moveToDone index (Breakdown breakdown) =
    Array.get index breakdown.current
        |> Maybe.map
            (\todo ->
                let
                    newCurrent =
                        Array.removeAt index breakdown.current

                    newDone =
                        Array.push todo breakdown.done
                in
                Breakdown { breakdown | current = newCurrent, done = newDone }
            )


moveToCurrent : Int -> Breakdown -> Maybe Breakdown
moveToCurrent index (Breakdown breakdown) =
    Array.get index breakdown.done
        |> Maybe.map
            (\todo ->
                let
                    newDone =
                        Array.removeAt index breakdown.done

                    newCurrent =
                        Array.push todo breakdown.current
                in
                Breakdown { breakdown | current = newCurrent, done = newDone }
            )
