module Todo exposing
    ( Todo
    , action
    , decoder
    , encode
    , from
    , fromAction
    , readAction
    , setAction
    , setSubtodos
    , subtodos
    )

import IdList
import Json.Decode as Decode
import Json.Encode as Encode
import SelectCollection exposing (SelectCollection)
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type alias TodoCollection =
    SelectCollection Todo


type Todo
    = Todo
        { action : NonEmptyString
        , subtodos : TodoCollection
        }



-- BUILD


from : NonEmptyString -> TodoCollection -> Todo
from act subs =
    Todo
        { action = act
        , subtodos = subs
        }


fromAction : NonEmptyString -> Todo
fromAction act =
    from act SelectCollection.empty



-- READ


action : Todo -> NonEmptyString
action (Todo todo) =
    todo.action


readAction : Todo -> String
readAction todo =
    action todo |> NonEmptyString.toString


subtodos : Todo -> TodoCollection
subtodos (Todo todo) =
    todo.subtodos



-- MODIFY


setAction : NonEmptyString -> Todo -> Todo
setAction newAction (Todo todo) =
    Todo { todo | action = newAction }


setSubtodos : TodoCollection -> Todo -> Todo
setSubtodos newSubtodos (Todo todo) =
    Todo { todo | subtodos = newSubtodos }



-- JSON


actionField : String
actionField =
    "action"


subtodosField : String
subtodosField =
    "subtodos"


currentField : String
currentField =
    "currentTodos"


doneField : String
doneField =
    "doneTodos"


encode : Todo -> Encode.Value
encode todo =
    let
        currentSubtodos =
            SelectCollection.mapToList SelectCollection.Current (\id t -> t) (subtodos todo)

        doneSubtodos =
            SelectCollection.mapToList SelectCollection.Done (\id t -> t) (subtodos todo)
    in
    Encode.object
        [ ( actionField, readAction todo |> Encode.string )
        , ( subtodosField
          , Encode.object
                [ ( currentField, Encode.list encode currentSubtodos )
                , ( doneField, Encode.list encode doneSubtodos )
                ]
          )
        ]


decoder : Decode.Decoder Todo
decoder =
    let
        decodeAction =
            Decode.string
                |> Decode.andThen
                    (\rawAction ->
                        case NonEmptyString.fromString rawAction of
                            Just act ->
                                Decode.succeed act

                            Nothing ->
                                Decode.fail "Invalid action."
                    )

        decodeSubtodos =
            Decode.map2
                (\current done -> SelectCollection.init { current = current, done = done })
                (Decode.field currentField
                    (Decode.list (Decode.lazy (\_ -> decoder)))
                )
                (Decode.field doneField
                    (Decode.list
                        (Decode.lazy (\_ -> decoder))
                    )
                )
    in
    Decode.map2
        from
        (Decode.field actionField decodeAction)
        (Decode.field subtodosField decodeSubtodos)
