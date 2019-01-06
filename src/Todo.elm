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

import Json.Decode as Decode
import Json.Encode as Encode
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type Todo
    = Todo
        { action : NonEmptyString
        , subtodos : List Todo
        }



-- BUILD


from : NonEmptyString -> List Todo -> Todo
from act subs =
    Todo
        { action = act
        , subtodos = subs
        }


fromAction : NonEmptyString -> Todo
fromAction act =
    from act []



-- READ


action : Todo -> NonEmptyString
action (Todo todo) =
    todo.action


readAction : Todo -> String
readAction todo =
    action todo |> NonEmptyString.toString


subtodos : Todo -> List Todo
subtodos (Todo todo) =
    todo.subtodos



-- MODIFY


setAction : NonEmptyString -> Todo -> Todo
setAction newAction (Todo todo) =
    Todo { todo | action = newAction }


setSubtodos : List Todo -> Todo -> Todo
setSubtodos newSubtodos (Todo todo) =
    Todo { todo | subtodos = newSubtodos }



-- JSON


actionField : String
actionField =
    "action"


subtodosField : String
subtodosField =
    "subtodos"


encode : Todo -> Encode.Value
encode todo =
    Encode.object
        [ ( actionField, readAction todo |> Encode.string )
        , ( subtodosField, Encode.list encode (subtodos todo) )
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
            Decode.list (Decode.lazy (\_ -> decoder))
    in
    Decode.map2
        from
        (Decode.field actionField decodeAction)
        (Decode.field subtodosField decodeSubtodos)
