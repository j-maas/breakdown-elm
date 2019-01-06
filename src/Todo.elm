module Todo exposing (Todo, action, decoder, encode, fromAction, readAction, setAction)

import Json.Decode as Decode
import Json.Encode as Encode
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type Todo
    = Todo NonEmptyString


fromAction : NonEmptyString -> Todo
fromAction =
    Todo


action : Todo -> NonEmptyString
action (Todo act) =
    act


readAction : Todo -> String
readAction todo =
    action todo |> NonEmptyString.toString


setAction : NonEmptyString -> Todo -> Todo
setAction newAction _ =
    Todo newAction


encode : Todo -> Encode.Value
encode =
    readAction >> Encode.string


decoder : Decode.Decoder Todo
decoder =
    Decode.string
        |> Decode.andThen
            (\rawAction ->
                case NonEmptyString.fromString rawAction of
                    Just act ->
                        Decode.succeed (from act)

                    Nothing ->
                        Decode.fail "Invalid action."
            )
