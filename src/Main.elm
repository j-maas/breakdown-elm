module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Html exposing (Html)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = \_ -> NoOp
        }


init flags url key =
    simply { key = key }


simply : Model -> ( Model, Cmd Msg )
simply model =
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { key : Nav.Key
    }



-- UPDATE


type Msg
    = NoOp
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            simply model

        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Breakdown"
    , body =
        [ Element.layout [] <| Element.text "Welcome to Breakdown."
        ]
    }
