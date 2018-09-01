module Main exposing (addTask, main)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
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



-- MODEL


type alias Model =
    { key : Nav.Key
    , newTaskAction : String
    , tasks : List String
    }


init flags url key =
    simply
        { key = key
        , newTaskAction = ""
        , tasks = []
        }


simply : Model -> ( Model, Cmd Msg )
simply model =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | UrlRequest Browser.UrlRequest
    | UpdateNewTask String
    | AddNewTask


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

        UpdateNewTask action ->
            simply { model | newTaskAction = action }

        AddNewTask ->
            let
                newTasks =
                    addTask model.newTaskAction model.tasks
            in
            simply
                { model
                    | tasks = newTasks
                    , newTaskAction = ""
                }


addTask : String -> List String -> List String
addTask newTask tasks =
    let
        cleaned =
            String.trim newTask
    in
    if String.isEmpty cleaned then
        tasks

    else
        tasks ++ [ cleaned ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Breakdown"
    , body =
        [ Element.layout [ padding 18 ] <|
            column [ centerX, width (shrink |> minimum 400) ]
                [ viewActionInput model.newTaskAction
                , viewTaskList model.tasks
                ]
        ]
    }


viewActionInput : String -> Element Msg
viewActionInput currentAction =
    row [ spacing 8, width fill ]
        [ Input.text []
            { label = Input.labelAbove [] <| text "Action"
            , onChange = UpdateNewTask
            , text = currentAction
            , placeholder = Nothing
            }
        , viewButtonWithStyle [ alignBottom ] (Just AddNewTask) "+"
        ]


viewButtonWithStyle : List (Attribute Msg) -> Maybe Msg -> String -> Element Msg
viewButtonWithStyle style msg label =
    Input.button (buttonStyle ++ style)
        { onPress = msg
        , label = paragraph [ centerX, centerY, width shrink ] [ text label ]
        }


buttonStyle =
    [ Background.color <| rgb 0.8 0.8 0.8
    , width <| px 40
    , height <| px 40
    , mouseOver buttonHoverStyle
    , focused buttonHoverStyle
    ]


buttonHoverStyle =
    [ Border.glow (rgba 0 0 0 0.2) 1 ]


viewTaskList : List String -> Element Msg
viewTaskList tasks =
    html <| Html.ol [] <| List.map (\task -> Html.li [] <| [ Html.text task ]) tasks
