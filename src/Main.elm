module Main exposing (addTask, main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html
import Html.Styled exposing (Html, div, form, input, li, ol, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
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
        List.map toUnstyled
            [ viewAppContainer
                [ viewActionInput model.newTaskAction
                , viewTaskList model.tasks
                ]
            ]
    }


viewAppContainer : List (Html Msg) -> Html Msg
viewAppContainer content =
    div [ css [ displayFlex, justifyContent center ] ]
        [ div [ css [ minWidth (em 20) ] ] content
        ]


viewActionInput : String -> Html Msg
viewActionInput currentAction =
    form [ onSubmit AddNewTask ]
        [ input
            [ type_ "text"
            , value currentAction
            , onInput UpdateNewTask
            , autofocus True
            , css [ boxSizing borderBox, width (pct 100) ]
            ]
            []
        , div
            [ css
                [ displayFlex
                , flexDirection row
                , justifyContent center
                ]
            ]
            [ input [ type_ "submit", value "✔️" ] []
            , input [ type_ "reset", value "❌", onClick (UpdateNewTask "") ] []
            ]
        ]


viewTaskList : List String -> Html Msg
viewTaskList =
    ol [] << List.map (\task -> li [] <| [ text task ])
