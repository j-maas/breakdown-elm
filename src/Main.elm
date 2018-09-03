module Main exposing (addTask, main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global exposing (global, selector)
import Html
import Html.Styled exposing (Html, button, div, form, input, label, li, main_, ol, section, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import List.Extra as List
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
    , newTask : String
    , currentTasks : List String
    , accomplishedTasks : List String
    }


init flags url key =
    simply
        { key = key
        , newTask = ""
        , currentTasks = []
        , accomplishedTasks = []
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
    | AccomplishTask String
    | UnaccomplishTask String


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
            simply { model | newTask = action }

        AddNewTask ->
            let
                newTasks =
                    addTask model.newTask model.currentTasks
            in
            simply
                { model
                    | currentTasks = newTasks
                    , newTask = ""
                }

        AccomplishTask task ->
            simply
                { model
                    | currentTasks = List.remove task model.currentTasks
                    , accomplishedTasks = task :: model.accomplishedTasks
                }

        UnaccomplishTask task ->
            simply
                { model
                    | accomplishedTasks = List.remove task model.accomplishedTasks
                    , currentTasks = model.currentTasks ++ [ task ]
                }


addTask : String -> List String -> List String
addTask newTask currentTasks =
    let
        cleaned =
            String.trim newTask
    in
    if String.isEmpty cleaned then
        currentTasks

    else
        currentTasks ++ [ cleaned ]



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
            [ global
                [ selector "body"
                    [ displayFlex
                    , justifyContent center
                    , margin (em 1)
                    , fontFamily sansSerif
                    ]
                ]
            , main_ [ css [ minWidth (em 20) ] ]
                [ viewActionInput [] model.newTask
                , viewTaskList [ marginTop (em 1.5) ] <| List.map viewTask model.currentTasks
                , viewTaskList [ marginTop (em 1.5) ] <| List.map viewAccomplishedTask model.accomplishedTasks
                ]
            ]
    }


viewActionInput : List Style -> String -> Html Msg
viewActionInput styles currentAction =
    form [ onSubmit AddNewTask, css styles ]
        [ label []
            [ span [ css [ hide ] ] [ text "New task's action" ]
            , input
                [ type_ "text"
                , value currentAction
                , onInput UpdateNewTask
                , autofocus True
                , css [ boxSizing borderBox, width (pct 100) ]
                ]
                []
            ]
        , div
            [ css
                [ displayFlex
                , flexDirection row
                , justifyContent center
                ]
            ]
            [ label []
                [ span [ css [ hide ] ] [ text "Add new task" ]
                , input [ type_ "submit", value "✔️" ] []
                ]
            , label []
                [ span [ css [ hide ] ] [ text "Clear input" ]
                , input [ type_ "reset", value "❌", onClick (UpdateNewTask "") ] []
                ]
            ]
        ]


viewTaskList : List Style -> List (Html Msg) -> Html Msg
viewTaskList styles =
    ol [ css ([ listStyleType none, margin zero, padding zero, maxWidth (em 20) ] ++ styles) ]
        << List.map
            (\task ->
                li
                    [ css
                        [ hover [ backgroundColor (rgba 0 0 0 0.03) ]
                        , pseudoClass "not(:last-child)"
                            [ borderBottom3 (px 1) solid (rgba 0 0 0 0.1)
                            ]
                        ]
                    ]
                    [ task ]
            )


viewTask : String -> Html Msg
viewTask task =
    section
        [ css
            [ height (em 2)
            , displayFlex
            , alignItems center
            , padding (em 0.5)
            ]
        ]
        [ span
            [ css
                [ whiteSpace noWrap
                , overflow hidden
                , textOverflow ellipsis
                , flex (num 1)
                ]
            ]
            [ text task ]
        , iconButton (AccomplishTask task) "Mark as done" "✔️"
        ]


viewAccomplishedTask : String -> Html Msg
viewAccomplishedTask task =
    section
        [ css
            [ height (em 2)
            , displayFlex
            , alignItems center
            , padding (em 0.5)
            ]
        ]
        [ span
            [ css
                [ whiteSpace noWrap
                , overflow hidden
                , textOverflow ellipsis
                , textDecoration lineThrough
                , flex (num 1)
                ]
            ]
            [ text task ]
        , iconButton (UnaccomplishTask task) "Mark as to do" "🔄"
        ]


iconButton : Msg -> String -> String -> Html Msg
iconButton msg hint icon =
    button [ onClick msg ] [ span [ css [ hide ] ] [ text hint ], text icon ]


{-| Hides an element visually, but keeps it discoverable to assistive technologies.

See <https://www.w3.org/WAI/tutorials/forms/labels/#note-on-hiding-elements> for further information.

-}
hide : Style
hide =
    batch
        [ border zero
        , property "clip" "rect(0 0 0 0)"
        , height (px 1)
        , margin (px -1)
        , overflow hidden
        , padding zero
        , position absolute
        , whiteSpace noWrap
        , width (px 1)
        ]
