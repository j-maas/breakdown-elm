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
    , editing : Maybe Int
    }


init flags url key =
    simply
        { key = key
        , newTask = ""
        , currentTasks = []
        , accomplishedTasks = []
        , editing = Nothing
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
    | AccomplishTask Int
    | UnaccomplishTask Int
    | Edit Int
    | CloseEdit


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

        AccomplishTask index ->
            let
                task =
                    List.getAt index model.currentTasks

                newCurrentTasks =
                    List.removeAt index model.currentTasks

                newAccomplishedTasks =
                    case task of
                        Nothing ->
                            model.accomplishedTasks

                        Just aTask ->
                            aTask :: model.accomplishedTasks
            in
            simply
                { model
                    | currentTasks = newCurrentTasks
                    , accomplishedTasks = newAccomplishedTasks
                }

        UnaccomplishTask index ->
            let
                task =
                    List.getAt index model.accomplishedTasks

                newAccomplishedTasks =
                    List.removeAt index model.accomplishedTasks

                newCurrentTasks =
                    case task of
                        Nothing ->
                            model.currentTasks

                        Just aTask ->
                            model.currentTasks ++ [ aTask ]
            in
            simply
                { model
                    | accomplishedTasks = newAccomplishedTasks
                    , currentTasks = newCurrentTasks
                }

        Edit index ->
            simply { model | editing = Just index }

        CloseEdit ->
            simply { model | editing = Nothing }


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
                [ viewActionInput model.newTask
                , viewTaskList [ marginTop (em 1.5) ] <| List.indexedMap viewTask model.currentTasks
                , viewTaskList [ marginTop (em 1.5) ] <| List.indexedMap viewAccomplishedTask model.accomplishedTasks
                ]
            ]
    }


viewActionInput : String -> Html Msg
viewActionInput currentAction =
    form [ onSubmit AddNewTask ]
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
                , input [ css [ buttonStyle ], type_ "submit", value "➕" ] []
                ]
            , label []
                [ span [ css [ hide ] ] [ text "Clear input" ]
                , input [ css [ buttonStyle ], type_ "reset", value "❌", onClick (UpdateNewTask "") ] []
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


viewTask : Int -> String -> Html Msg
viewTask index task =
    viewTaskBase []
        (iconButton (AccomplishTask index) "Mark as done" "✔️")
        task


viewTaskBase : List Style -> Html Msg -> String -> Html Msg
viewTaskBase textStyles actionButton task =
    div
        [ css
            [ height (em 2)
            , displayFlex
            , alignItems center
            , padding (em 0.5)
            ]
        ]
        [ span
            [ css
                ([ whiteSpace noWrap
                 , overflow hidden
                 , textOverflow ellipsis
                 , flex (num 1)
                 ]
                    ++ textStyles
                )
            ]
            [ text task ]
        , actionButton
        ]


viewAccomplishedTask : Int -> String -> Html Msg
viewAccomplishedTask index task =
    viewTaskBase
        [ textDecoration lineThrough
        , opacity (num 0.6)
        ]
        (iconButton (UnaccomplishTask index) "Mark as to do" "🔄")
        task


iconButton : Msg -> String -> String -> Html Msg
iconButton msg hint icon =
    button [ onClick msg, css [ buttonStyle ] ] [ span [ css [ hide ] ] [ text hint ], text icon ]


buttonStyle : Style
buttonStyle =
    let
        size =
            em 2
    in
    batch
        [ border zero
        , padding zero
        , width size
        , height size
        , textAlign center
        , backgroundColor (rgba 0 0 0 0.1)
        , hover [ backgroundColor (rgba 0 0 0 0.07) ]
        , active [ boxShadow5 inset (em 0.1) (em 0.1) (em 0.2) (rgba 0 0 0 0.1) ]
        , margin (em 0.1)
        ]


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
