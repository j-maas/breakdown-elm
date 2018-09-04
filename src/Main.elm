module Main exposing (addTask, main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global exposing (global, selector)
import Html.Styled exposing (Attribute, Html, button, div, form, input, label, li, main_, ol, section, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, id, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit)
import Json.Decode as Decode
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
    | StartEdit Int
    | Edit Int String
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
                    , editing = Nothing
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

        StartEdit index ->
            simply { model | editing = Just index }

        CloseEdit ->
            simply { model | editing = Nothing }

        Edit index newAction ->
            let
                newCurrentTasks =
                    List.setAt index newAction model.currentTasks
            in
            simply { model | currentTasks = newCurrentTasks }


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
            , main_
                [ css
                    [ minWidth (em 20) ]
                ]
                [ viewActionInput model.newTask
                , let
                    renderTask =
                        case model.editing of
                            Nothing ->
                                \index task -> viewTask index False task

                            Just editIndex ->
                                \index task -> viewTask index (editIndex == index) task
                  in
                  viewTaskList [ marginTop (em 1.5) ] <| List.indexedMap renderTask model.currentTasks
                , viewTaskList [ marginTop (em 1.5) ] <| List.indexedMap viewAccomplishedTask model.accomplishedTasks
                ]
            ]
    }


viewActionInput : String -> Html Msg
viewActionInput =
    viewActionInputBase UpdateNewTask AddNewTask (UpdateNewTask "")


viewActionInputBase : (String -> Msg) -> Msg -> Msg -> String -> Html Msg
viewActionInputBase updateAction add reset currentAction =
    form [ onSubmit add, css [ flex (num 1) ] ]
        [ label []
            [ span [ css [ hide ] ] [ text "New task's action" ]
            , input
                [ type_ "text"
                , value currentAction
                , onInput updateAction
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
                , input [ css [ buttonStyle ], type_ "reset", value "❌", onClick reset ] []
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


viewTask : Int -> Bool -> String -> Html Msg
viewTask index isEditing task =
    viewTaskBase
        index
        (if isEditing then
            onClickWithId (idForTask index) CloseEdit

         else
            onClick (StartEdit index)
        )
        (if isEditing then
            viewEditAction index task

         else
            viewAction [] task
        )
        (iconButton (AccomplishTask index) "Mark as done" "✔️")


idForTask : Int -> String
idForTask index =
    "task-" ++ String.fromInt index


viewAccomplishedTask : Int -> String -> Html Msg
viewAccomplishedTask index task =
    viewTaskBase
        index
        (onClick NoOp)
        (viewAction
            [ textDecoration lineThrough
            , opacity (num 0.6)
            ]
            task
        )
        (iconButton (UnaccomplishTask index) "Mark as to do" "🔄")


viewTaskBase : Int -> Attribute Msg -> Html Msg -> Html Msg -> Html Msg
viewTaskBase index whenClicked action btn =
    div
        [ id (idForTask index)
        , css
            [ height (em 2)
            , displayFlex
            , alignItems center
            , padding (em 0.5)
            ]
        , whenClicked
        ]
        [ action
        , btn
        ]


viewAction : List Style -> String -> Html Msg
viewAction textStyles action =
    span
        [ css
            ([ whiteSpace noWrap
             , overflow hidden
             , textOverflow ellipsis
             , flex (num 1)
             ]
                ++ textStyles
            )
        ]
        [ text action ]


viewEditAction : Int -> String -> Html Msg
viewEditAction index =
    viewActionInputBase (Edit index) CloseEdit (Edit index "")


iconButton : Msg -> String -> String -> Html Msg
iconButton msg hint icon =
    button [ onClick msg, css [ buttonStyle ] ] [ span [ css [ hide ] ] [ text hint ], text icon ]


{-| Only fires for clicks exactly on the element.

See <https://javascript.info/bubbling-and-capturing#event-target> for further information.

-}
onClickWithId : String -> Msg -> Attribute Msg
onClickWithId targetId msg =
    on "click"
        (Decode.at [ "target", "id" ] Decode.string
            |> Decode.andThen
                (\actualId ->
                    if actualId == targetId then
                        Decode.succeed msg

                    else
                        Decode.fail <| "Element id was " ++ actualId ++ ", expected " ++ targetId ++ "."
                )
        )


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
