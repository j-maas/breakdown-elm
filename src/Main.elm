module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global exposing (global, selector)
import Html.Styled exposing (Attribute, Html, button, div, form, input, label, li, main_, ol, section, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, id, title, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import List.Extra as List
import Tasks
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


type Current
    = Current


type Done
    = Done


type alias Model =
    { key : Nav.Key
    , newTask : String
    , currentTasks : Tasks.Collection Current
    , doneTasks : Tasks.Collection Done
    , editing : Maybe ( Tasks.TaskId Current, String )
    }


init flags url key =
    simply
        { key = key
        , newTask = ""
        , currentTasks = Tasks.empty Current
        , doneTasks = Tasks.empty Done
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
    | DoTask (Tasks.TaskId Current)
    | UndoTask (Tasks.TaskId Done)
    | StartEdit (Tasks.TaskId Current)
    | Edit (Tasks.TaskId Current) String
    | StopEdit
    | CancelEdit
    | DeleteTask (Tasks.TaskId Current)
    | BackgroundClicked


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

        DoTask id ->
            let
                ( newCurrentTasks, newDoneTasks ) =
                    Tasks.moveTask id model.currentTasks model.doneTasks
            in
            simply
                { model
                    | currentTasks = newCurrentTasks
                    , doneTasks = newDoneTasks
                    , editing = Nothing
                }

        UndoTask id ->
            let
                ( newDoneTasks, newCurrentTasks ) =
                    Tasks.moveTask id model.doneTasks model.currentTasks
            in
            simply
                { model
                    | doneTasks = newDoneTasks
                    , currentTasks = newCurrentTasks
                }

        StartEdit id ->
            let
                taskAction =
                    case
                        Tasks.toList model.currentTasks
                            |> List.find (Tasks.getId >> (==) id)
                    of
                        Just task ->
                            Tasks.readAction task

                        Nothing ->
                            ""
            in
            simply { model | editing = Just ( id, taskAction ) }

        StopEdit ->
            simply
                { model
                    | editing = Nothing
                }

        CancelEdit ->
            simply { model | editing = Nothing }

        Edit id newRawAction ->
            let
                maybeNewAction =
                    Tasks.actionFromString newRawAction

                updatedCurrentTasks =
                    case maybeNewAction of
                        Just newAction ->
                            Tasks.editTask id newAction model.currentTasks

                        Nothing ->
                            model.currentTasks
            in
            simply { model | currentTasks = updatedCurrentTasks }

        DeleteTask id ->
            let
                newCurrentTasks =
                    Tasks.removeTask id model.currentTasks
            in
            simply { model | currentTasks = newCurrentTasks }

        BackgroundClicked ->
            simply { model | editing = Nothing }


addTask : String -> Tasks.Collection c -> Tasks.Collection c
addTask rawAction currentTasks =
    case Tasks.actionFromString rawAction of
        Just action ->
            Tasks.addTask action currentTasks

        Nothing ->
            currentTasks



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
                [ selector "html" [ height (pct 100) ]
                , selector "body"
                    [ margin zero
                    , height (pct 100)
                    ]
                ]
            , div
                [ css
                    [ boxSizing borderBox
                    , width (pct 100)
                    , height (pct 100)
                    , displayFlex
                    , justifyContent center
                    , padding (em 1)
                    , fontFamily sansSerif
                    ]
                , id "background"
                , onClickWithId "background" BackgroundClicked
                ]
                [ main_
                    [ css [ minWidth (em 20) ]
                    ]
                    [ viewActionInput model.newTask
                    , viewCurrentTaskList model.editing model.currentTasks
                    , viewDoneTaskList model.doneTasks
                    ]
                ]
            ]
    }


viewActionInput : String -> Html Msg
viewActionInput currentAction =
    form [ onSubmit AddNewTask, css [ flex (num 1) ] ]
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


viewCurrentTaskList : Maybe ( Tasks.TaskId Current, String ) -> Tasks.Collection Current -> Html Msg
viewCurrentTaskList editing =
    ol [ css [ taskListStyle ] ]
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
                    [ case editing of
                        Nothing ->
                            viewTask task

                        Just ( id, editAction ) ->
                            if Tasks.getId task == id then
                                viewEditTask task

                            else
                                viewTask task
                    ]
            )
        << Tasks.toList


viewTask : Tasks.Task Current -> Html Msg
viewTask task =
    viewTaskBase
        (onClick (StartEdit <| Tasks.getId task))
        (viewAction noStyle (Tasks.readAction task))
        (iconButton (DoTask <| Tasks.getId task) "Mark as done" "✔️")


viewEditTask : Tasks.Task Current -> Html Msg
viewEditTask task =
    viewTaskBase
        (onClick StopEdit)
        (viewEditAction (Tasks.getId task) (Tasks.readAction task))
        (iconButton (DoTask <| Tasks.getId task) "Mark as done" "✔️")


viewTaskBase : Attribute Msg -> Html Msg -> Html Msg -> Html Msg
viewTaskBase whenClicked action btn =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , padding (em 0.5)
            ]
        , whenClicked
        ]
        [ action
        , btn
        ]


viewAction : Style -> String -> Html Msg
viewAction customStyle action =
    span
        [ css
            [ whiteSpace noWrap
            , overflow hidden
            , textOverflow ellipsis
            , flex (num 1)
            , customStyle
            ]
        ]
        [ text action ]


viewEditAction : Tasks.TaskId Current -> String -> Html Msg
viewEditAction id currentAction =
    form
        [ css [ flex (num 1) ]
        , onSubmit StopEdit
        ]
        [ label []
            [ span [ css [ hide ] ] [ text "Action" ]
            , input
                [ type_ "text"
                , value currentAction
                , onInput (Edit id)
                , stopPropagation
                , css [ boxSizing borderBox, width (pct 100) ]
                ]
                []
            ]
        , div
            [ css [ displayFlex, justifyContent center ] ]
            [ label []
                [ span [ css [ hide ] ] [ text "Undo changes" ]
                , input [ css [ buttonStyle ], onButtonClick CancelEdit, type_ "reset", value "️↩️" ] []
                ]
            , label []
                [ span [ css [ hide ] ] [ text "Delete task" ]
                , input [ css [ buttonStyle ], onButtonClick (DeleteTask id), type_ "button", value "🗑️" ] []
                ]
            ]
        ]


viewDoneTaskList : Tasks.Collection Done -> Html Msg
viewDoneTaskList =
    ol [ css [ taskListStyle ] ]
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
                    [ viewDoneTask task
                    ]
            )
        << Tasks.toList


viewDoneTask : Tasks.Task Done -> Html Msg
viewDoneTask task =
    viewTaskBase
        (onClick NoOp)
        (viewAction
            (batch
                [ textDecoration lineThrough
                , opacity (num 0.6)
                ]
            )
            (Tasks.readAction task)
        )
        (iconButton (UndoTask <| Tasks.getId task) "Mark as to do" "🔄")



-- ELEMENTS


iconButton : Msg -> String -> String -> Html Msg
iconButton msg hint icon =
    button [ onButtonClick msg, css [ buttonStyle ], title hint ] [ span [ css [ hide ] ] [ text hint ], text icon ]



-- STYLES


{-| Shortcut for no style.
-}
noStyle : Style
noStyle =
    batch []


taskListStyle : Style
taskListStyle =
    batch
        [ listStyleType none
        , margin3 (em 1.5) zero zero
        , padding zero
        , maxWidth (em 20)
        ]


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



-- EVENTS


stopPropagation : Attribute Msg
stopPropagation =
    stopPropagationOn "click" (Decode.succeed ( NoOp, True ))


onButtonClick : Msg -> Attribute Msg
onButtonClick msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))


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
