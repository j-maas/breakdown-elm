port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global exposing (global, selector)
import Html.Styled exposing (Attribute, Html, button, div, form, input, label, li, main_, ol, section, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, id, title, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Tasks
import Url exposing (Url)


main : Program Decode.Value Model Msg
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


type GlobalTaskId
    = CurrentId (Tasks.TaskId Current)
    | DoneId (Tasks.TaskId Done)


type Current
    = Current


type Done
    = Done


type alias Model =
    { key : Nav.Key
    , newTask : String
    , currentTasks : Tasks.Collection Current
    , doneTasks : Tasks.Collection Done
    , editing : Maybe Editing
    }


type alias Editing =
    { id : GlobalTaskId
    , info : EditInfo
    }


type alias EditInfo =
    { newRawAction : String, previousAction : Tasks.Action }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( actualCurrentTasks, actualDoneTasks ) =
            decodeFlags flags
    in
    simply
        { key = key
        , newTask = ""
        , currentTasks = actualCurrentTasks
        , doneTasks = actualDoneTasks
        , editing = Nothing
        }


decodeFlags : Decode.Value -> ( Tasks.Collection Current, Tasks.Collection Done )
decodeFlags flags =
    Decode.decodeValue
        (Decode.map2
            (let
                collectionFromList c rawActions =
                    List.filterMap Tasks.actionFromString rawActions
                        |> List.foldl (\action collection -> Tasks.addTask action collection) (Tasks.empty c)
             in
             \current done ->
                ( collectionFromList Current current
                , collectionFromList Done done
                )
            )
            (Decode.field "currentTasks" <| Decode.list Decode.string)
            (Decode.field "doneTasks" <| Decode.list Decode.string)
        )
        flags
        |> Result.withDefault ( Tasks.empty Current, Tasks.empty Done )


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
    | StartEdit GlobalTaskId
    | EditTask String
    | ApplyEdit
    | CancelEdit
    | DeleteTask GlobalTaskId
    | BackgroundClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
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
                -- We might already be editing.
                updatedModel =
                    applyEdit model

                editing =
                    let
                        taskToEditing task =
                            Maybe.map
                                (\t ->
                                    { id = id
                                    , info =
                                        { newRawAction = Tasks.readAction t
                                        , previousAction = Tasks.getAction t
                                        }
                                    }
                                )
                                task
                    in
                    case id of
                        CurrentId currentId ->
                            Tasks.toList model.currentTasks
                                |> List.find (Tasks.getId >> (==) currentId)
                                |> taskToEditing

                        DoneId doneId ->
                            Tasks.toList model.doneTasks
                                |> List.find (Tasks.getId >> (==) doneId)
                                |> taskToEditing
            in
            simply { updatedModel | editing = editing }

        ApplyEdit ->
            simply <| applyEdit model

        CancelEdit ->
            let
                updatedModel =
                    Maybe.map
                        (\{ id, info } ->
                            case id of
                                CurrentId currentId ->
                                    { model
                                        | currentTasks =
                                            Tasks.editTask currentId info.previousAction model.currentTasks
                                    }

                                DoneId doneId ->
                                    { model
                                        | doneTasks =
                                            Tasks.editTask doneId info.previousAction model.doneTasks
                                    }
                        )
                        model.editing
                        |> Maybe.withDefault model
            in
            simply
                { updatedModel
                    | editing = Nothing
                }

        EditTask newRawAction ->
            let
                updatedModel =
                    case model.editing of
                        Just ({ id, info } as currentEditing) ->
                            case id of
                                CurrentId currentId ->
                                    let
                                        updatedCurrentTasks =
                                            case Tasks.actionFromString newRawAction of
                                                Just newAction ->
                                                    Tasks.editTask currentId newAction model.currentTasks

                                                Nothing ->
                                                    Tasks.editTask currentId info.previousAction model.currentTasks
                                    in
                                    { model | currentTasks = updatedCurrentTasks, editing = Just { currentEditing | info = { info | newRawAction = newRawAction } } }

                                DoneId doneId ->
                                    let
                                        updatedDoneTasks =
                                            case Tasks.actionFromString newRawAction of
                                                Just newAction ->
                                                    Tasks.editTask doneId newAction model.doneTasks

                                                Nothing ->
                                                    Tasks.editTask doneId info.previousAction model.doneTasks
                                    in
                                    { model | doneTasks = updatedDoneTasks, editing = Just { currentEditing | info = { info | newRawAction = newRawAction } } }

                        Nothing ->
                            model
            in
            simply updatedModel

        DeleteTask id ->
            let
                updatedModel =
                    case id of
                        CurrentId currentId ->
                            { model | currentTasks = Tasks.removeTask currentId model.currentTasks }

                        DoneId doneId ->
                            { model | doneTasks = Tasks.removeTask doneId model.doneTasks }
            in
            simply updatedModel

        BackgroundClicked ->
            simply <| { model | editing = Nothing }
    )
        |> (\( newModel, newMsg ) -> ( newModel, Cmd.batch [ newMsg, save newModel ] ))


save : Model -> Cmd msg
save model =
    saveRaw <|
        Encode.object
            [ ( "currentTasks", Encode.list encodeTask <| Tasks.toList model.currentTasks )
            , ( "doneTasks", Encode.list encodeTask <| Tasks.toList model.doneTasks )
            ]


encodeTask : Tasks.Task a -> Encode.Value
encodeTask =
    Tasks.readAction >> Encode.string


port saveRaw : Encode.Value -> Cmd msg


addTask : String -> Tasks.Collection c -> Tasks.Collection c
addTask rawAction currentTasks =
    case Tasks.actionFromString rawAction of
        Just action ->
            Tasks.addTask action currentTasks

        Nothing ->
            currentTasks


type alias EditModel a =
    { a
        | currentTasks : Tasks.Collection Current
        , doneTasks : Tasks.Collection Done
        , editing : Maybe Editing
    }


applyEdit : EditModel a -> EditModel a
applyEdit ({ editing } as currentModel) =
    case editing of
        Just { id, info } ->
            let
                updatedModel =
                    case id of
                        CurrentId currentId ->
                            { currentModel
                                | currentTasks = editTask currentId info.newRawAction currentModel.currentTasks
                            }

                        DoneId doneId ->
                            { currentModel
                                | doneTasks = editTask doneId info.newRawAction currentModel.doneTasks
                            }
            in
            { updatedModel | editing = Nothing }

        Nothing ->
            currentModel


editTask : Tasks.TaskId c -> String -> Tasks.Collection c -> Tasks.Collection c
editTask id editedAction currentCollection =
    case Tasks.actionFromString editedAction of
        Just newAction ->
            Tasks.editTask id newAction currentCollection

        Nothing ->
            currentCollection



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
                , selector "*" [ fontSize (pct 100) ]
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
                , onButtonClick BackgroundClicked
                ]
                [ let
                    ( currentEditing, doneEditing ) =
                        case model.editing of
                            Just { id, info } ->
                                case id of
                                    CurrentId currentId ->
                                        ( Just { id = currentId, info = info }, Nothing )

                                    DoneId doneId ->
                                        ( Nothing, Just { id = doneId, info = info } )

                            Nothing ->
                                ( Nothing, Nothing )
                  in
                  main_
                    [ css
                        [ width (pct 100)
                        , maxWidth (em 25)
                        ]
                    ]
                    [ viewActionInput model.newTask
                    , viewCurrentTaskList currentEditing model.currentTasks
                    , viewDoneTaskList doneEditing model.doneTasks
                    ]
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
                , css [ actionInputStyle ]
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
            [ iconButtonInput "submit" NoOp "Add new task" "➕"
            , iconButtonInput "reset" (UpdateNewTask "") "Clear input" "❌"
            ]
        ]


type alias EditingCollection c =
    { id : Tasks.TaskId c
    , info : EditInfo
    }


viewCurrentTaskList : Maybe (EditingCollection Current) -> Tasks.Collection Current -> Html Msg
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

                        Just { id, info } ->
                            if Tasks.getId task == id then
                                viewEditTask info.newRawAction info.previousAction task

                            else
                                viewTask task
                    ]
            )
        << Tasks.toList


viewTask : Tasks.Task Current -> Html Msg
viewTask task =
    viewTaskBase
        (onButtonClick (StartEdit <| CurrentId <| Tasks.getId task))
        (viewAction noStyle (Tasks.readAction task))
        (iconButton (DoTask <| Tasks.getId task) "Mark as done" "✔️")


viewEditTask : String -> Tasks.Action -> Tasks.Task Current -> Html Msg
viewEditTask editedAction previousAction task =
    viewTaskBase
        (onButtonClick ApplyEdit)
        (viewEditAction editedAction previousAction task CurrentId)
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


viewEditAction : String -> Tasks.Action -> Tasks.Task a -> (Tasks.TaskId a -> GlobalTaskId) -> Html Msg
viewEditAction editedAction previousAction task toGlobalId =
    form
        [ css [ flex (num 1) ]
        , onSubmit ApplyEdit
        ]
        [ let
            isEmpty =
                Tasks.actionFromString editedAction == Nothing
          in
          label []
            [ span [ css [ hide ] ] [ text "Action" ]
            , input
                [ type_ "text"
                , value editedAction
                , onInput EditTask
                , stopPropagation
                , css [ actionInputStyle ]
                ]
                []
            , span
                [ css
                    ([ errorMessageStyle ]
                        ++ (if isEmpty then
                                []

                            else
                                [ hide ]
                           )
                    )
                ]
                [ text "The action cannot be empty." ]
            ]
        , div
            [ css [ displayFlex, justifyContent center ] ]
            [ iconButtonInputDisable (editedAction == Tasks.stringFromAction previousAction)
                "reset"
                CancelEdit
                "Undo changes"
                "️↩️"
            , iconButtonInput "button" (DeleteTask <| toGlobalId <| Tasks.getId task) "Delete task" "🗑️"
            ]
        ]


viewDoneTaskList : Maybe (EditingCollection Done) -> Tasks.Collection Done -> Html Msg
viewDoneTaskList editing =
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
                            viewDoneTask task

                        Just { id, info } ->
                            if Tasks.getId task == id then
                                viewEditDoneTask info.newRawAction info.previousAction task

                            else
                                viewDoneTask task
                    ]
            )
        << Tasks.toList


viewDoneTask : Tasks.Task Done -> Html Msg
viewDoneTask task =
    viewTaskBase
        (onButtonClick (StartEdit <| DoneId <| Tasks.getId task))
        (viewAction
            (batch
                [ textDecoration lineThrough
                , opacity (num 0.6)
                ]
            )
            (Tasks.readAction task)
        )
        (iconButton (UndoTask <| Tasks.getId task) "Mark as to do" "🔄")


viewEditDoneTask : String -> Tasks.Action -> Tasks.Task Done -> Html Msg
viewEditDoneTask editedAction previousAction task =
    viewTaskBase
        (onButtonClick ApplyEdit)
        (viewEditAction editedAction previousAction task DoneId)
        (iconButton (UndoTask <| Tasks.getId task) "Mark as to do" "🔄")



-- ELEMENTS


iconButton : Msg -> String -> String -> Html Msg
iconButton msg hint icon =
    button [ onButtonClick msg, css [ buttonStyle ], title hint ] [ span [ css [ hide ] ] [ text hint ], text icon ]


iconButtonInput =
    iconButtonInputDisable False


iconButtonInputDisable : Bool -> String -> Msg -> String -> String -> Html Msg
iconButtonInputDisable isDisabled inputType msg hint icon =
    label []
        [ span [ css [ hide ] ] [ text hint ]
        , input
            [ css [ buttonStyle ]
            , title hint
            , Html.Styled.Attributes.disabled isDisabled
            , type_ inputType
            , value icon
            , onButtonClick msg
            ]
            []
        ]



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
        ]


actionInputStyle : Style
actionInputStyle =
    batch
        [ boxSizing borderBox
        , width (pct 100)
        , padding (em 0.75)
        ]


errorMessageStyle : Style
errorMessageStyle =
    batch
        [ color (rgb 255 0 0) ]


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
        , fontSize (pct 125)
        , textAlign center
        , backgroundColor (rgba 0 0 0 0.1)
        , hover [ backgroundColor (rgba 0 0 0 0.07) ]
        , active [ boxShadow5 inset (em 0.1) (em 0.1) (em 0.2) (rgba 0 0 0 0.1) ]
        , margin (em 0.1)
        , pseudoClass "disabled"
            [ color (rgb 0 0 0)
            , opacity (num 0.3)
            ]
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
