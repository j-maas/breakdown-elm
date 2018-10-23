port module Main exposing (GlobalTaskId(..), Model, Msg(..), initModel, main, update)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global exposing (global, selector)
import Html
import Html.Styled exposing (Attribute, Html, button, div, form, input, label, li, main_, ol, section, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, id, title, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Tasks
import Url exposing (Url)


main : Program Decode.Value AppModel AppMsg
main =
    Browser.application
        { init = init
        , view = appView
        , update = updateApp
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = \_ -> Msg NoOp
        }



-- MODEL


{-| Tags for the different types of task collections.
-}
type Current
    = Current


type Done
    = Done


{-| Wrapper to make list-locally unique TaskIds globally unique.
-}
type GlobalTaskId
    = CurrentId (Tasks.TaskId Current)
    | DoneId (Tasks.TaskId Done)


{-| The highest-level Model, containing the Nav.Key.

This extra layer is because Nav.Key prohibits testing, see <https://github.com/elm-explorations/test/issues/24>.

-}
type alias AppModel =
    { key : Nav.Key
    , model : Model
    }


type alias Model =
    { newTask : String
    , currentTasks : Tasks.Collection Current
    , doneTasks : Tasks.Collection Done
    }


init : Decode.Value -> Url -> Nav.Key -> ( AppModel, Cmd AppMsg )
init flags url key =
    let
        ( actualCurrentTasks, actualDoneTasks ) =
            decodeFlags flags
    in
    simply
        { key = key
        , model =
            { initModel
                | currentTasks = actualCurrentTasks
                , doneTasks = actualDoneTasks
            }
        }


initModel : Model
initModel =
    { newTask = ""
    , currentTasks = Tasks.empty Current
    , doneTasks = Tasks.empty Done
    }


decodeFlags : Decode.Value -> ( Tasks.Collection Current, Tasks.Collection Done )
decodeFlags flags =
    let
        decodeCollection field c =
            Result.withDefault (Tasks.empty c) <|
                Decode.decodeValue
                    (Decode.map (collectionFromStrings c)
                        (Decode.field field <| Decode.list Decode.string)
                    )
                    flags

        collectionFromStrings c rawActions =
            List.filterMap Tasks.actionFromString rawActions
                |> collectionFromActions c

        collectionFromActions c =
            List.foldl
                (\action collection ->
                    Tasks.appendTask action collection
                )
                (Tasks.empty c)
    in
    ( decodeCollection "currentTasks" Current
    , decodeCollection "doneTasks" Done
    )


{-| Convenience function for when no commands are sent.
-}
simply : model -> ( model, Cmd msg )
simply model =
    ( model, Cmd.none )



-- UPDATE


{-| Global Msg wrapper.

Due to test issues with Nav.Key (see <https://github.com/elm-explorations/test/issues/24>)
this will wrap the testable messages.

-}
type AppMsg
    = UrlRequest Browser.UrlRequest
    | Msg Msg


type Msg
    = NoOp
    | UpdateNewTask String
    | AddNewTask
    | DoTask (Tasks.TaskId Current)
    | UndoTask (Tasks.TaskId Done)
    | StartEdit GlobalTaskId
    | EditTask GlobalTaskId String
    | ApplyEdit GlobalTaskId
    | CancelEdit GlobalTaskId
    | DeleteTask GlobalTaskId
    | BackgroundClicked


{-| Wrapper to pass on and convert back from the update function to the global types.

This wrapping is due to issues with testing Nav.Key (see <https://github.com/elm-explorations/test/issues/24>).

-}
updateApp : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
updateApp appMsg appModel =
    case appMsg of
        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( appModel, Nav.pushUrl appModel.key (Url.toString url) )

                Browser.External href ->
                    ( appModel, Nav.load href )

        Msg msg ->
            update msg appModel.model
                |> Tuple.mapBoth
                    (\model -> { appModel | model = model })
                    (Cmd.map Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            simply model

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
                startEdit =
                    Tasks.startEdit >> Tasks.TaskInEdit
            in
            simply (updateTask id startEdit model)

        ApplyEdit id ->
            let
                applyEdit task =
                    updateEditTask
                        (\editInfo ->
                            case Tasks.applyEdit editInfo of
                                Just editedTask ->
                                    editedTask

                                Nothing ->
                                    task
                        )
                        task
            in
            simply (updateTask id applyEdit model)

        CancelEdit id ->
            let
                cancelEdit =
                    updateEditTask
                        Tasks.cancelEdit
            in
            simply (updateTask id cancelEdit model)

        EditTask id newRawAction ->
            let
                editTask =
                    updateEditTask
                        (Tasks.edit newRawAction >> Tasks.TaskInEdit)
            in
            simply (updateTask id editTask model)

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
            let
                applyEdit collection =
                    Tasks.map
                        (\task ->
                            updateEditTask
                                (\editInfo ->
                                    case Tasks.applyEdit editInfo of
                                        Just editedTask ->
                                            editedTask

                                        Nothing ->
                                            task
                                )
                                task
                        )
                        collection
            in
            simply
                { model
                    | currentTasks = applyEdit model.currentTasks
                    , doneTasks = applyEdit model.doneTasks
                }
    )
        |> (\( newModel, newMsg ) -> ( newModel, Cmd.batch [ newMsg, save newModel ] ))


{-| Command that saves the tasks collections persistently.
-}
save : Model -> Cmd msg
save model =
    Encode.object
        [ ( "currentTasks", Encode.list encodeTask <| Tasks.toList model.currentTasks )
        , ( "doneTasks", Encode.list encodeTask <| Tasks.toList model.doneTasks )
        ]
        |> saveRaw


encodeTask : Tasks.TaskEntry a -> Encode.Value
encodeTask =
    Tasks.readAction >> Encode.string


{-| Port for saving an encoded model to localStorage.
-}
port saveRaw : Encode.Value -> Cmd msg


{-| Adds a task to the collection, iff the `String` is a valid `Action`.
-}
addTask : String -> Tasks.Collection c -> Tasks.Collection c
addTask rawAction currentTasks =
    case Tasks.actionFromString rawAction of
        Just action ->
            Tasks.appendTask action currentTasks

        Nothing ->
            currentTasks


updateTask : GlobalTaskId -> (Tasks.Task -> Tasks.Task) -> Model -> Model
updateTask id updateFunction model =
    case id of
        CurrentId currentId ->
            { model | currentTasks = Tasks.updateTask currentId updateFunction model.currentTasks }

        DoneId doneId ->
            { model | doneTasks = Tasks.updateTask doneId updateFunction model.doneTasks }


updateEditTask : (Tasks.TaskEditInfo -> Tasks.Task) -> Tasks.Task -> Tasks.Task
updateEditTask updateFunction task =
    case task of
        Tasks.TaskInEdit editInfo ->
            updateFunction editInfo

        Tasks.Task _ ->
            task



-- SUBSCRIPTIONS


subscriptions : AppModel -> Sub AppMsg
subscriptions model =
    Sub.none



-- VIEW


{-| Wrapper to pass on and convert back from the view function to the global types.

This wrapping is due to issues with testing Nav.Key (see <https://github.com/elm-explorations/test/issues/24>).

-}
appView : AppModel -> Browser.Document AppMsg
appView appModel =
    view appModel.model
        |> mapDocument Msg


{-| Maps the msg type inside a Document.
-}
mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument f document =
    { title = document.title
    , body = List.map (Html.map f) document.body
    }


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
                [ main_
                    [ css
                        [ width (pct 100)
                        , maxWidth (em 25)
                        ]
                    ]
                    [ viewActionInput model.newTask
                    , viewCurrentTaskList model.currentTasks
                    , viewDoneTaskList model.doneTasks
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


viewCurrentTaskList : Tasks.Collection Current -> Html Msg
viewCurrentTaskList currentTasks =
    ol [ css [ taskListStyle ] ] <|
        List.map
            (\entry ->
                li
                    [ css
                        [ hover [ backgroundColor (rgba 0 0 0 0.03) ]
                        , pseudoClass "not(:last-child)"
                            [ borderBottom3 (px 1) solid (rgba 0 0 0 0.1)
                            ]
                        ]
                    ]
                    [ case entry.item of
                        Tasks.Task taskInfo ->
                            viewTask entry.id taskInfo

                        Tasks.TaskInEdit editingInfo ->
                            viewEditTask entry.id editingInfo
                    ]
            )
        <|
            Tasks.toList currentTasks


viewTask : Tasks.TaskId Current -> Tasks.TaskInfo -> Html Msg
viewTask id task =
    viewTaskBase
        (onButtonClick (StartEdit <| CurrentId id))
        (viewAction noStyle (task.action |> Tasks.stringFromAction))
        (iconButton (DoTask id) "Mark as done" "✔️")


viewEditTask : Tasks.TaskId Current -> Tasks.TaskEditInfo -> Html Msg
viewEditTask id editTask =
    viewTaskBase
        (onButtonClick (ApplyEdit <| CurrentId id))
        (viewEditAction id editTask CurrentId)
        (iconButton (DoTask id) "Mark as done" "✔️")


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


viewEditAction : Tasks.TaskId a -> Tasks.TaskEditInfo -> (Tasks.TaskId a -> GlobalTaskId) -> Html Msg
viewEditAction id editInfo toGlobalId =
    form
        [ css [ flex (num 1) ]
        , onSubmit (ApplyEdit <| toGlobalId id)
        ]
        [ let
            edited =
                Tasks.getEdited editInfo.editing

            isEmpty =
                Tasks.actionFromString edited == Nothing
          in
          label []
            [ span [ css [ hide ] ] [ text "Action" ]
            , input
                [ type_ "text"
                , value edited
                , onInput (EditTask <| toGlobalId id)
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
            [ let
                isUnchanged =
                    Tasks.getEdited editInfo.editing == Tasks.readPrevious editInfo.editing
              in
              iconButtonInputDisable
                isUnchanged
                "reset"
                (CancelEdit <| toGlobalId id)
                "Undo changes"
                "️↩️"
            , iconButtonInput "button" (DeleteTask <| toGlobalId id) "Delete task" "🗑️"
            ]
        ]


viewDoneTaskList : Tasks.Collection Done -> Html Msg
viewDoneTaskList collection =
    ol [ css [ taskListStyle ] ] <|
        List.map
            (\entry ->
                li
                    [ css
                        [ hover [ backgroundColor (rgba 0 0 0 0.03) ]
                        , pseudoClass "not(:last-child)"
                            [ borderBottom3 (px 1) solid (rgba 0 0 0 0.1)
                            ]
                        ]
                    ]
                    [ case entry.item of
                        Tasks.Task taskInfo ->
                            viewDoneTask entry.id taskInfo

                        Tasks.TaskInEdit editingInfo ->
                            viewEditDoneTask entry.id editingInfo
                    ]
            )
        <|
            Tasks.toList collection


viewDoneTask : Tasks.TaskId Done -> Tasks.TaskInfo -> Html Msg
viewDoneTask id task =
    viewTaskBase
        (onButtonClick (StartEdit <| DoneId id))
        (viewAction
            (batch
                [ textDecoration lineThrough
                , opacity (num 0.6)
                ]
            )
            (Tasks.stringFromAction task.action)
        )
        (iconButton (UndoTask id) "Mark as to do" "🔄")


viewEditDoneTask : Tasks.TaskId Done -> Tasks.TaskEditInfo -> Html Msg
viewEditDoneTask id editInfo =
    viewTaskBase
        (onButtonClick (ApplyEdit <| DoneId id))
        (viewEditAction id editInfo DoneId)
        (iconButton (UndoTask id) "Mark as to do" "🔄")



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
