port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global as Global
import Html.Styled as Html
    exposing
        ( Html
        , button
        , div
        , label
        , li
        , span
        , text
        , ul
        )
import Html.Styled.Attributes exposing (attribute, css, title, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import Json.Encode as Encode
import Todo exposing (Todo)
import TodoTree exposing (TodoNode(..), TodoTree)
import Url exposing (Url)
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }



-- INIT


type alias Model =
    { key : Nav.Key
    , newTodoInput : String
    , todos : TodoTree
    , editing : Maybe EditingInfo
    }


type alias EditingInfo =
    { todoId : TodoTree.Id
    , rawNewAction : String
    , oldAction : NonEmptyString
    , newSubtodoInput : String
    }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ key =
    let
        todos =
            decodeFlags flags
                |> Maybe.withDefault TodoTree.empty
    in
    ( { key = key
      , newTodoInput = ""
      , todos = todos
      , editing = Nothing
      }
    , Cmd.none
    )


decodeFlags : Decode.Value -> Maybe TodoTree
decodeFlags flags =
    let
        currentTodos =
            decodeTodos "currentTodos" flags

        doneTodos =
            decodeTodos "doneTodos" flags

        decodeTodos field value =
            Decode.decodeValue
                (Decode.field field
                    (Decode.list TodoTree.nodeDecoder)
                )
                value
    in
    Result.map2
        (\current done ->
            TodoTree.fromItems { current = current, done = done }
        )
        currentTodos
        doneTodos
        |> Result.toMaybe



-- UPDATE


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo
    | MoveToCurrent TodoTree.Id
    | MoveToDone TodoTree.Id
    | Remove TodoTree.Id
    | StartEdit TodoTree.Id
    | UpdateEdit EditingInfo
    | AddSubtodo EditingInfo
    | ApplyEdit
    | CancelEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateNewTodoInput newTodo ->
            let
                newModel =
                    { model | newTodoInput = newTodo }
            in
            ( newModel, Cmd.none )

        AddNewTodo ->
            let
                maybeAction =
                    NonEmptyString.fromString model.newTodoInput
            in
            case maybeAction of
                Just action ->
                    let
                        todo : Todo
                        todo =
                            Todo.from action
                    in
                    ( { model
                        | newTodoInput = ""
                        , todos =
                            TodoTree.insertCurrent (SimpleTodo todo) model.todos
                                |> Tuple.second
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MoveToCurrent id ->
            ( invalidateTodoWithId id
                model
                (TodoTree.moveToCurrent id)
            , Cmd.none
            )

        MoveToDone id ->
            ( invalidateTodoWithId id
                model
                (TodoTree.moveToDone id)
            , Cmd.none
            )

        Remove id ->
            ( invalidateTodoWithId id
                model
                (TodoTree.remove id)
            , Cmd.none
            )

        StartEdit id ->
            case TodoTree.get id model.todos of
                Just node ->
                    let
                        todo =
                            case node of
                                SimpleTodo t ->
                                    t

                                CompositTodo t _ ->
                                    t
                    in
                    ( { model
                        | editing =
                            Just
                                { todoId = id
                                , rawNewAction = Todo.readAction todo
                                , oldAction = Todo.action todo
                                , newSubtodoInput = ""
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdateEdit newEditInfo ->
            let
                newTodos =
                    NonEmptyString.fromString newEditInfo.rawNewAction
                        |> Maybe.andThen
                            (\newAction ->
                                TodoTree.update
                                    (\node ->
                                        case node of
                                            SimpleTodo t ->
                                                SimpleTodo (Todo.setAction newAction t)

                                            CompositTodo t subtodos ->
                                                CompositTodo (Todo.setAction newAction t) subtodos
                                    )
                                    newEditInfo.todoId
                                    model.todos
                            )
                        |> Maybe.withDefault model.todos
            in
            ( { model | todos = newTodos, editing = Just newEditInfo }, Cmd.none )

        AddSubtodo newEditInfo ->
            let
                newTodos =
                    NonEmptyString.fromString newEditInfo.newSubtodoInput
                        |> Maybe.andThen
                            (\newAction ->
                                TodoTree.insertCurrentAt
                                    newEditInfo.todoId
                                    (SimpleTodo (Todo.fromAction newAction))
                                    model.todos
                            )
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault model.todos

                updatedEditInfo =
                    { newEditInfo | newSubtodoInput = "" }
            in
            ( { model | todos = newTodos, editing = Just updatedEditInfo }, Cmd.none )

        ApplyEdit ->
            ( { model | editing = Nothing }, Cmd.none )

        CancelEdit ->
            let
                newTodos =
                    model.editing
                        |> Maybe.andThen
                            (\editInfo ->
                                TodoTree.update
                                    (\node ->
                                        case node of
                                            SimpleTodo t ->
                                                SimpleTodo (Todo.setAction editInfo.oldAction t)

                                            CompositTodo t subtodos ->
                                                CompositTodo (Todo.setAction editInfo.oldAction t) subtodos
                                    )
                                    editInfo.todoId
                                    model.todos
                            )
                        |> Maybe.withDefault model.todos
            in
            ( { model | todos = newTodos, editing = Nothing }, Cmd.none )
    )
        |> (\( mdl, cmd ) ->
                ( mdl, Cmd.batch [ cmd, save mdl ] )
           )


invalidateTodoWithId : TodoTree.Id -> Model -> (TodoTree -> Maybe TodoTree) -> Model
invalidateTodoWithId id model doUpdate =
    let
        newEditing =
            Maybe.andThen
                (\editInfo ->
                    if editInfo.todoId == id then
                        Nothing

                    else
                        model.editing
                )
                model.editing

        newTodos =
            doUpdate model.todos
                |> Maybe.withDefault model.todos
    in
    { model | editing = newEditing, todos = newTodos }


type Selector
    = Current
    | Done


{-| Command that saves the tasks collections persistently.
-}
save : Model -> Cmd msg
save model =
    let
        getTodos selector =
            case selector of
                Current ->
                    TodoTree.mapCurrent (\_ todo -> todo)

                Done ->
                    TodoTree.mapDone (\_ todo -> todo)
    in
    Encode.object
        [ ( "currentTodos", Encode.list TodoTree.encodeNode <| getTodos Current model.todos )
        , ( "doneTodos", Encode.list TodoTree.encodeNode <| getTodos Done model.todos )
        ]
        |> saveRaw


{-| Port for saving an encoded model to localStorage.
-}
port saveRaw : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Breakdown"
    , body =
        List.map Html.toUnstyled
            [ Global.global
                [ Global.body
                    [ maxWidth (em 26)
                    , margin2 (em 1) auto
                    , fontFamily sansSerif
                    ]
                ]
            , newTodoInput model.newTodoInput
            , viewCurrentTodos model.todos model.editing
            , viewDoneTodos model.todos model.editing
            ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentNewTodoInput =
    newTodoInputTemplate currentNewTodoInput UpdateNewTodoInput AddNewTodo


newTodoInputTemplate : String -> (String -> Msg) -> Msg -> Html Msg
newTodoInputTemplate currentInput onInputMsg onSubmitMsg =
    Html.form
        [ onSubmit onSubmitMsg
        , css
            [ inputContainerStyle
            ]
        ]
        [ input
            [ type_ "text"
            , onInput onInputMsg
            , value currentInput
            , css
                [ width (pct 100)
                , boxSizing borderBox
                , padding (em 0.75)
                , fontSize (pct 100)
                , height (em 3)
                , marginRight (em 0.5)
                ]
            ]
            []
        , inputSubmit "Add new todo" "add"
        ]


viewCurrentTodos : TodoTree -> Maybe EditingInfo -> Html Msg
viewCurrentTodos todos editing =
    ul [ css [ todoListStyle ] ]
        (todos
            |> TodoTree.mapCurrent
                (\id todo ->
                    let
                        currentEdit =
                            Maybe.andThen
                                (\editInfo ->
                                    if editInfo.todoId == id then
                                        Just editInfo

                                    else
                                        Nothing
                                )
                                editing
                    in
                    li [ css [ todoListEntryStyle ] ]
                        [ case currentEdit of
                            Just editInfo ->
                                viewEditTodo id todo editInfo

                            Nothing ->
                                viewTodo id Current todo
                        ]
                )
        )


viewDoneTodos : TodoTree -> Maybe EditingInfo -> Html Msg
viewDoneTodos todos editing =
    ul [ css [ textDecoration lineThrough, todoListStyle ] ]
        (todos
            |> TodoTree.mapDone
                (\id todo ->
                    let
                        currentEdit =
                            Maybe.andThen
                                (\editInfo ->
                                    if editInfo.todoId == id then
                                        Just editInfo

                                    else
                                        Nothing
                                )
                                editing
                    in
                    li
                        [ css
                            [ hover [ opacity (num 1) ]
                            , opacity (num 0.6)
                            , todoListEntryStyle
                            ]
                        ]
                        [ case currentEdit of
                            Just editInfo ->
                                viewEditTodo id todo editInfo

                            Nothing ->
                                viewTodo id Done todo
                        ]
                )
        )


viewTodo : TodoTree.Id -> Selector -> TodoNode -> Html Msg
viewTodo id selector node =
    let
        ( iconName, moveText, moveMessage ) =
            case selector of
                Current ->
                    ( "done", "Mark as done", MoveToDone )

                Done ->
                    ( "refresh", "Mark as to do", MoveToCurrent )

        ( todo, maybeSubtodos ) =
            case node of
                SimpleTodo t ->
                    ( t, Nothing )

                CompositTodo t subs ->
                    ( t, Just subs )
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "1fr auto"
            , property "grid-template-areas" "\"action button\" \"current current\" \"done done\""
            , property "grid-gap" "0.5em"
            , alignItems center
            , padding (em 0.5)
            ]
        , onClick (StartEdit id)
        ]
        [ text (Todo.readAction todo)
        , div []
            [ button moveText iconName (moveMessage id)
            ]
        , ul [ css [ todoListStyle, property "grid-area" "current" ] ]
            (case maybeSubtodos of
                Nothing ->
                    []

                Just subtodos ->
                    TodoTree.mapCurrentSubtodos
                        (\subId subNode ->
                            li [] [ viewTodo subId Current subNode ]
                        )
                        id
                        subtodos
            )
        , ul [ css [ textDecoration lineThrough, todoListStyle, property "grid-area" "done" ] ]
            (case maybeSubtodos of
                Nothing ->
                    []

                Just subtodos ->
                    TodoTree.mapDoneSubtodos
                        (\subId subNode ->
                            li [] [ viewTodo subId Done subNode ]
                        )
                        id
                        subtodos
            )
        ]


viewEditTodo : TodoTree.Id -> TodoNode -> EditingInfo -> Html Msg
viewEditTodo id node editInfo =
    let
        todo =
            case node of
                SimpleTodo t ->
                    t

                CompositTodo t _ ->
                    t
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "auto"
            , property "grid-gap" "0.5em"
            , alignItems center
            , padding (em 0.5)
            ]
        , onClick ApplyEdit
        ]
        [ Html.form [ onSubmit ApplyEdit ]
            [ input
                [ type_ "text"
                , onInput
                    (\newInput ->
                        UpdateEdit
                            { todoId = id
                            , rawNewAction = newInput
                            , oldAction = editInfo.oldAction
                            , newSubtodoInput = editInfo.newSubtodoInput
                            }
                    )
                , value editInfo.rawNewAction
                , css [ width (pct 100), boxSizing borderBox ]
                ]
                []
            ]
        , div []
            [ button "Undo changes" "undo" CancelEdit
            , button "Remove" "delete" (Remove id)
            ]
        , newTodoInputTemplate editInfo.newSubtodoInput
            (\newSubtodoInput ->
                UpdateEdit { editInfo | newSubtodoInput = newSubtodoInput }
            )
            (AddSubtodo editInfo)
        ]



-- COMPONENTS


button : String -> String -> Msg -> Html Msg
button description iconName action =
    Html.button
        [ onClick action, css [ buttonStyle, icon iconName ], title description ]
        [ span [ css [ visuallyHidden ] ] [ text description ] ]


inputSubmit : String -> String -> Html Msg
inputSubmit description iconName =
    label []
        [ span [ css [ visuallyHidden ] ] [ text description ]
        , input
            [ type_ "submit"
            , css [ buttonStyle, icon iconName, color transparent ]
            , title description
            ]
            []
        ]



-- STYLES


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ borderRadius (em 0.5)
        , backgroundColor (hsl 0.0 0.0 0.9)
        , border zero
        , padding (em 0.5)
        , margin (em 0.1)
        , textAlign center
        , hover [ backgroundColor (hsl 0.0 0.0 0.92) ]
        , active [ backgroundColor (hsl 0.0 0.0 0.88) ]
        ]


icon : String -> Css.Style
icon iconName =
    Css.batch
        [ backgroundImage (url <| "./icons/" ++ iconName ++ ".svg")
        , backgroundRepeat noRepeat
        , backgroundPosition center
        , backgroundSize (pct 75)
        , width (em 3)
        , height (em 3)
        ]


todoListStyle : Css.Style
todoListStyle =
    Css.batch
        [ listStyle none
        , padding zero
        ]


todoListEntryStyle : Css.Style
todoListEntryStyle =
    Css.batch
        [ borderBottom3 (px 1) solid (hsla 0.0 0.0 0.0 0.1)
        , hover
            [ backgroundColor (hsla 0.0 0.0 0.0 0.02)
            ]
        ]


inputContainerStyle : Css.Style
inputContainerStyle =
    Css.batch
        [ property "display" "grid"
        , property "grid-template-columns" "1fr auto"
        , property "grid-gap" "0.5em"
        , alignItems center
        , padding (em 0.5)
        ]


{-| Hides an element visually, but keeps it discoverable to assistive technologies.
See <https://www.w3.org/WAI/tutorials/forms/labels/#note-on-hiding-elements> for further information.
-}
visuallyHidden : Css.Style
visuallyHidden =
    Css.batch
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


input : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
input attributes children =
    Html.input (attributes ++ [ stopPropagation ]) children


stopPropagation : Html.Attribute Msg
stopPropagation =
    stopPropagationOn "click" (Decode.succeed ( NoOp, True ))


onClick : Msg -> Html.Attribute Msg
onClick msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))
