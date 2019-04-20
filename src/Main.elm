port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import CheckTree exposing (CheckTree, Node(..), Subnodes)
import Checklist exposing (Checklist)
import Css exposing (..)
import Css.Global as Css
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
import Maybe.Extra as Maybe
import Todo exposing (Todo)
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


type alias TodoTree =
    CheckTree Todo CompositTodo


type alias TodoNode =
    Node Todo CompositTodo


type alias TodoSubnodes =
    Subnodes Todo CompositTodo


type alias CompositTodo =
    ( Todo, Fold )


type Fold
    = Show
    | Hide


toggleFold : Fold -> Fold
toggleFold fold =
    case fold of
        Show ->
            Hide

        Hide ->
            Show


type alias EditingInfo =
    { todoId : CheckTree.Id
    , rawNewAction : String
    , oldAction : NonEmptyString
    , newSubtodoInput : String
    }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ key =
    let
        todos =
            decodeFlags flags
                |> Maybe.withDefault CheckTree.empty
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
                    (Decode.list <| CheckTree.nodeDecoder Todo.decoder decodeCompositTodo)
                )
                value
    in
    Result.map2
        (\current done ->
            CheckTree.fromItems { current = current, done = done }
        )
        currentTodos
        doneTodos
        |> Result.toMaybe


encodeCompositeTodo : CompositTodo -> Encode.Value
encodeCompositeTodo ( todo, _ ) =
    Todo.encode todo


decodeCompositTodo : Decode.Decoder CompositTodo
decodeCompositTodo =
    Decode.map (\todo -> ( todo, Hide )) Todo.decoder



-- UPDATE


type Msg
    = NoOp
    | ChangedNewTodoInput String
    | AddNewTodo
    | MoveToCurrent CheckTree.Id
    | MoveToDone CheckTree.Id
    | Remove CheckTree.Id
    | ClickedTodo CheckTree.Id
    | DoubleClickedTodo CheckTree.Id
    | UpdateEdit EditingInfo
    | AddSubtodo EditingInfo
    | ApplyEdit
    | CancelEdit
    | BackgroundClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangedNewTodoInput newTodo ->
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
                            CheckTree.insertCurrent (SimpleNode todo) model.todos
                                |> Tuple.second
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MoveToCurrent id ->
            ( invalidateTodoWithId id
                model
                (CheckTree.moveToCurrent id)
            , Cmd.none
            )

        MoveToDone id ->
            ( invalidateTodoWithId id
                model
                (CheckTree.moveToDone id)
            , Cmd.none
            )

        Remove id ->
            ( invalidateTodoWithId id
                model
                (CheckTree.remove id)
            , Cmd.none
            )

        ClickedTodo id ->
            let
                newModel =
                    { model
                        | todos =
                            CheckTree.update
                                (\node ->
                                    case node of
                                        CompositNode ( todo, fold ) children ->
                                            CompositNode ( todo, toggleFold fold ) children

                                        n ->
                                            n
                                )
                                id
                                model.todos
                                |> Maybe.withDefault model.todos
                    }
            in
            ( applyEdit newModel, Cmd.none )

        DoubleClickedTodo id ->
            case CheckTree.get id model.todos of
                Just node ->
                    let
                        todo =
                            case node of
                                SimpleNode t ->
                                    t

                                CompositNode ( t, _ ) _ ->
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
                                CheckTree.update
                                    (\node ->
                                        case node of
                                            SimpleNode t ->
                                                SimpleNode (Todo.setAction newAction t)

                                            CompositNode ( t, fold ) subtodos ->
                                                CompositNode ( Todo.setAction newAction t, fold ) subtodos
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
                                CheckTree.insertCurrentAt
                                    newEditInfo.todoId
                                    (SimpleNode (Todo.fromAction newAction))
                                    (\todo -> ( todo, Hide ))
                                    model.todos
                            )
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault model.todos

                updatedEditInfo =
                    { newEditInfo | newSubtodoInput = "" }
            in
            ( { model | todos = newTodos, editing = Just updatedEditInfo }, Cmd.none )

        ApplyEdit ->
            ( applyEdit model, Cmd.none )

        CancelEdit ->
            ( cancelEdit model, Cmd.none )

        BackgroundClicked ->
            ( applyEdit model, Cmd.none )
    )
        |> (\( mdl, cmd ) ->
                ( mdl, Cmd.batch [ cmd, save mdl ] )
           )


applyEdit : Model -> Model
applyEdit model =
    { model | editing = Nothing }


cancelEdit : Model -> Model
cancelEdit model =
    let
        newTodos =
            model.editing
                |> Maybe.andThen
                    (\editInfo ->
                        CheckTree.update
                            (\node ->
                                case node of
                                    SimpleNode t ->
                                        SimpleNode (Todo.setAction editInfo.oldAction t)

                                    CompositNode ( t, fold ) subtodos ->
                                        CompositNode ( Todo.setAction editInfo.oldAction t, fold ) subtodos
                            )
                            editInfo.todoId
                            model.todos
                    )
                |> Maybe.withDefault model.todos
    in
    { model | todos = newTodos, editing = Nothing }


invalidateTodoWithId : CheckTree.Id -> Model -> (TodoTree -> Maybe TodoTree) -> Model
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
                    CheckTree.mapCurrent (\_ todo -> todo)

                Done ->
                    CheckTree.mapDone (\_ todo -> todo)
    in
    Encode.object
        [ ( "currentTodos", Encode.list (CheckTree.encodeNode Todo.encode encodeCompositeTodo) <| getTodos Current model.todos )
        , ( "doneTodos", Encode.list (CheckTree.encodeNode Todo.encode encodeCompositeTodo) <| getTodos Done model.todos )
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
            [ Css.global
                [ Css.body
                    [ margin zero
                    ]
                ]
            , div
                [ css
                    [ width (vw 100)
                    , height (vh 100)
                    ]
                , onClick BackgroundClicked
                ]
                [ Html.main_
                    [ css
                        [ maxWidth (em 26)
                        , margin2 zero auto
                        , padding2 (em 1) zero
                        , fontFamily sansSerif
                        ]
                    ]
                    [ viewTodoTree model.todos
                        model.editing
                        model.newTodoInput
                    ]
                ]
            ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentInput =
    newTodoInputTemplate currentInput
        { onInput = ChangedNewTodoInput, onSubmit = AddNewTodo }


newSubtodoInput : CheckTree.Id -> EditingInfo -> Html Msg
newSubtodoInput id editInfo =
    newTodoInputTemplate editInfo.newSubtodoInput
        { onInput = \newInput -> UpdateEdit { editInfo | newSubtodoInput = newInput }
        , onSubmit = AddSubtodo editInfo
        }


newTodoInputTemplate : String -> { onInput : String -> Msg, onSubmit : Msg } -> Html Msg
newTodoInputTemplate currentInput msgs =
    Html.form
        [ onSubmit msgs.onSubmit
        , css
            [ inputContainerStyle
            , todoLeftMarginStyle
            , todoRightMarginStyle
            ]
        ]
        [ input
            [ type_ "text"
            , onInput msgs.onInput
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


inputContainerStyle : Css.Style
inputContainerStyle =
    Css.batch
        [ property "display" "grid"
        , property "grid-template-columns" "1fr auto"
        , property "grid-gap" "0.5em"
        , alignItems center
        , todoVerticalMarginStyle
        ]


viewTodoTree : TodoTree -> Maybe EditingInfo -> String -> Html Msg
viewTodoTree todos editing currentInput =
    div [ css [ todoTreeStyle ] ]
        [ ul [ css [ todoListStyle ] ]
            ([ li [ css [ todoListEntryStyle ] ] [ newTodoInput currentInput ] ]
                ++ CheckTree.mapCurrent
                    (viewTodoListItem Current editing)
                    todos
            )
        , ul [ css [ textDecoration lineThrough, todoListStyle ] ]
            (CheckTree.mapDone
                (viewTodoListItem Done editing)
                todos
            )
        ]


viewSubtodoTree : CheckTree.Id -> TodoSubnodes -> Maybe EditingInfo -> Html Msg
viewSubtodoTree id subtodos editing =
    div [ css [ todoTreeStyle ] ]
        [ ul [ css [ todoListStyle ] ]
            ((case editingForCurrent id editing of
                Just editInfo ->
                    [ li [ css [ todoListEntryStyle ] ] [ newSubtodoInput id editInfo ] ]

                Nothing ->
                    []
             )
                ++ CheckTree.mapCurrentSubtodos
                    (viewTodoListItem Current editing)
                    id
                    subtodos
            )
        , ul [ css [ textDecoration lineThrough, todoListStyle ] ]
            (CheckTree.mapDoneSubtodos
                (viewTodoListItem Done editing)
                id
                subtodos
            )
        ]


todoTreeStyle : Css.Style
todoTreeStyle =
    Css.batch
        [ property "display" "grid"
        , property "grid-template-columns" "auto"
        , property "grid-gap" "0.5em"
        ]


editingForCurrent : CheckTree.Id -> Maybe EditingInfo -> Maybe EditingInfo
editingForCurrent id editing =
    Maybe.filter (\edit -> edit.todoId == id) editing


viewTodoListItem : Selector -> Maybe EditingInfo -> CheckTree.Id -> TodoNode -> Html Msg
viewTodoListItem selector editing id node =
    let
        extraStyles =
            case selector of
                Done ->
                    [ hover [ opacity (num 1) ], opacity (num 0.6) ]

                _ ->
                    []
    in
    li
        [ css
            ([ todoListEntryStyle
             ]
                ++ extraStyles
            )
        ]
        [ case editingForCurrent id editing of
            Just edit ->
                viewEditNode id node edit

            Nothing ->
                viewTodoNode selector editing id node
        ]


todoListStyle : Css.Style
todoListStyle =
    Css.batch
        [ listStyle none
        , padding zero
        , margin zero
        , todoListEntryBorderStyle borderLeft3
        , todoListEntryBorderStyle borderRight3
        , Css.descendants [ Css.typeSelector "ul" [ borderRight zero ] ]
        ]


todoListEntryStyle : Css.Style
todoListEntryStyle =
    Css.batch
        [ Css.adjacentSiblings [ Css.typeSelector "li" [ borderTop zero ] ]
        , todoListEntryBorderStyle borderTop3
        , todoListEntryBorderStyle borderBottom3
        , hover
            [ backgroundColor (hsla 0.0 0.0 0.0 0.02)
            ]
        ]


todoListEntryBorderStyle border =
    border (px 1) solid (hsla 0.0 0.0 0.0 0.1)


viewTodoNode : Selector -> Maybe EditingInfo -> CheckTree.Id -> TodoNode -> Html Msg
viewTodoNode selector editing id node =
    let
        ( iconName, moveText, moveMessage ) =
            case selector of
                Current ->
                    ( "done", "Mark as done", MoveToDone )

                Done ->
                    ( "refresh", "Mark as to do", MoveToCurrent )

        ( todo, maybeSubtodos, toggleIcon ) =
            case node of
                SimpleNode t ->
                    ( t, Nothing, [] )

                CompositNode ( t, fold ) subs ->
                    let
                        ( maybeSubtodos_, toggleIcon_ ) =
                            case fold of
                                Show ->
                                    ( Just subs, icon "arrow-down" )

                                Hide ->
                                    ( Nothing, icon "arrow-right" )
                    in
                    ( t, maybeSubtodos_, [ toggleIcon_ ] )
    in
    div
        [ css
            [ todoContainerStyle
            ]
        , onClick (ClickedTodo id)
        , onDoubleClick (DoubleClickedTodo id)
        ]
        ([ div [ css [ todoLeftMarginStyle, displayFlex, flexDirection row, alignItems center ] ] (toggleIcon ++ [ text (Todo.readAction todo) ])
         , div [ css [ todoRightMarginStyle ] ] [ button moveText iconName (moveMessage id) ]
         ]
            ++ (case maybeSubtodos of
                    Just subtodos ->
                        [ div [ css [ subtodoTreeStyle ] ]
                            [ viewSubtodoTree id subtodos editing ]
                        ]

                    Nothing ->
                        []
               )
        )


viewEditNode : CheckTree.Id -> TodoNode -> EditingInfo -> Html Msg
viewEditNode id node editInfo =
    let
        ( todo, subtodos ) =
            case node of
                SimpleNode t ->
                    ( t, CheckTree.emptySubnodes )

                CompositNode ( t, _ ) subs ->
                    ( t, subs )
    in
    div
        [ css
            [ todoContainerStyle
            ]
        , onClick ApplyEdit
        ]
        [ Html.form [ onSubmit ApplyEdit, css [ todoLeftMarginStyle ] ]
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
        , div [ css [ todoRightMarginStyle ] ]
            [ button "Undo changes" "undo" CancelEdit
            , button "Remove" "delete" (Remove id)
            ]
        , div [ css [ subtodoTreeStyle ] ]
            [ viewSubtodoTree id subtodos (Just editInfo) ]
        ]


todoContainerStyle : Css.Style
todoContainerStyle =
    Css.batch
        [ property "display" "grid"
        , property "grid-template-columns" "1fr auto"
        , property "grid-gap" "0.5em"
        , alignItems center
        , paddingTop (em 0.5)
        , paddingBottom (em 0.5)
        ]


todoMargin =
    em 0.5


todoVerticalMarginStyle : Css.Style
todoVerticalMarginStyle =
    Css.batch
        [ marginTop todoMargin
        , marginBottom todoMargin
        ]


todoLeftMarginStyle : Css.Style
todoLeftMarginStyle =
    marginLeft todoMargin


todoRightMarginStyle : Css.Style
todoRightMarginStyle =
    marginRight todoMargin


subtodoTreeStyle : Css.Style
subtodoTreeStyle =
    Css.batch
        [ property "grid-column" "1 / 3"
        , paddingLeft (em 1)
        ]



-- COMPONENTS


button : String -> String -> Msg -> Html Msg
button description iconName action =
    Html.button
        [ onClick action, css [ iconStyle iconName, buttonStyle ], title description ]
        [ span [ css [ visuallyHidden ] ] [ text description ] ]


inputSubmit : String -> String -> Html Msg
inputSubmit description iconName =
    label []
        [ span [ css [ visuallyHidden ] ] [ text description ]
        , input
            [ type_ "submit"
            , css [ iconStyle iconName, buttonStyle, color transparent ]
            , title description
            ]
            []
        ]


icon : String -> Html msg
icon iconName =
    Html.div [ css [ iconStyle iconName ] ] []



-- STYLES


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ borderRadius (em 0.5)
        , backgroundColor (hsl 0.0 0.0 0.9)
        , border zero
        , padding (em 0.5)
        , margin (em 0.1)
        , width (em 3)
        , height (em 3)
        , backgroundSize (pct 75)
        , textAlign center
        , hover [ backgroundColor (hsl 0.0 0.0 0.92) ]
        , active [ backgroundColor (hsl 0.0 0.0 0.88) ]
        ]


iconStyle : String -> Css.Style
iconStyle iconName =
    Css.batch
        [ backgroundImage (url <| "./icons/" ++ iconName ++ ".svg")
        , backgroundRepeat noRepeat
        , backgroundPosition center
        , backgroundSize (pct 100)
        , minWidth (em 2)
        , minHeight (em 2)
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


onDoubleClick : Msg -> Html.Attribute Msg
onDoubleClick msg =
    stopPropagationOn "dblclick" (Decode.succeed ( msg, True ))
