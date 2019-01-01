module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global as Global
import Html.Styled as Html
    exposing
        ( Html
        , button
        , div
        , input
        , label
        , li
        , span
        , text
        , ul
        )
import Html.Styled.Attributes exposing (attribute, css, title, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import List.Zipper as Zipper exposing (Zipper)
import Todo exposing (Todo)
import TodoCollection exposing (TodoCollection)
import Url exposing (Url)
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)
import Utils.ZipperUtils as Zipper


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


type alias Model =
    { key : Nav.Key
    , newTodoInput : String
    , todos : TodoCollection
    , editing : Maybe EditingInfo
    }


type alias EditingInfo =
    { todoId : TodoCollection.Id
    , rawNewAction : String
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , newTodoInput = ""
      , todos = TodoCollection.empty
      , editing = Nothing
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo
    | Move TodoCollection.Id
    | Remove TodoCollection.Id
    | StartEdit TodoCollection.Id
    | UpdateEdit TodoCollection.Id String
    | ApplyEdit
    | CancelEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                        todo =
                            Todo.from action
                    in
                    ( { model
                        | newTodoInput = ""
                        , todos = TodoCollection.put TodoCollection.Current todo model.todos
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Move id ->
            updateId id
                model
                (\zipper ->
                    ( { model | todos = TodoCollection.move zipper }, Cmd.none )
                )

        Remove id ->
            updateId id
                model
                (\zipper ->
                    ( { model | todos = TodoCollection.remove zipper }, Cmd.none )
                )

        StartEdit id ->
            updateId id
                model
                (\zipper ->
                    let
                        todo =
                            TodoCollection.current zipper
                    in
                    ( { model
                        | editing =
                            Just
                                { todoId = id
                                , rawNewAction = Todo.readAction todo
                                }
                      }
                    , Cmd.none
                    )
                )

        UpdateEdit id rawNewAction ->
            ( { model
                | editing =
                    Just
                        { todoId = id
                        , rawNewAction = rawNewAction
                        }
              }
            , Cmd.none
            )

        ApplyEdit ->
            model.editing
                |> Maybe.andThen
                    (\editInfo ->
                        NonEmptyString.fromString editInfo.rawNewAction
                            |> Maybe.map
                                (\newAction ->
                                    updateId editInfo.todoId
                                        model
                                        (\zipper ->
                                            ( { model
                                                | todos = TodoCollection.mapTodo (Todo.setAction newAction) zipper
                                                , editing = Nothing
                                              }
                                            , Cmd.none
                                            )
                                        )
                                )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        CancelEdit ->
            ( { model | editing = Nothing }, Cmd.none )


updateId : TodoCollection.Id -> Model -> (TodoCollection.Zipper -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateId id model doUpdate =
    case TodoCollection.find id model.todos of
        Just zipper ->
            doUpdate zipper

        Nothing ->
            ( model, Cmd.none )


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
    Html.form [ onSubmit AddNewTodo ]
        [ input [ type_ "text", onInput UpdateNewTodoInput, value currentNewTodoInput ] []
        , inputSubmit "Add new todo" "add"
        ]


viewCurrentTodos : TodoCollection -> Maybe EditingInfo -> Html Msg
viewCurrentTodos todos editing =
    ul [ css [ todoListStyle ] ]
        (todos
            |> TodoCollection.mapToList TodoCollection.Current
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
                                viewTodo id todo
                        ]
                )
        )


viewDoneTodos : TodoCollection -> Maybe EditingInfo -> Html Msg
viewDoneTodos todos editing =
    ul [ css [ textDecoration lineThrough, todoListStyle ] ]
        (todos
            |> TodoCollection.mapToList TodoCollection.Done
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
                                viewTodo id todo
                        ]
                )
        )


viewTodo : TodoCollection.Id -> Todo -> Html Msg
viewTodo id todo =
    let
        ( iconName, moveText ) =
            case TodoCollection.selectorFromId id of
                TodoCollection.Current ->
                    ( "done", "Mark as done" )

                TodoCollection.Done ->
                    ( "refresh", "Mark as to do" )
    in
    div [ css [ containerStyle ], onClick (StartEdit id) ]
        [ text (Todo.readAction todo)
        , div []
            [ button moveText iconName (Move id)
            ]
        ]


viewEditTodo : TodoCollection.Id -> Todo -> EditingInfo -> Html Msg
viewEditTodo id todo editInfo =
    div [ css [ containerStyle ], onClick ApplyEdit ]
        [ Html.form [ onSubmit ApplyEdit ]
            [ input
                [ type_ "text"
                , stopPropagation
                , onInput (UpdateEdit id)
                , value editInfo.rawNewAction
                ]
                []
            ]
        , div []
            [ button "Undo changes" "undo" CancelEdit
            , button "Remove" "delete" (Remove id)
            ]
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
            , css [ buttonStyle, icon iconName ]
            , title description
            , value ""
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
        , padding (em 0.5)
        , hover
            [ backgroundColor (hsla 0.0 0.0 0.0 0.02)
            ]
        ]


containerStyle : Css.Style
containerStyle =
    Css.batch
        [ displayFlex
        , flexDirection row
        , justifyContent spaceBetween
        , alignItems center
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


stopPropagation : Html.Attribute Msg
stopPropagation =
    stopPropagationOn "click" (Decode.succeed ( NoOp, True ))


onClick : Msg -> Html.Attribute Msg
onClick msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))
