module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import List.Zipper as Zipper exposing (Zipper)
import Todo exposing (Todo)
import Url exposing (Url)
import Utils.NonEmptyString as NonEmptyString
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
    , currentTodos : List Todo
    , doneTodos : List Todo
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , newTodoInput = ""
      , currentTodos =
            [ Todo.from (NonEmptyString.build 'H' "ello")
            , Todo.from (NonEmptyString.build 't' "here")
            ]
      , doneTodos = [ Todo.from (NonEmptyString.build 'H' "ow's it going?") ]
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo


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

                        newCurrentTodos =
                            model.currentTodos ++ [ todo ]
                    in
                    ( { model
                        | newTodoInput = ""
                        , currentTodos = newCurrentTodos
                      }
                    , Cmd.none
                    )

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
        [ newTodoInput model.newTodoInput
        , viewCurrentTodos model.currentTodos
        , viewDoneTodos model.doneTodos
        ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentNewTodoInput =
    Html.form [ onSubmit AddNewTodo ]
        [ input [ type_ "text", onInput UpdateNewTodoInput, value currentNewTodoInput ] []
        , input [ type_ "submit", value "+" ] []
        ]


viewCurrentTodos : List Todo -> Html Msg
viewCurrentTodos todos =
    ul []
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    li [] [ viewTodo todoZipper ]
                )
        )


viewDoneTodos : List Todo -> Html Msg
viewDoneTodos todos =
    ul []
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    li [] [ viewTodo todoZipper ]
                )
        )


viewTodo : Zipper Todo -> Html Msg
viewTodo todoZipper =
    let
        todo =
            Zipper.current todoZipper
    in
    text (Todo.action todo)
