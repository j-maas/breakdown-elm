module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
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
    , currentTodos : List (TodoEntry Current)
    , doneTodos : List (TodoEntry Done)
    }


type TodoEntry a
    = TodoEntry Todo


todoFromEntry : TodoEntry a -> Todo
todoFromEntry (TodoEntry todo) =
    todo


type TodoZipper
    = CurrentZipper (Zipper (TodoEntry Current))
    | DoneZipper (Zipper (TodoEntry Done))


zipperFromTodoZipper : TodoZipper -> Zipper Todo
zipperFromTodoZipper todoZipper =
    case todoZipper of
        CurrentZipper zipper ->
            Zipper.map todoFromEntry zipper

        DoneZipper zipper ->
            Zipper.map todoFromEntry zipper


type Current
    = Current


type Done
    = Done


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , newTodoInput = ""
      , currentTodos =
            [ TodoEntry <| Todo.from (NonEmptyString.build 'H' "ello")
            , TodoEntry <| Todo.from (NonEmptyString.build 't' "here")
            ]
      , doneTodos = [ TodoEntry <| Todo.from (NonEmptyString.build 'H' "ow's it going?") ]
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo
    | Move TodoZipper
    | Remove TodoZipper


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
                            model.currentTodos ++ [ TodoEntry todo ]
                    in
                    ( { model
                        | newTodoInput = ""
                        , currentTodos = newCurrentTodos
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Move todoZipper ->
            let
                ( newCurrent, newDone ) =
                    case todoZipper of
                        CurrentZipper zipper ->
                            let
                                currentZipper =
                                    Zipper.map todoFromEntry zipper

                                done =
                                    List.map todoFromEntry model.doneTodos

                                ( rawNewCurrent, rawNewDone ) =
                                    Zipper.move currentZipper done
                            in
                            ( List.map TodoEntry rawNewCurrent, List.map TodoEntry rawNewDone )

                        DoneZipper zipper ->
                            let
                                doneZipper =
                                    Zipper.map todoFromEntry zipper

                                current =
                                    List.map todoFromEntry model.currentTodos

                                ( rawNewDone, rawNewCurrent ) =
                                    Zipper.move doneZipper current
                            in
                            ( List.map TodoEntry rawNewCurrent, List.map TodoEntry rawNewDone )
            in
            ( { model | currentTodos = newCurrent, doneTodos = newDone }, Cmd.none )

        Remove todoZipper ->
            case todoZipper of
                CurrentZipper zipper ->
                    ( { model | currentTodos = Zipper.remove zipper }, Cmd.none )

                DoneZipper zipper ->
                    ( { model | doneTodos = Zipper.remove zipper }, Cmd.none )


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


viewCurrentTodos : List (TodoEntry Current) -> Html Msg
viewCurrentTodos todos =
    ul []
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    li [] [ viewTodo (CurrentZipper todoZipper) ]
                )
        )


viewDoneTodos : List (TodoEntry Done) -> Html Msg
viewDoneTodos todos =
    ul []
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    li [] [ viewTodo (DoneZipper todoZipper) ]
                )
        )


viewTodo : TodoZipper -> Html Msg
viewTodo todoZipper =
    let
        todo =
            Zipper.current (zipperFromTodoZipper todoZipper)

        moveText =
            case todoZipper of
                CurrentZipper _ ->
                    "Mark as done"

                DoneZipper _ ->
                    "Mark as to do"
    in
    div []
        [ text (Todo.action todo)
        , button [ onClick (Move todoZipper) ] [ text moveText ]
        , button [ onClick (Remove todoZipper) ] [ text "Remove" ]
        ]
