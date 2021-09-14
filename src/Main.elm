module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, label, li, s, text, ul)
import Html.Attributes exposing (checked, for, id, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { completed : Bool
    , name : String
    }


type alias Model =
    { inputValue : String
    , todos : List Todo
    }


init : Model
init =
    { inputValue = "", todos = [] }



-- UPDATE


completeTodo : String -> Todo -> Todo
completeTodo name todo =
    if todo.name == name then
        { todo | completed = not todo.completed }

    else
        todo


type Msg
    = InputChange String
    | Add
    | Toggle String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChange newValue ->
            { model | inputValue = newValue }

        Add ->
            { model | todos = { name = model.inputValue, completed = False } :: model.todos }

        Toggle name ->
            { model | todos = List.map (completeTodo name) model.todos }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ label [ for "todo-input" ] [ text "Add something that needs done 🙂" ]
        , input [ id "todo-input", value model.inputValue, onInput InputChange ] []
        , button [ onClick Add ] [ text "Add" ]
        , ul [] (List.map viewTodo model.todos)
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [] [ input [ type_ "checkbox", checked todo.completed, onClick (Toggle todo.name) ] [], viewTodoText todo ]


viewTodoText : Todo -> Html Msg
viewTodoText todo =
    if todo.completed then
        s [] [ text todo.name ]

    else
        text todo.name
