module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.element
        { subscriptions = subscriptions
        , view = view
        , update = update
        , init = init
        }


type alias Model =
    { attacks : Array Int }


initialModel : Model
initialModel =
    { attacks = Array.repeat 1 0 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = AddAttack
    | EditAttack Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAttack ->
            ( { model | attacks = Array.push 0 model.attacks }, Cmd.none )

        EditAttack index value ->
            ( { model | attacks = Array.set index value model.attacks }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] <| Array.toList <| Array.indexedMap viewAttack model.attacks
        , button [ onClick AddAttack ] [ text "+ Add attack" ]
        , div []
            [ text "Total damage: "
            , text <| String.fromInt <| total model.attacks
            ]
        ]


viewAttack : Int -> Int -> Html Msg
viewAttack index attack =
    div []
        [ label [] [ text "Attack value ", text <| String.fromInt <| index + 1 ]
        , input [ value <| String.fromInt attack, onInput (createEditAttack index) ] []
        ]


createEditAttack : Int -> String -> Msg
createEditAttack index value =
    EditAttack index <| Maybe.withDefault 0 <| String.toInt value


total : Array Int -> Int
total attacks =
    Array.foldl (+) 0 attacks
