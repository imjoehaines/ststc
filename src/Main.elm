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
    { attacks : Array Int
    , block : Int
    }


initialModel : Model
initialModel =
    { attacks = Array.repeat 1 0
    , block = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = EditAttack Int Int
    | EditBlock Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditAttack index value ->
            let
                newAttacks =
                    if index >= (Array.length model.attacks) then
                        Array.push value model.attacks
                    else
                        Array.set index value model.attacks
            in
                ( { model | attacks = newAttacks }, Cmd.none )

        EditBlock newBlock ->
            ( { model | block = newBlock }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] <| Array.toList <| Array.indexedMap viewAttack model.attacks
        , viewAttack (Array.length model.attacks) 0
        , div []
            [ label [] [ text "Block" ]
            , input [ value <| String.fromInt model.block, onInput createEditBlock ] []
            ]
        , div []
            [ text "Total damage: "
            , text <| String.fromInt <| calculateDamage model
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


createEditBlock : String -> Msg
createEditBlock value =
    EditBlock <| Maybe.withDefault 0 <| String.toInt value


calculateDamage : Model -> Int
calculateDamage model =
    max 0 ((totalDamage model.attacks) - model.block)


totalDamage : Array Int -> Int
totalDamage attacks =
    Array.foldl (+) 0 attacks
