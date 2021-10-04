module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (datetime)
import Html.Events exposing (onClick)
import Time


main : Program () Counter Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }


type alias Counter =
    Int


type Msg
    = Increment
    | Decrement


update : Msg -> Counter -> Counter
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Counter -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


type alias Check =
    { amount : Float, checkNumber : Int, payTo : String, date : Time.Posix }
