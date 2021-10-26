module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, datetime)
import Html.Events exposing (onClick)
import Time exposing (toDay, utc)


type alias Check =
    { amount : Float, checkNumber : Int, payTo : String, date : Time.Posix }


type alias Model =
    { checks : List Check, total : Float }


type Msg
    = AddCheck Check


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCheck check ->
            { model | checks = check :: model.checks }


checkRow : Check -> Html Msg
checkRow check =
    tr []
        [ td [] [ text <| String.fromInt check.checkNumber ]
        , td [] [ text <| String.fromFloat check.amount ]
        , td [] [ text check.payTo ]
        , td [] [ text <| String.fromInt (toDay utc check.date) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "has-text-centered", class "is-size-1" ] [ text "Balance It! (Your checkbook ...)" ]
        , div [ class "columns", class "is-centered"]
            [ table [ class "table" ]
                [ thead []
                    [ tr []
                        [ td [] [ text "Check Number" ]
                        , td [] [ text "Amount" ]
                        , td [] [ text "Pay to" ]
                        , td [] [ text "Date" ]
                        ]
                    ]
                , tbody [] (List.map checkRow model.checks)
                ]
            ]
        , button [] [ text "+" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox 
        { init = { 
            total = 5000.00,
            checks = [ { amount = 22.3, checkNumber = 3, date = Time.millisToPosix 100, payTo = "Me!" },
            { amount = 44.6, checkNumber = 4, date = Time.millisToPosix 100, payTo = "You!" } ] 
          }, 
          update = update, 
          view = view 
        }
