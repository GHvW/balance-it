module Main exposing (main)

import Browser
import Html exposing (Html, button, div, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (datetime)
import Html.Events exposing (onClick)
import Time exposing (toDay, utc)


main : Program () Model Msg
main =
    Browser.sandbox { init = [ { amount = 22.3, checkNumber = 3, date = Time.millisToPosix 100, payTo = "Me!" } ], update = update, view = view }


type alias Model =
    List Check


type Msg
    = AddCheck Check


update : Msg -> List Check -> List Check
update msg model =
    case msg of
        AddCheck check ->
            check :: model


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ table []
                [ thead []
                    [ tr []
                        [ td [] [ text "Check Number" ]
                        , td [] [ text "Amount" ]
                        , td [] [ text "Pay to" ]
                        , td [] [ text "Date" ]
                        ]
                    ]
                , tbody []
                    (List.map
                        (\check ->
                            tr []
                                [ td [] [ text <| String.fromInt check.checkNumber ]
                                , td [] [ text <| String.fromFloat check.amount ]
                                , td [] [ text check.payTo ]
                                , td [] [ text <| String.fromInt (toDay utc check.date) ]
                                ]
                        )
                        model
                    )
                ]
            ]
        ]


type alias Check =
    { amount : Float, checkNumber : Int, payTo : String, date : Time.Posix }
