module Main exposing (main)

import Array exposing (empty)
import Browser
import Html exposing (Html, button, div, h1, input, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, datetime, placeholder, type_)
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


emptyCheck : Check
emptyCheck =
    Check 0 0 "" (Time.millisToPosix 0)


scan : (a -> b -> b) -> b -> List a -> List b
scan reducer state list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                next =
                    reducer x state
            in
            next :: scan reducer next xs


checkBalance : Check -> ( Check, Float ) -> ( Check, Float )
checkBalance check lastCheckWithBalance =
    let
        ( _, balance ) =
            lastCheckWithBalance
    in
    ( check, balance - check.amount )


curry2 : (( a, b ) -> c) -> a -> b -> c
curry2 fn =
    \a_ b_ -> fn ( a_, b_ )


uncurry2 : (a -> b -> c) -> ( a, b ) -> c
uncurry2 fn =
    \( a_, b_ ) -> fn a_ b_


checkRow : Check -> Float -> Html Msg
checkRow check balance =
    tr []
        [ td [] [ text <| String.fromInt check.checkNumber ]
        , td [] [ text <| String.fromFloat check.amount ]
        , td [] [ text check.payTo ]
        , td [] [ text <| String.fromInt (toDay utc check.date) ]
        , td [] [ text <| String.fromFloat balance ]
        ]


block : List (Html Msg) -> Html Msg
block =
    div [ class "block" ]


column : List (Html Msg) -> Html Msg
column =
    div [ class "column" ]


view : Model -> Html Msg
view model =
    div []
        [ block [ h1 [ class "has-text-centered", class "is-size-1" ] [ text "Balance It! (Your checkbook ...)" ] ]
        , block
            [ div [ class "columns", class "is-centered" ]
                [ div [ class "has-text-centered", class "is-size-3" ] [ text ("Initial Balance: " ++ String.fromFloat model.total) ]
                ]
            ]
        , block
            [ div [ class "columns", class "is-centered" ]
                -- turn into list
                [ column [ input [ class "input", placeholder "Check Number", type_ "number" ] [] ]
                , column [ input [ class "input", placeholder "Amount", type_ "number" ] [] ]
                , column [ input [ class "input", placeholder "Pay To", type_ "text" ] [] ]
                , column [ input [ class "input", placeholder "Date", type_ "date" ] [] ]
                , column [ button [ class "button", class "is-primary" ] [ text "+" ] ]
                ]
            , div [ class "columns", class "is-centered" ]
                [ table [ class "table" ]
                    [ thead []
                        [ tr []
                            [ td [] [ text "Check Number" ]
                            , td [] [ text "Amount" ]
                            , td [] [ text "Pay to" ]
                            , td [] [ text "Date" ]
                            , td [] [ text "Balanced!" ]
                            ]
                        ]
                    , tbody []
                        (model.checks
                            |> scan checkBalance ( emptyCheck, model.total )
                            |> List.map (uncurry2 checkRow)
                        )
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { total = 5000.0
            , checks =
                [ { amount = 22.3, checkNumber = 3, date = Time.millisToPosix 100, payTo = "Me!" }
                , { amount = 44.6, checkNumber = 4, date = Time.millisToPosix 100, payTo = "You!" }
                , { amount = 100.1, checkNumber = 5, date = Time.millisToPosix 100, payTo = "Someone" }
                ]
            }
        , update = update
        , view = view
        }
