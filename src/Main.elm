module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, datetime, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Time exposing (Month(..), toDay, utc)



-- Lens stuff


type alias Lens a b =
    { get : a -> b
    , set : b -> a -> a
    }


compose : Lens b c -> Lens a b -> Lens a c
compose bc ab =
    Lens
        (ab.get >> bc.get)
        (\item coll -> ab.set (bc.set item (ab.get coll)) coll)


modelNewCheck : Lens Model Check
modelNewCheck =
    { get = \model -> model.newCheckInput
    , set = \check model -> { model | newCheckInput = check }
    }


checkPayTo : Lens Check String
checkPayTo =
    { get = \check -> check.payTo
    , set = \name check -> { check | payTo = name }
    }


checkAmount : Lens Check Float
checkAmount =
    { get = \check -> check.amount
    , set = \amount check -> { check | amount = amount }
    }


checkCheckNumber : Lens Check Int
checkCheckNumber =
    { get = \check -> check.checkNumber
    , set = \num check -> { check | checkNumber = num }
    }


checkDay : Lens Check Time.Posix
checkDay =
    { get = \check -> check.date
    , set = \day check -> { check | date = day }
    }


modelCheckNumber : Lens Model Int
modelCheckNumber =
    modelNewCheck |> compose checkCheckNumber


modelPayTo : Lens Model String
modelPayTo =
    modelNewCheck |> compose checkPayTo


modelAmount : Lens Model Float
modelAmount =
    modelNewCheck |> compose checkAmount


modelDay : Lens Model Time.Posix
modelDay =
    modelNewCheck |> compose checkDay



-- Model


type alias Check =
    { amount : Float
    , checkNumber : Int
    , payTo : String
    , date : Time.Posix
    }



-- TODO - add Set of check #'s already used
-- TODO - add way to classify spending


type alias Model =
    { checks : List Check
    , total : Float
    , newCheckInput : Check
    , month : Month
    , year : Int
    }



-- Update


type Msg
    = AddCheck
    | PayTo String
    | CheckNumber String
    | Amount String
    | Date String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCheck ->
            { model | checks = model.newCheckInput :: model.checks }

        PayTo name ->
            modelPayTo.set name model

        CheckNumber num ->
            modelCheckNumber.set (num |> String.toInt |> Maybe.withDefault 0) model

        Amount amount ->
            modelAmount.set (amount |> String.toFloat |> Maybe.withDefault 0.0) model

        Date day ->
            modelDay.set (day |> (String.toInt >> Maybe.withDefault 0) |> Time.millisToPosix) model



-- Helpers


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


monthToName : Month -> String
monthToName month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12



-- Bulma stuff


block : List (Html Msg) -> Html Msg
block =
    div [ class "block" ]


column : List (Html Msg) -> Html Msg
column =
    div [ class "column" ]


dcontrol : List (Html Msg) -> Html Msg
dcontrol =
    div [ class "control" ]



-- View


checkRow : Check -> Float -> Html Msg
checkRow check balance =
    tr []
        [ td [] [ text <| String.fromInt check.checkNumber ]
        , td [] [ text <| String.fromFloat check.amount ]
        , td [] [ text check.payTo ]
        , td [] [ text <| String.fromInt (toDay utc check.date) ]
        , td [] [ text <| String.fromFloat balance ]
        ]


addCheck : Html Msg
addCheck =
    div [ class "field", class "has-addons" ]
        [ dcontrol [ input [ class "input", placeholder "Check Number", type_ "number", onInput CheckNumber ] [] ]
        , dcontrol [ input [ class "input", placeholder "Amount", type_ "number", onInput Amount ] [] ]
        , dcontrol [ input [ class "input", placeholder "Pay To", type_ "text", onInput PayTo ] [] ]
        , dcontrol [ input [ class "input", placeholder "Date", type_ "date" ] [] ]
        , dcontrol [ button [ class "button", class "is-primary", onClick AddCheck ] [ text "+" ] ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ section [] [ block [ h1 [ class "has-text-centered", class "is-size-1" ] [ text "Balance It!!!! (Your checkbook ...)" ] ] ]
        , section [ class "container" ]
            [ block
                [ div [ class "columns", class "is-centered" ]
                    [ div [ class "has-text-centered", class "is-size-3" ] [ text (monthToName model.month ++ " starting balance: " ++ String.fromFloat model.total) ]
                    ]
                ]
            , block
                [ div [ class "columns", class "is-centered" ]
                    [ addCheck
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
                            -- TODO - do something better here (the reverses)
                            (model.checks
                                |> List.reverse
                                |> scan checkBalance ( emptyCheck, model.total )
                                |> List.reverse
                                |> List.map (uncurry2 checkRow)
                            )
                        ]
                    ]
                ]
            ]
        ]



-- Main


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
            , newCheckInput = emptyCheck
            , month = Oct
            , year = 2021
            }
        , update = update
        , view = view
        }
