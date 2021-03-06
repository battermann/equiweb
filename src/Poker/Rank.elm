module Poker.Rank exposing
    ( Rank(..)
    , all
    , from
    , gt
    , gte
    , isConnected
    , lt
    , lte
    , order
    , parser
    , range
    , toString
    )

import Parser exposing (Parser)


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


toString : Rank -> String
toString rank =
    case rank of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "T"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Ace ->
            "A"


all : List Rank
all =
    [ Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two ]


from : Rank -> List Rank
from rank =
    all |> List.filter (\r -> toInt r >= toInt rank)


range : Rank -> Rank -> List Rank
range r1 r2 =
    if gt r1 r2 then
        all |> List.filter (\r -> (toInt r >= toInt r1) && (toInt r <= toInt r2))

    else
        all |> List.filter (\r -> (toInt r >= toInt r2) && (toInt r <= toInt r1))


gt : Rank -> Rank -> Bool
gt =
    compare (>)


gte : Rank -> Rank -> Bool
gte =
    compare (>=)


lt : Rank -> Rank -> Bool
lt =
    compare (<)


lte : Rank -> Rank -> Bool
lte =
    compare (<=)


compare : (Int -> Int -> Bool) -> Rank -> Rank -> Bool
compare f lhs rhs =
    f (toInt rhs) (toInt lhs)


toInt : Rank -> Int
toInt rank =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14


parser : Parser Rank
parser =
    Parser.oneOf
        [ Parser.symbol "A" |> Parser.map (always Ace)
        , Parser.symbol "a" |> Parser.map (always Ace)
        , Parser.symbol "K" |> Parser.map (always King)
        , Parser.symbol "k" |> Parser.map (always King)
        , Parser.symbol "Q" |> Parser.map (always Queen)
        , Parser.symbol "q" |> Parser.map (always Queen)
        , Parser.symbol "J" |> Parser.map (always Jack)
        , Parser.symbol "j" |> Parser.map (always Jack)
        , Parser.symbol "T" |> Parser.map (always Ten)
        , Parser.symbol "t" |> Parser.map (always Ten)
        , Parser.symbol "9" |> Parser.map (always Nine)
        , Parser.symbol "8" |> Parser.map (always Eight)
        , Parser.symbol "7" |> Parser.map (always Seven)
        , Parser.symbol "6" |> Parser.map (always Six)
        , Parser.symbol "5" |> Parser.map (always Five)
        , Parser.symbol "4" |> Parser.map (always Four)
        , Parser.symbol "3" |> Parser.map (always Three)
        , Parser.symbol "2" |> Parser.map (always Two)
        ]


order : Rank -> Rank -> Order
order r1 r2 =
    if r1 |> gt r2 then
        GT

    else if r1 |> lt r2 then
        LT

    else
        EQ


isConnected : Rank -> Rank -> Bool
isConnected r1 r2 =
    abs (toInt r1 - toInt r2) == 1
