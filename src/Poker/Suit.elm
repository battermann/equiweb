module Poker.Suit exposing (Suit(..), all, gt, gte, lt, lte, parser, suitCombinations, suitToChar, toString)

import List.Extra
import Parser


type Suit
    = Club
    | Spades
    | Heart
    | Diamond


all : List Suit
all =
    [ Club
    , Spades
    , Heart
    , Diamond
    ]


gt : Suit -> Suit -> Bool
gt =
    compare (>)


gte : Suit -> Suit -> Bool
gte =
    compare (>=)


lt : Suit -> Suit -> Bool
lt =
    compare (<)


lte : Suit -> Suit -> Bool
lte =
    compare (<=)


compare : (Int -> Int -> Bool) -> Suit -> Suit -> Bool
compare f lhs rhs =
    f (suitToInt rhs) (suitToInt lhs)


suitToInt : Suit -> Int
suitToInt suit =
    case suit of
        Club ->
            4

        Spades ->
            3

        Heart ->
            2

        Diamond ->
            1


suitCombinations : List ( Suit, Suit )
suitCombinations =
    List.Extra.uniquePairs all


suitToChar : Suit -> Char
suitToChar suit =
    case suit of
        Spades ->
            '♠'

        Club ->
            '♣'

        Heart ->
            '♥'

        Diamond ->
            '♦'


toString : Suit -> String
toString suit =
    case suit of
        Spades ->
            "s"

        Club ->
            "c"

        Heart ->
            "h"

        Diamond ->
            "d"


parser : Parser.Parser Suit
parser =
    Parser.oneOf
        [ Parser.symbol "c" |> Parser.map (always Club)
        , Parser.symbol "C" |> Parser.map (always Club)
        , Parser.symbol "s" |> Parser.map (always Spades)
        , Parser.symbol "S" |> Parser.map (always Spades)
        , Parser.symbol "h" |> Parser.map (always Heart)
        , Parser.symbol "H" |> Parser.map (always Heart)
        , Parser.symbol "d" |> Parser.map (always Diamond)
        , Parser.symbol "D" |> Parser.map (always Diamond)
        ]
