module Poker.Board exposing (flopGenerator, parser, toString, validate)

import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Poker.Card as Card exposing (Card)
import Poker.CardRemoval as CardRemoval
import Poker.HandOrCombo exposing (HandOrCombo)
import Random exposing (Generator)
import Random.List
import Result.Extra


validate : String -> Result (List String) (List Card)
validate board =
    Parser.run parser (board |> String.replace " " "" |> String.replace "," "")
        |> Result.Extra.mapBoth (always [ "Board not valid" ]) identity
        |> Result.Extra.filter [ "The same card cannot appear multiple times" ]
            (\xs -> List.Extra.unique xs == xs)


parser : Parser (List Card)
parser =
    Parser.oneOf
        [ Parser.backtrackable riverParser
        , Parser.backtrackable turnParser
        , Parser.backtrackable flopParser
        , preflopParser
        ]


preflopParser : Parser (List Card)
preflopParser =
    Parser.succeed [] |. Parser.end


flopParser : Parser (List Card)
flopParser =
    Parser.succeed (\fst snd third -> [ fst, snd, third ])
        |= Card.parser
        |= Card.parser
        |= Card.parser
        |. Parser.end


turnParser : Parser (List Card)
turnParser =
    Parser.succeed (\flopCard1 flopCard2 flopCard3 turn -> [ flopCard1, flopCard2, flopCard3, turn ])
        |= Card.parser
        |= Card.parser
        |= Card.parser
        |= Card.parser
        |. Parser.end


riverParser : Parser (List Card)
riverParser =
    Parser.succeed (\flopCard1 flopCard2 flopCard3 turn river -> [ flopCard1, flopCard2, flopCard3, turn, river ])
        |= Card.parser
        |= Card.parser
        |= Card.parser
        |= Card.parser
        |= Card.parser
        |. Parser.end


toString : List Card -> String
toString cards =
    case cards of
        [] ->
            "Preflop"

        _ :: _ :: _ :: [] ->
            cards |> List.map Card.toString |> String.concat

        flop1 :: flop2 :: flop3 :: turn :: [] ->
            [ Card.toString flop1, Card.toString flop2, Card.toString flop3, " ", Card.toString turn ] |> String.concat

        flop1 :: flop2 :: flop3 :: turn :: river :: [] ->
            [ Card.toString flop1, Card.toString flop2, Card.toString flop3, " ", Card.toString turn, " ", Card.toString river ] |> String.concat

        _ ->
            ""


flopGenerator : List (List HandOrCombo) -> Generator (List Card)
flopGenerator ranges =
    CardRemoval.unblockedCards ranges Card.all |> Random.List.shuffle |> Random.map (List.take 3)
