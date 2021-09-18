module Poker.Board exposing (parser, validate)

import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Poker.Card as Card exposing (Card)
import Result.Extra


validate : String -> Result (List String) (List Card)
validate board =
    Parser.run parser (board |> String.replace " " "")
        |> Result.Extra.mapBoth (Parser.deadEndsToString >> List.singleton) identity
        |> Result.Extra.filter [ "The same card cannot appear multiple times" ]
            (\xs ->
                List.Extra.unique xs == xs
            )


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
