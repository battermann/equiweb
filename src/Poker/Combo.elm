module Poker.Combo exposing (Combo, all, combo, order, parser, toString)

import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=))
import Poker.Card as Card exposing (Card)
import Poker.Rank as Rank
import Poker.Suit as Suit


type Combo
    = Combo Card Card


all : List Combo
all =
    List.Extra.uniquePairs Card.all |> List.map (\( c1, c2 ) -> combo c1 c2) |> Maybe.Extra.values


combo : Card -> Card -> Maybe Combo
combo card1 card2 =
    if card1 == card2 then
        Nothing

    else if card1.rank |> Rank.gt card2.rank then
        Just (Combo card1 card2)

    else if card1.rank |> Rank.lt card2.rank then
        Just (Combo card2 card1)

    else if card1.suit |> Suit.gt card2.suit then
        Just (Combo card1 card2)

    else
        Just (Combo card2 card1)


fst : Combo -> Card
fst (Combo card _) =
    card


snd : Combo -> Card
snd (Combo _ card) =
    card


toString : Combo -> String
toString c =
    Card.toString (fst c) ++ Card.toString (snd c)


parser : Parser.Parser Combo
parser =
    Parser.andThen
        (\maybeCombo ->
            case maybeCombo of
                Just c ->
                    Parser.succeed c

                Nothing ->
                    Parser.problem "Not a valid combo"
        )
        (Parser.succeed combo
            |= Card.parser
            |= Card.parser
            |. Parser.end
        )


order : Combo -> Combo -> Order
order (Combo h1 l1) (Combo h2 l2) =
    case Card.order h1 h2 of
        EQ ->
            Card.order l1 l2

        orElse ->
            orElse
