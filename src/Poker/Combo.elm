module Poker.Combo exposing
    ( Combo
    , aceOfClubsAceOfspades
    , all
    , combo
    , contains
    , fst
    , getOffsuit
    , getPairs
    , getSuited
    , hasBlocker
    , isOffsuit
    , isPair
    , isSuited
    , order
    , parser
    , percentage
    , snd
    , toString
    , total
    )

import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=))
import Poker.Card as Card exposing (Card)
import Poker.Rank as Rank exposing (Rank)
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
    if isPair (Combo h1 l1) && isPair (Combo h2 l2) then
        case Rank.order h1.rank h2.rank of
            EQ ->
                case Suit.order h1.suit h2.suit of
                    EQ ->
                        Suit.order l1.suit l2.suit

                    x ->
                        x

            x ->
                x

    else if isPair (Combo h1 l1) then
        GT

    else if isPair (Combo h2 l2) then
        LT

    else if isSuited (Combo h1 l1) && isSuited (Combo h2 l2) then
        case Rank.order h1.rank h2.rank of
            EQ ->
                case Rank.order l1.rank l2.rank of
                    EQ ->
                        case Suit.order h1.suit h2.suit of
                            EQ ->
                                Suit.order l1.suit l2.suit

                            x ->
                                x

                    x ->
                        x

            x ->
                x

    else if isSuited (Combo h1 l1) then
        GT

    else if isSuited (Combo h2 l2) then
        LT

    else
        case Rank.order h1.rank h2.rank of
            EQ ->
                case Rank.order l1.rank l2.rank of
                    EQ ->
                        case Suit.order h1.suit h2.suit of
                            EQ ->
                                Suit.order l1.suit l2.suit

                            x ->
                                x

                    x ->
                        x

            x ->
                x


total : Int
total =
    1326


isPair : Combo -> Bool
isPair (Combo card1 card2) =
    card1.rank == card2.rank


isSuited : Combo -> Bool
isSuited (Combo card1 card2) =
    card1.suit == card2.suit


isOffsuit : Combo -> Bool
isOffsuit c =
    not (isPair c) && not (isSuited c)


getPairs : Rank -> List Combo -> List Combo
getPairs rank =
    List.filter (\c -> isPair c && (fst c).rank == rank) >> List.Extra.unique


sameRanks : Combo -> Rank -> Rank -> Bool
sameRanks (Combo c1 c2) r1 r2 =
    (c1.rank == r1 && c2.rank == r2) || (c1.rank == r2 && c2.rank == r1)


getSuited : Rank -> Rank -> List Combo -> List Combo
getSuited rank1 rank2 =
    List.filter (\c -> isSuited c && sameRanks c rank1 rank2) >> List.Extra.unique


getOffsuit : Rank -> Rank -> List Combo -> List Combo
getOffsuit rank1 rank2 =
    List.filter (\c -> isOffsuit c && sameRanks c rank1 rank2) >> List.Extra.unique


contains : Card -> Combo -> Bool
contains card (Combo c1 c2) =
    c1 == card || c2 == card


hasBlocker : Combo -> Combo -> Bool
hasBlocker (Combo c1 c2) c =
    (c |> contains c1) || (c |> contains c2)


aceOfClubsAceOfspades : Combo
aceOfClubsAceOfspades =
    Combo (Card Rank.Ace Suit.Clubs) (Card Rank.Ace Suit.Spades)


percentage : Int -> Float
percentage n =
    toFloat n / toFloat total
