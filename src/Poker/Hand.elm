module Poker.Hand exposing (Hand, combos, grid, highCard, isOffsuit, isPair, isSuited, lowCard, magic, offsuit, offsuitAces, offsuitBroadways, order, pair, pairs, parser, suited, suitedAces, suitedBroadways, toHandRanges, toString)

import List
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Poker.Card exposing (Card)
import Poker.Combo as Combo exposing (Combo)
import Poker.Ranges exposing (Ranges(..))
import Poker.Rank as Rank exposing (Rank)
import Poker.Suit as Suit


type Hand
    = Pair Rank
    | Suited Rank Rank
    | Offsuit Rank Rank


combos : Hand -> List Combo
combos hand =
    case hand of
        Pair rank ->
            Suit.suitCombinations
                |> List.map (\( s1, s2 ) -> Combo.combo (Card rank s1) (Card rank s2))
                |> Maybe.Extra.values

        Suited high low ->
            Suit.all
                |> List.map (\suit -> Combo.combo (Card high suit) (Card low suit))
                |> Maybe.Extra.values

        Offsuit high low ->
            Suit.all
                |> List.concatMap (\s1 -> Suit.all |> List.map (Tuple.pair s1))
                |> List.filter (\( s1, s2 ) -> s1 /= s2)
                |> List.map (\( s1, s2 ) -> Combo.combo (Card high s1) (Card low s2))
                |> Maybe.Extra.values


grid : List (List Hand)
grid =
    Rank.all
        |> List.reverse
        |> List.map
            (\r1 ->
                Rank.all
                    |> List.reverse
                    |> List.map
                        (\r2 ->
                            if r1 == r2 then
                                Pair r1

                            else if r1 |> Rank.gt r2 then
                                Suited r1 r2

                            else
                                Offsuit r2 r1
                        )
            )


isOffsuit : Hand -> Bool
isOffsuit hand =
    case hand of
        Offsuit _ _ ->
            True

        _ ->
            False


isPair : Hand -> Bool
isPair hand =
    case hand of
        Pair _ ->
            True

        _ ->
            False


isSuited : Hand -> Bool
isSuited hand =
    case hand of
        Suited _ _ ->
            True

        _ ->
            False


toString : Hand -> String
toString hand =
    case hand of
        Pair rank ->
            Rank.toString rank ++ Rank.toString rank

        Suited high low ->
            Rank.toString high ++ Rank.toString low ++ "s"

        Offsuit high low ->
            Rank.toString high ++ Rank.toString low ++ "o"


highCard : Hand -> Rank
highCard hand =
    case hand of
        Pair rank ->
            rank

        Suited rank _ ->
            rank

        Offsuit rank _ ->
            rank


lowCard : Hand -> Rank
lowCard hand =
    case hand of
        Pair rank ->
            rank

        Suited _ rank ->
            rank

        Offsuit _ rank ->
            rank


pair : Rank -> Hand
pair rank =
    Pair rank


suited : Rank -> Rank -> Maybe Hand
suited r1 r2 =
    if r1 == r2 then
        Nothing

    else if r1 |> Rank.gt r2 then
        Just <| Suited r1 r2

    else
        Just <| Suited r2 r1


offsuit : Rank -> Rank -> Maybe Hand
offsuit r1 r2 =
    if r1 == r2 then
        Nothing

    else if r1 |> Rank.gt r2 then
        Just <| Offsuit r1 r2

    else
        Just <| Offsuit r2 r1


parser : Parser (List Hand)
parser =
    Parser.oneOf
        [ Parser.backtrackable rangeParser |. Parser.end
        , Parser.backtrackable rangePlusParser |. Parser.end
        , singleHandParser |. Parser.end
        ]


singleHandParser : Parser (List Hand)
singleHandParser =
    Parser.oneOf
        [ Parser.backtrackable (pairParser |> Parser.map List.singleton)
        , Parser.backtrackable (suitedParser |> Parser.map List.singleton)
        , Parser.backtrackable (offsuitParser |> Parser.map List.singleton)
        , anySuitParser
        ]


ranksParser : Parser ( Rank, Rank )
ranksParser =
    Parser.succeed Tuple.pair
        |= Rank.parser
        |= Rank.parser


equalRanksParser : Parser.Parser Rank
equalRanksParser =
    ranksParser
        |> Parser.andThen
            (\( r1, r2 ) ->
                if r1 == r2 then
                    Parser.succeed r1

                else
                    Parser.problem "Ranks are not equal"
            )


pairParser : Parser.Parser Hand
pairParser =
    equalRanksParser |> Parser.map Pair


validateUnpaired : ( Rank, Rank ) -> Parser ( Rank, Rank )
validateUnpaired ( r1, r2 ) =
    if r1 |> Rank.gt r2 then
        Parser.succeed ( r1, r2 )

    else if r2 |> Rank.gt r1 then
        Parser.succeed ( r2, r1 )

    else
        Parser.problem "Ranks must not be equal"


unpairedParser : Parser ( Rank, Rank )
unpairedParser =
    ranksParser |> Parser.andThen validateUnpaired


toSuited : ( Rank, Rank ) -> Parser Hand
toSuited ( r1, r2 ) =
    case suited r1 r2 of
        Just hand ->
            Parser.succeed hand

        Nothing ->
            Parser.problem "Suited hand is not valid"


toOffsuit : ( Rank, Rank ) -> Parser Hand
toOffsuit ( r1, r2 ) =
    case offsuit r1 r2 of
        Just hand ->
            Parser.succeed hand

        Nothing ->
            Parser.problem "Offsuit hand is not valid"


suitedParser : Parser Hand
suitedParser =
    (unpairedParser |> Parser.andThen toSuited)
        |. Parser.symbol "s"


offsuitParser : Parser Hand
offsuitParser =
    (unpairedParser |> Parser.andThen toOffsuit)
        |. Parser.symbol "o"


anySuitParser : Parser (List Hand)
anySuitParser =
    unpairedParser
        |> Parser.andThen
            (\( r1, r2 ) ->
                toSuited ( r1, r2 )
                    |> Parser.andThen
                        (\suitedHand ->
                            toOffsuit ( r1, r2 )
                                |> Parser.map (\offsuitHand -> [ suitedHand, offsuitHand ])
                        )
            )


rangeParser : Parser (List Hand)
rangeParser =
    Parser.andThen combineTuplesToRanges <|
        Parser.succeed Tuple.pair
            |= singleHandParser
            |. Parser.symbol "-"
            |= singleHandParser


combineTuplesToRanges : ( List Hand, List Hand ) -> Parser (List Hand)
combineTuplesToRanges ( list1, list2 ) =
    List.map2 validateRange list1 list2
        |> List.foldl (\p1 p2 -> p1 |> Parser.andThen (\r1 -> p2 |> Parser.map (\r2 -> r2 ++ r1))) (Parser.succeed [])
        |> Parser.andThen validateNonEmpty


validateNonEmpty : List a -> Parser (List a)
validateNonEmpty =
    \list ->
        case list of
            [] ->
                Parser.problem "Range must not be empty"

            xs ->
                Parser.succeed xs


validateRange : Hand -> Hand -> Parser (List Hand)
validateRange h1 h2 =
    case ( h1, h2 ) of
        ( Suited high1 low1, Suited high2 low2 ) ->
            if high1 == high2 then
                Parser.succeed (Rank.range low1 low2 |> List.map (Suited high1))

            else
                Parser.problem "This is not a valid hand range"

        ( Offsuit high1 low1, Offsuit high2 low2 ) ->
            if high1 == high2 then
                Parser.succeed (Rank.range low1 low2 |> List.map (Offsuit high1))

            else
                Parser.problem "This is not a valid hand range"

        ( Pair r1, Pair r2 ) ->
            Parser.succeed (Rank.range r1 r2 |> List.map Pair)

        _ ->
            Parser.problem "This is not a valid hand range"


rangePlusParser : Parser (List Hand)
rangePlusParser =
    Parser.andThen combineHandsToPlusRanges <|
        singleHandParser
            |. Parser.symbol "+"


combineHandsToPlusRanges : List Hand -> Parser (List Hand)
combineHandsToPlusRanges =
    List.foldl (\h p -> validateRangePlus h |> Parser.andThen (\r1 -> p |> Parser.map (\r2 -> r2 ++ r1))) (Parser.succeed [])


validateRangePlus : Hand -> Parser (List Hand)
validateRangePlus hand =
    case hand of
        Suited high low ->
            Parser.succeed (Rank.range high low |> List.drop 1 |> List.map (Suited high))

        Offsuit high low ->
            Parser.succeed (Rank.range high low |> List.drop 1 |> List.map (Offsuit high))

        Pair r ->
            Parser.succeed (Rank.from r |> List.map Pair)


order : Hand -> Hand -> Order
order lhs rhs =
    case ( lhs, rhs ) of
        ( Pair r1, Pair r2 ) ->
            if r1 |> Rank.gt r2 then
                GT

            else if r1 |> Rank.lt r2 then
                LT

            else
                EQ

        ( Pair _, _ ) ->
            GT

        ( _, Pair _ ) ->
            LT

        ( Suited _ _, Offsuit _ _ ) ->
            GT

        ( Suited h1 l1, Suited h2 l2 ) ->
            if h1 |> Rank.gt h2 then
                GT

            else if h1 |> Rank.lt h2 then
                LT

            else if l1 |> Rank.gt l2 then
                GT

            else if l1 |> Rank.lt l2 then
                LT

            else
                EQ

        ( _, Suited _ _ ) ->
            LT

        ( Offsuit h1 l1, Offsuit h2 l2 ) ->
            if h1 |> Rank.gt h2 then
                GT

            else if h1 |> Rank.lt h2 then
                LT

            else if l1 |> Rank.gt l2 then
                GT

            else if l1 |> Rank.lt l2 then
                LT

            else
                EQ


toHandRanges : Hand -> Ranges
toHandRanges hand =
    case hand of
        Pair Rank.Ace ->
            PairPlus Rank.Ace

        Pair belowAce ->
            PairRange belowAce belowAce

        Suited h l ->
            if Rank.isConnected h l then
                SuitedPlus h l

            else
                SuitedRange h l l

        Offsuit h l ->
            if Rank.isConnected h l then
                OffsuitPlus h l

            else
                OffsuitRange h l l


magic : Hand -> Ranges -> List Ranges
magic hand ranges =
    case ( hand, ranges ) of
        ( Pair low, PairPlus high ) ->
            if Rank.isConnected high low then
                [ PairPlus low ]

            else
                [ toHandRanges hand, ranges ]

        ( Pair lowest, PairRange high low ) ->
            if Rank.isConnected low lowest then
                [ PairRange high lowest ]

            else
                [ toHandRanges hand, ranges ]

        ( Pair _, _ ) ->
            [ toHandRanges hand, ranges ]

        ( Suited high lowest, SuitedPlus otherHigh low ) ->
            if high == otherHigh && Rank.isConnected low lowest then
                [ SuitedPlus high lowest ]

            else
                [ toHandRanges hand, ranges ]

        ( Suited high lowest, SuitedRange otherHigh lowTo lowFrom ) ->
            if high == otherHigh && Rank.isConnected lowFrom lowest then
                [ SuitedRange high lowTo lowest ]

            else
                [ toHandRanges hand, ranges ]

        ( Suited _ _, _ ) ->
            [ toHandRanges hand, ranges ]

        ( Offsuit high lowest, OffsuitPlus otherHigh low ) ->
            if high == otherHigh && Rank.isConnected low lowest then
                [ OffsuitPlus high lowest ]

            else
                [ toHandRanges hand, ranges ]

        ( Offsuit high lowest, OffsuitRange otherHigh lowTo lowFrom ) ->
            if high == otherHigh && Rank.isConnected lowFrom lowest then
                [ OffsuitRange high lowTo lowest ]

            else
                [ toHandRanges hand, ranges ]

        ( Offsuit _ _, _ ) ->
            [ toHandRanges hand, ranges ]


pairs : List Hand
pairs =
    Rank.all |> List.map Pair


suitedAces : List Hand
suitedAces =
    Rank.all |> List.filter ((/=) Rank.Ace) |> List.map (Suited Rank.Ace)


offsuitAces : List Hand
offsuitAces =
    Rank.all |> List.filter ((/=) Rank.Ace) |> List.map (Offsuit Rank.Ace)


suitedBroadways : List Hand
suitedBroadways =
    [ Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Ten ]
        |> List.Extra.uniquePairs
        |> List.map (\( r1, r2 ) -> suited r1 r2)
        |> Maybe.Extra.values


offsuitBroadways : List Hand
offsuitBroadways =
    [ Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Ten ]
        |> List.Extra.uniquePairs
        |> List.map (\( r1, r2 ) -> offsuit r1 r2)
        |> Maybe.Extra.values
