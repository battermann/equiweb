module Poker.Range exposing (HandRange, best, combos, fromCombo, fromHand, isCombo, isHand, numberOfCombos, offsuitAces, offsuitBroadways, pairs, parseAndNormalize, percentage, suitedAces, suitedBroadways, toNormalizedString, toString)

import List.Extra
import Maybe.Extra
import Parser exposing ((|.), Parser)
import Poker.Combo as Combo exposing (Combo)
import Poker.Hand as Hand exposing (Hand)
import Poker.Ranges as Ranges exposing (Ranges(..))
import Result.Extra


type HandRange
    = Hand Hand
    | Combo Combo


fromHand : Hand -> HandRange
fromHand =
    Hand


fromCombo : Combo -> HandRange
fromCombo =
    Combo


toString : HandRange -> String
toString handRange =
    case handRange of
        Hand h ->
            Hand.toString h

        Combo c ->
            Combo.toString c


hand : HandRange -> Maybe Hand
hand handRange =
    case handRange of
        Hand h ->
            Just h

        Combo _ ->
            Nothing


combo : HandRange -> Maybe Combo
combo handRange =
    case handRange of
        Hand _ ->
            Nothing

        Combo c ->
            Just c


isHand : HandRange -> Bool
isHand h =
    case h of
        Hand _ ->
            True

        Combo _ ->
            False


isCombo : HandRange -> Bool
isCombo h =
    case h of
        Hand _ ->
            False

        Combo _ ->
            True


parser : Parser (List HandRange)
parser =
    Parser.oneOf
        [ Parser.backtrackable (Hand.parser |> Parser.map (List.map Hand))
        , Parser.backtrackable (Combo.parser |> Parser.map List.singleton |> Parser.map (List.map Combo))
        , Parser.end |> Parser.map (always [])
        ]


percentageParser : Parser (List HandRange)
percentageParser =
    Parser.oneOf
        [ Parser.int
            |. Parser.symbol "%"
            |. Parser.end
        , Parser.succeed 100
            |. Parser.symbol "random"
            |. Parser.end
        ]
        |> Parser.map (\p -> (p |> toFloat) / 100.0)
        |> Parser.map best


removeRedundantCombos : List HandRange -> List HandRange
removeRedundantCombos range =
    let
        uniqueHands =
            range |> List.map hand |> Maybe.Extra.values |> List.Extra.unique

        uniqueCombos =
            range |> List.map combo |> Maybe.Extra.values |> List.Extra.unique
    in
    (uniqueHands |> List.map Hand) ++ (uniqueCombos |> List.filter (\c -> uniqueHands |> List.any (\h -> Hand.combos h |> List.member c) |> not) |> List.map Combo)


parseAndNormalize : String -> Result (List String) (List HandRange)
parseAndNormalize rangeString =
    rangeString
        |> Parser.run percentageParser
        |> Result.Extra.orElse
            (rangeString
                |> String.split ","
                |> List.map (String.replace " " "")
                |> List.map (Parser.run parser)
                |> Result.Extra.combine
                |> Result.map (List.concat >> removeRedundantCombos)
                |> Result.map (List.sortWith order >> List.reverse)
            )
        |> Result.Extra.mapBoth (always [ "Range is not valid" ]) identity


order : HandRange -> HandRange -> Order
order hr1 hr2 =
    case ( hr1, hr2 ) of
        ( Hand h1, Hand h2 ) ->
            Hand.order h1 h2

        ( Hand _, _ ) ->
            GT

        ( _, Hand _ ) ->
            LT

        ( Combo c1, Combo c2 ) ->
            Combo.order c1 c2


magic : List HandRange -> List Ranges
magic =
    List.foldl fun []


fun : HandRange -> List Ranges -> List Ranges
fun hr ranges =
    case ranges of
        h :: t ->
            combine hr h ++ t

        [] ->
            [ handRangeToRanges hr ]


handRangeToRanges : HandRange -> Ranges
handRangeToRanges handRange =
    case handRange of
        Combo c ->
            SingleCombo c

        Hand h ->
            Hand.toHandRanges h


combine : HandRange -> Ranges -> List Ranges
combine handRange ranges =
    case handRange of
        Combo c ->
            [ SingleCombo c, ranges ]

        Hand h ->
            Hand.magic h ranges


toNormalizedString : List HandRange -> String
toNormalizedString =
    removeRedundantCombos >> List.sortWith order >> List.reverse >> magic >> List.reverse >> List.map Ranges.toString >> String.join ","


combos : HandRange -> List Combo
combos handRange =
    case handRange of
        Combo c ->
            [ c ]

        Hand h ->
            Hand.combos h


numberOfCombos : List HandRange -> Int
numberOfCombos =
    List.map (combos >> List.length) >> List.sum


percentage : List HandRange -> Float
percentage handRanges =
    (numberOfCombos handRanges |> toFloat) / toFloat Combo.total


best : Float -> List HandRange
best p =
    Hand.allRanked
        |> List.foldl
            (\h hr ->
                if (numberOfCombos hr |> toFloat) / toFloat Combo.total < p then
                    Hand h :: hr

                else
                    hr
            )
            []


pairs : List HandRange
pairs =
    Hand.pairs |> List.map Hand


suitedAces : List HandRange
suitedAces =
    Hand.suitedAces |> List.map Hand


offsuitAces : List HandRange
offsuitAces =
    Hand.offsuitAces |> List.map Hand


suitedBroadways : List HandRange
suitedBroadways =
    Hand.suitedBroadways |> List.map Hand


offsuitBroadways : List HandRange
offsuitBroadways =
    Hand.offsuitBroadways |> List.map Hand
