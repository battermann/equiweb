module Poker.HandOrCombo exposing
    ( HandOrCombo
    , best
    , combos
    , fromCombo
    , fromHand
    , isCombo
    , isHand
    , numberOfCombos
    , offsuitAces
    , offsuitBroadways
    , pairs
    , parseAsCononicalHandsOrCombos
    , percentage
    , range
    , suitedAces
    , suitedBroadways
    , toNormalizedString
    , toString
    )

import Maybe.Extra
import Parser exposing ((|.), Parser)
import Poker.Combo as Combo exposing (Combo)
import Poker.Hand as Hand exposing (Hand)
import Poker.RangeNotation as RangeNotation exposing (RangeNotation(..))
import Result.Extra


type HandOrCombo
    = Hand Hand
    | Combo Combo


fromHand : Hand -> HandOrCombo
fromHand =
    Hand


fromCombo : Combo -> HandOrCombo
fromCombo =
    Combo


toString : HandOrCombo -> String
toString handOrCombo =
    case handOrCombo of
        Hand h ->
            Hand.toString h

        Combo c ->
            Combo.toString c


isHand : HandOrCombo -> Bool
isHand h =
    case h of
        Hand _ ->
            True

        Combo _ ->
            False


isCombo : HandOrCombo -> Bool
isCombo h =
    case h of
        Hand _ ->
            False

        Combo _ ->
            True


parser : Parser (List HandOrCombo)
parser =
    Parser.oneOf
        [ Parser.backtrackable (Hand.parser |> Parser.map (List.map Hand))
        , Parser.backtrackable (Combo.parser |> Parser.map List.singleton |> Parser.map (List.map Combo))
        , Parser.end |> Parser.map (always [])
        ]


percentageParser : Parser (List HandOrCombo)
percentageParser =
    Parser.oneOf
        [ Parser.int
            |. Parser.symbol "%"
            |. Parser.end
        , Parser.succeed 100
            |. Parser.symbol "random"
            |. Parser.end
        , Parser.succeed 100
            |. Parser.symbol "Random"
            |. Parser.end
        ]
        |> Parser.map (\p -> (p |> toFloat) / 100.0)
        |> Parser.map best


parseAsCononicalHandsOrCombos : String -> Result (List String) (List HandOrCombo)
parseAsCononicalHandsOrCombos rangeString =
    rangeString
        |> Parser.run percentageParser
        |> Result.Extra.orElse
            (rangeString
                |> String.split ","
                |> List.map (String.replace " " "")
                |> List.map (Parser.run parser)
                |> Result.Extra.combine
                |> Result.map (List.concat >> toCanonicalHandsOrCombos)
            )
        |> Result.Extra.mapBoth (always [ "The range is not valid" ]) identity


order : HandOrCombo -> HandOrCombo -> Order
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


toRangeNotation : List HandOrCombo -> List RangeNotation
toRangeNotation =
    List.sortWith (reverseOrder order) >> List.foldl combineHandOrCombo []


reverseOrder : (a -> a -> Order) -> a -> a -> Order
reverseOrder ord a b =
    case ord a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


combineHandOrCombo : HandOrCombo -> List RangeNotation -> List RangeNotation
combineHandOrCombo hr ranges =
    case ranges of
        h :: t ->
            combine hr h ++ t

        [] ->
            [ handOrComboToRangeNotation hr ]


handOrComboToRangeNotation : HandOrCombo -> RangeNotation
handOrComboToRangeNotation handOrCombo =
    case handOrCombo of
        Combo c ->
            SingleCombo c

        Hand h ->
            Hand.toRangeNotation h


combine : HandOrCombo -> RangeNotation -> List RangeNotation
combine handOrCombo ranges =
    case handOrCombo of
        Combo c ->
            [ SingleCombo c, ranges ]

        Hand h ->
            Hand.combine h ranges


toNormalizedString : List HandOrCombo -> String
toNormalizedString =
    toCanonicalHandsOrCombos >> toRangeNotation >> List.reverse >> List.map RangeNotation.toString >> String.join ","


combos : HandOrCombo -> List Combo
combos handOrCombo =
    case handOrCombo of
        Combo c ->
            [ c ]

        Hand h ->
            Hand.combos h


numberOfCombos : List HandOrCombo -> Int
numberOfCombos =
    List.map (combos >> List.length) >> List.sum


percentage : List HandOrCombo -> Float
percentage handOrCombos =
    (numberOfCombos handOrCombos |> toFloat) / toFloat Combo.total


best : Float -> List HandOrCombo
best p =
    Hand.allWithAccumulatedNumberOfCombosOrderedByRank
        |> List.filterMap
            (\( h, _, accP ) ->
                if p >= accP then
                    Just (Hand h)

                else
                    Nothing
            )


range : Float -> Float -> List HandOrCombo
range vpip pfr =
    best vpip |> List.filter (\hr -> best pfr |> List.member hr |> not)


pairs : List HandOrCombo
pairs =
    Hand.pairs |> List.map Hand


suitedAces : List HandOrCombo
suitedAces =
    Hand.suitedAces |> List.map Hand


offsuitAces : List HandOrCombo
offsuitAces =
    Hand.offsuitAces |> List.map Hand


suitedBroadways : List HandOrCombo
suitedBroadways =
    Hand.suitedBroadways |> List.map Hand


offsuitBroadways : List HandOrCombo
offsuitBroadways =
    Hand.offsuitBroadways |> List.map Hand


toCanonicalHandsOrCombos : List HandOrCombo -> List HandOrCombo
toCanonicalHandsOrCombos =
    List.concatMap combos >> combosToHandOrCombo


combosToHandOrCombo : List Combo -> List HandOrCombo
combosToHandOrCombo cs =
    let
        onPair r =
            let
                ps =
                    cs |> Combo.getPairs r
            in
            if List.length ps == 6 then
                [ Hand (Hand.pair r) ]

            else
                ps |> List.map Combo

        onSuited r1 r2 =
            let
                suitedCombos =
                    cs |> Combo.getSuited r1 r2
            in
            if List.length suitedCombos == 4 then
                Hand.suited r1 r2 |> Maybe.Extra.unwrap [] (Hand >> List.singleton)

            else
                suitedCombos |> List.map Combo

        onOffsuit r1 r2 =
            let
                offsuitCombos =
                    cs |> Combo.getOffsuit r1 r2
            in
            if List.length offsuitCombos == 12 then
                Hand.offsuit r1 r2 |> Maybe.Extra.unwrap [] (Hand >> List.singleton)

            else
                offsuitCombos |> List.map Combo
    in
    Hand.grid
        |> List.concat
        |> List.concatMap (Hand.fold onPair onSuited onOffsuit)
