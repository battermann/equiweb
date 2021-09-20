module Poker.Range exposing (isCombo, isHand, rewrite, toString)

import List.Extra
import Maybe.Extra
import Parser exposing (Parser)
import Poker.Combo as Combo exposing (Combo)
import Poker.Hand as Hand exposing (Hand)
import Result.Extra


type HandRange
    = Hand Hand
    | Combo Combo


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


removeRedundantCombos : List HandRange -> List HandRange
removeRedundantCombos range =
    let
        uniqueHands =
            range |> List.map hand |> Maybe.Extra.values |> List.Extra.unique

        uniqueCombos =
            range |> List.map combo |> Maybe.Extra.values |> List.Extra.unique
    in
    (uniqueHands |> List.map Hand) ++ (uniqueCombos |> List.filter (\c -> uniqueHands |> List.any (\h -> Hand.combos h |> List.member c) |> not) |> List.map Combo)


rewrite : String -> Result (List String) String
rewrite rangeString =
    rangeString
        |> String.split ","
        |> List.map (String.replace " " "")
        |> List.map (Parser.run parser)
        |> Result.Extra.combine
        |> Result.map (List.concat >> removeRedundantCombos >> List.map toString >> List.intersperse "," >> String.concat)
        |> Result.Extra.mapBoth (always [ "Range is not valid" ]) identity
