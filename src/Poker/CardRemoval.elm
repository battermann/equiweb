module Poker.CardRemoval exposing (unblocked)

import Poker.Board as Board
import Poker.Card exposing (Card)
import Poker.Combo as Combo exposing (Combo)


type alias Board =
    List Card


type alias Range =
    List Combo


unblocked : Combo -> Board -> List Range -> Bool
unblocked combo board ranges =
    if board |> Board.blocks combo then
        False

    else if ranges |> List.isEmpty then
        True

    else
        unblockedByRanges [ combo ] (ranges |> List.map (\range -> range |> List.filter (\com -> board |> Board.blocks com |> not)))


unblockedByRanges : List Combo -> List Range -> Bool
unblockedByRanges combos ranges =
    case ranges of
        [] ->
            True

        currentRange :: remainingRanges ->
            case currentRange |> firstUnBlocked combos of
                Just ( combo, remainingCombos ) ->
                    unblockedByRanges (combo :: combos) remainingRanges
                        || unblockedByRanges combos (remainingCombos :: remainingRanges)

                Nothing ->
                    False


firstUnBlocked : List Combo -> List Combo -> Maybe ( Combo, List Combo )
firstUnBlocked combos ranges =
    case ranges of
        [] ->
            Nothing

        headCombo :: tailCombos ->
            if combos |> List.any (Combo.hasBlocker headCombo) |> not then
                Just ( headCombo, tailCombos )

            else
                firstUnBlocked combos tailCombos
