module Poker.CardRemoval exposing (blockedCombosForRangeSelection, numberOfCombos, unblocked)

import List.Extra
import Poker.Board as Board
import Poker.Card exposing (Card)
import Poker.Combo as Combo exposing (Combo)
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)


type alias Board =
    List Card


type alias Range =
    List Combo


{-| This is a heuristic optimization.
We want to make sure that the other ranges do not have blocking combos to a given combo.
With combo we mean a combination of two cards. E.g. AhJd.
For any given combo there are 50 other cards left in the deck. So if the given combo is AhJd e.g. it will be blocked by any AhX or any JdX combo.
AhX and JdX each have 50 different possibilities which makes 100 in total (disregarding that we count AhJd twice).
So if a range contains more than 100 combos it cannot block any single given combo.
If we consider 2 combos (which occurrs when we 2 ranges to validate against) we can disregard ranges with more than 2 x 100 = 200 combos.
So for n ranges we can always disregard ranges with more than n x 100 combos.
-}
removeBigRanges : List Range -> List Range
removeBigRanges ranges =
    let
        filteredRanges =
            ranges |> List.filter (\range -> (range |> List.length) <= (List.length ranges * 100))
    in
    if List.length ranges == List.length filteredRanges then
        filteredRanges

    else
        removeBigRanges filteredRanges


{-| We sort by length because if we consider the smaller ranges first, we will find the conflicting combos faster
-}
filteredByBoard : Board -> List Range -> List Range
filteredByBoard board ranges =
    ranges
        |> List.map (List.filter (\com -> board |> Board.blocks com |> not))
        |> List.Extra.filterNot List.isEmpty
        |> List.sortBy List.length
        |> removeBigRanges


unblocked : Board -> List Range -> Combo -> Bool
unblocked board ranges combo =
    if board |> Board.blocks combo then
        False

    else if ranges |> List.isEmpty then
        True

    else
        unblockedByRanges [ combo ] ranges


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


numberOfCombos : List Combo -> Board -> List (List HandOrCombo) -> Int
numberOfCombos combos board ranges =
    combos
        |> List.Extra.count
            (unblocked board
                (ranges
                    |> List.map (List.concatMap HandOrCombo.combos)
                    |> List.Extra.filterNot List.isEmpty
                    |> filteredByBoard board
                )
            )


blockedCombosForRangeSelection : Board -> List (List HandOrCombo) -> List Combo
blockedCombosForRangeSelection board ranges =
    Combo.all
        |> List.Extra.filterNot
            (unblocked board
                (ranges
                    |> List.map (List.concatMap HandOrCombo.combos)
                    |> List.Extra.filterNot List.isEmpty
                    |> filteredByBoard board
                )
            )
