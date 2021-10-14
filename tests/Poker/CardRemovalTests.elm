module Poker.CardRemovalTests exposing (..)

import Expect
import Fuzz exposing (..)
import Poker.Card as Card exposing (Card)
import Poker.CardRemoval as CardRemoval
import Poker.Combo as Combo exposing (Combo)
import Poker.Fuzzer as Fuzzer
import Poker.HandOrCombo as HandOrCombo
import Poker.Rank as Rank
import Poker.Suit as Suit
import Test exposing (..)


cardRemovalTests : Test
cardRemovalTests =
    describe "card removal tests"
        [ fuzz Fuzzer.combo "empty board and ranges always valid" <|
            \combo ->
                Expect.true "should always return true" (CardRemoval.unblocked combo [] [])
        , fuzz Fuzzer.combo "ranges with all combos always valid" <|
            \combo ->
                Expect.true "should always return true" (CardRemoval.unblocked combo [] [ Combo.all, Combo.all ])
        , fuzz Fuzzer.combo "ranges with single same combo always invalid" <|
            \combo ->
                Expect.false "should always return false" (CardRemoval.unblocked combo [] [ Combo.all, [ combo ] ])
        , fuzz Fuzzer.combo "board blocks combo always invalid" <|
            \combo ->
                Expect.false "should always return false"
                    (CardRemoval.unblocked combo [ Combo.fst combo, Card Rank.Ace Suit.Club, Card Rank.King Suit.Heart ] [])
        , test "AhJh valid" <|
            \_ ->
                case Combo.combo (Card Rank.Ace Suit.Heart) (Card Rank.Jack Suit.Heart) of
                    Just combo ->
                        Expect.true "should return true"
                            (CardRemoval.unblocked combo
                                []
                                ([ "AhKd,AdKd,2h2d", "AdKd,Ad3d" ] |> List.map makeCombos)
                            )

                    Nothing ->
                        Expect.fail "should not happen"
        , test "AhJh invalid" <|
            \_ ->
                case Combo.combo (Card Rank.Ace Suit.Heart) (Card Rank.Jack Suit.Heart) of
                    Just combo ->
                        Expect.false "should return false"
                            (CardRemoval.unblocked combo
                                []
                                ([ "AhKd,AdKd", "AdKd,Ad3d" ] |> List.map makeCombos)
                            )

                    Nothing ->
                        Expect.fail "should not happen"
        ]


makeCombos : String -> List Combo
makeCombos =
    HandOrCombo.parseAsCononicalHandsOrCombos >> Result.map (List.concatMap HandOrCombo.combos) >> Result.withDefault []
