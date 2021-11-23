module Poker.CardRemovalTests exposing (..)

import Expect
import List.Extra
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
                Expect.true "should always return true" (CardRemoval.unblocked [] [] combo)
        , fuzz Fuzzer.combo "ranges with all combos always valid" <|
            \combo ->
                Expect.true "should always return true" (CardRemoval.unblocked [] [ Combo.all, Combo.all ] combo)
        , fuzz Fuzzer.combo "ranges with single same combo always invalid" <|
            \combo ->
                Expect.false "should always return false" (CardRemoval.unblocked [] [ Combo.all, [ combo ] ] combo)
        , fuzz Fuzzer.combo "board blocks combo always invalid" <|
            \combo ->
                Expect.false "should always return false"
                    (CardRemoval.unblocked [ Combo.fst combo, Card Rank.Ace Suit.Clubs, Card Rank.King Suit.Hearts ] [] combo)
        , test "AhJh valid" <|
            \_ ->
                case Combo.combo (Card Rank.Ace Suit.Hearts) (Card Rank.Jack Suit.Hearts) of
                    Just combo ->
                        Expect.true "should return true"
                            (CardRemoval.unblocked
                                []
                                ([ "AhKd,AdKd,2h2d", "AdKd,Ad3d" ] |> List.map makeCombos)
                                combo
                            )

                    Nothing ->
                        Expect.fail "should not happen"
        , test "AhJh invalid" <|
            \_ ->
                case Combo.combo (Card Rank.Ace Suit.Hearts) (Card Rank.Jack Suit.Hearts) of
                    Just combo ->
                        Expect.false "should return false"
                            (CardRemoval.unblocked
                                []
                                ([ "AhKd,AdKd", "AdKd,Ad3d" ] |> List.map makeCombos)
                                combo
                            )

                    Nothing ->
                        Expect.fail "should not happen"
        , test "no card blocked if ranges are empty" <|
            \_ ->
                let
                    actual =
                        CardRemoval.unblockedCards [] Card.all

                    expected =
                        Card.all
                in
                Expect.equal actual expected
        , test "Ah and Kd should be blocked" <|
            \_ ->
                let
                    actual =
                        CardRemoval.unblockedCards ([ "AhKd" ] |> List.map (makeCombos >> List.map HandOrCombo.fromCombo)) Card.all |> List.sortWith Card.order

                    expected =
                        Card.all
                            |> List.Extra.filterNot (\card -> card == Card Rank.Ace Suit.Hearts || card == Card Rank.King Suit.Diamonds)
                            |> List.sortWith Card.order
                in
                Expect.equal actual expected
        , test "No cards blocked if range contains different combos" <|
            \_ ->
                let
                    actual =
                        CardRemoval.unblockedCards ([ "AhKd,AdKh" ] |> List.map (makeCombos >> List.map HandOrCombo.fromCombo)) Card.all |> List.sortWith Card.order

                    expected =
                        Card.all |> List.sortWith Card.order
                in
                Expect.equal actual expected
        , test "Ah and Kd should be blocked because all (more than one) combos contain them" <|
            \_ ->
                let
                    actual =
                        CardRemoval.unblockedCards ([ "Ah2d,Ah3h", "Kh2h,Kd2h" ] |> List.map (makeCombos >> List.map HandOrCombo.fromCombo)) Card.all |> List.sortWith Card.order

                    expected =
                        Card.all
                            |> List.Extra.filterNot (\card -> card == Card Rank.Ace Suit.Hearts || card == Card Rank.Two Suit.Hearts)
                            |> List.sortWith Card.order
                in
                Expect.equal actual expected
        , test "transitive blocking" <|
            \_ ->
                let
                    actual =
                        CardRemoval.unblockedCards ([ "Ah5h,Kh2h", "AhTs" ] |> List.map (makeCombos >> List.map HandOrCombo.fromCombo)) Card.all |> List.sortWith Card.order

                    expected =
                        Card.all
                            |> List.Extra.filterNot
                                (\card ->
                                    card
                                        == Card Rank.Ace Suit.Hearts
                                        || card
                                        == Card Rank.Ten Suit.Spades
                                        || card
                                        == Card Rank.King Suit.Hearts
                                        || card
                                        == Card Rank.Two Suit.Hearts
                                )
                            |> List.sortWith Card.order
                in
                Expect.equal actual expected
        ]


makeCombos : String -> List Combo
makeCombos =
    HandOrCombo.parseAsCononicalHandsOrCombos >> Result.map (List.concatMap HandOrCombo.combos) >> Result.withDefault []
