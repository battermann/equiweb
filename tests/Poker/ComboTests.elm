module Poker.ComboTests exposing (..)

import Expect
import Fuzz exposing (..)
import List.Extra
import Parser
import Poker.Card exposing (Card)
import Poker.Combo as Combo
import Poker.Rank as Rank
import Poker.Suit as Suit
import Test exposing (..)


comboTests : Test
comboTests =
    describe "combo tests"
        [ test "combo ctor higher card first" <|
            \_ ->
                Expect.equal (Combo.combo (Card Rank.Two Suit.Clubs) (Card Rank.Ten Suit.Spades) |> Maybe.map Combo.toString) (Just "Ts2c")
        , test "combo ctor higher suit first" <|
            \_ ->
                Expect.equal (Combo.combo (Card Rank.Ten Suit.Spades) (Card Rank.Ten Suit.Clubs) |> Maybe.map Combo.toString) (Just "TcTs")
        , test "1326 possible combinations of two hole cards" <|
            \_ ->
                Expect.equal (Combo.all |> List.length) 1326
        , test "1326 distinct possible combinations of two hole cards" <|
            \_ ->
                Expect.equal (Combo.all |> List.Extra.unique |> List.length) 1326
        , test "combo parser" <|
            \_ ->
                Expect.equal (Parser.run Combo.parser "TsTc" |> Result.toMaybe) (Combo.combo (Card Rank.Ten Suit.Spades) (Card Rank.Ten Suit.Clubs))
        , test "combo serde roundtrip" <|
            \_ ->
                Expect.equal (Combo.all |> List.map (Combo.toString >> Parser.run Combo.parser)) (Combo.all |> List.map Ok)
        ]
