module Poker.CardTests exposing (..)

import Expect
import Fuzz exposing (..)
import List.Extra
import Parser
import Poker.Card as Card
import Test exposing (..)
import Util.NonEmptyList


cardTests : Test
cardTests =
    describe "card tests"
        [ test "there should be 52 cards" <|
            \_ ->
                Expect.equal (Card.all |> List.length) 52
        , test "there should be 52 unique cards" <|
            \_ ->
                Expect.equal (Card.all |> List.Extra.unique |> List.length) 52
        , test "cards serde roundtrip" <|
            \_ -> Expect.equal (Card.all |> List.map (Card.toString >> Parser.run Card.parser)) (Card.all |> List.map Ok)
        , test "all should be the same as allNonEmpty" <|
            \_ -> Expect.equal Card.all (Card.allNonEmpty |> Util.NonEmptyList.toList)
        ]
