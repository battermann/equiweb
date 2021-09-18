module Poker.HandTests exposing (..)

import Expect
import Fuzz exposing (..)
import List.Extra
import Maybe.Extra
import Parser
import Poker.Card as Card exposing (Card)
import Poker.Combo as Combo
import Poker.Hand as Hand
import Poker.Rank as Rank
import Poker.Suit as Suit
import Result.Extra
import Test exposing (..)


handTests : Test
handTests =
    describe "hand tests"
        [ test "parse AA" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "AA") (Ok <| [ Hand.pair Rank.Ace ])
        , test "parse suited hand" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "KTs" |> Result.toMaybe) (Hand.suited Rank.King Rank.Ten |> Maybe.map List.singleton)
        , test "parse offsuit hand" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "KTo" |> Result.toMaybe) (Hand.offsuit Rank.King Rank.Ten |> Maybe.map List.singleton)
        , test "parse hand" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "KT" |> Result.withDefault [])
                    ([ Hand.suited Rank.King Rank.Ten
                     , Hand.offsuit Rank.King Rank.Ten
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse suited hand range from to" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "KTs-K7s" |> Result.withDefault [])
                    ([ Hand.suited Rank.King Rank.Ten
                     , Hand.suited Rank.King Rank.Nine
                     , Hand.suited Rank.King Rank.Eight
                     , Hand.suited Rank.King Rank.Seven
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse offsuit hand range from to" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "A2o-A5o" |> Result.withDefault [])
                    ([ Hand.offsuit Rank.Ace Rank.Five
                     , Hand.offsuit Rank.Ace Rank.Four
                     , Hand.offsuit Rank.Ace Rank.Three
                     , Hand.offsuit Rank.Ace Rank.Two
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse non pairs hand range from to" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "74-57" |> Result.withDefault [])
                    ([ Hand.suited Rank.Seven Rank.Five
                     , Hand.suited Rank.Seven Rank.Four
                     , Hand.offsuit Rank.Seven Rank.Five
                     , Hand.offsuit Rank.Seven Rank.Four
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse pairs range from to" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "77-TT" |> Result.withDefault [])
                    [ Hand.pair Rank.Ten
                    , Hand.pair Rank.Nine
                    , Hand.pair Rank.Eight
                    , Hand.pair Rank.Seven
                    ]
        , test "parse suited hand range plus" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "KTs+" |> Result.withDefault [])
                    ([ Hand.suited Rank.King Rank.Queen
                     , Hand.suited Rank.King Rank.Jack
                     , Hand.suited Rank.King Rank.Ten
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse offsuit hand range plus" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "8To+" |> Result.withDefault [])
                    ([ Hand.offsuit Rank.Ten Rank.Nine
                     , Hand.offsuit Rank.Ten Rank.Eight
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse non pairs hand range plus" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "QT+" |> Result.withDefault [])
                    ([ Hand.suited Rank.Queen Rank.Jack
                     , Hand.suited Rank.Queen Rank.Ten
                     , Hand.offsuit Rank.Queen Rank.Jack
                     , Hand.offsuit Rank.Queen Rank.Ten
                     ]
                        |> Maybe.Extra.values
                    )
        , test "parse pairs hand range plus" <|
            \_ ->
                Expect.equal (Parser.run Hand.parser "TT+" |> Result.withDefault [])
                    [ Hand.pair Rank.Ace
                    , Hand.pair Rank.King
                    , Hand.pair Rank.Queen
                    , Hand.pair Rank.Jack
                    , Hand.pair Rank.Ten
                    ]
        ]
