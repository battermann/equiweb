module Poker.RankTests exposing (..)

import Expect
import Fuzz exposing (..)
import Parser
import Poker.Rank as Rank
import Result.Extra
import Test exposing (..)


rankTests : Test
rankTests =
    describe "parse rank"
        [ fuzz char "should be a rank if one of 23456789TJQKA" <|
            \c ->
                if "23456789TJQKAtjqka" |> String.toList |> List.member c then
                    Expect.equal (Parser.run Rank.parser (String.fromChar c) |> Result.Extra.isOk) True

                else
                    Expect.equal (Parser.run Rank.parser (String.fromChar c) |> Result.Extra.isOk) False
        ]
