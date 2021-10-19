module Poker.SuitTests exposing (..)

import Expect
import Fuzz exposing (..)
import Parser
import Poker.Suit as Suit
import Result.Extra
import Test exposing (..)


suitTests : Test
suitTests =
    describe "parse suit"
        [ fuzz char "should be a suit if one of csdhCSDH" <|
            \c ->
                if "csdhCSDH" |> String.toList |> List.member c then
                    Expect.equal (Parser.run Suit.parser (String.fromChar c) |> Result.Extra.isOk) True

                else
                    Expect.equal (Parser.run Suit.parser (String.fromChar c) |> Result.Extra.isOk) False
        ]
