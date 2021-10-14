module Poker.HandOrComboTests exposing (..)

import Expect
import Fuzz exposing (..)
import List.Extra
import Poker.Combo as Combo
import Poker.Fuzzer as Fuzzer
import Poker.HandOrCombo as HandOrCombo
import Test exposing (..)
import Test.Table as Table


rangeTests : Test
rangeTests =
    describe "range tests"
        [ test "combo order" <|
            \_ ->
                Expect.equal (HandOrCombo.parseAsCononicalHandsOrCombos "4s3s,4c2c" |> Result.map HandOrCombo.toNormalizedString) (Ok "4s3s,4c2c")
        , fuzz Fuzzer.combos "to normalized string and parsed to combos should be the same" <|
            \combos ->
                Expect.equal
                    (combos
                        |> List.map Combo.toString
                        |> String.join ","
                        |> HandOrCombo.parseAsCononicalHandsOrCombos
                        |> Result.map (List.concatMap HandOrCombo.combos)
                        |> Result.withDefault []
                        |> List.sortWith Combo.order
                    )
                    (combos |> List.sortWith Combo.order)
        ]


normalizedRangeNotationParsedAndNormalizedShouldBeSameAsOriginalTests : Test
normalizedRangeNotationParsedAndNormalizedShouldBeSameAsOriginalTests =
    let
        pairs =
            [ "AA"
            , "TT+"
            , "22+"
            , "KK-TT"
            , "TT+,66-22"
            ]

        suited =
            [ "AKs"
            , "ATs+"
            , "T7s+"
            , "J9s-J7s"
            , "ATs,64s-62s"
            ]

        offsuited =
            [ "QTo+"
            , "75o-72o"
            , "98o"
            , "AQo,T8o-T6o"
            ]

        combos =
            [ "9s7s,9d7d"
            , "3c2s,3c2h"
            , "4s3s,4c2c"
            ]
    in
    Table.testTable "Normalized ranges parsed and normalized should be same as original"
        (pairs ++ suited ++ offsuited ++ combos ++ (List.Extra.cartesianProduct [ pairs, suited, offsuited, combos ] |> List.map (String.join ",")))
        (\strRange -> Expect.equal (HandOrCombo.parseAsCononicalHandsOrCombos strRange |> Result.map HandOrCombo.toNormalizedString) (Ok strRange))
