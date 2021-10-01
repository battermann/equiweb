module Poker.RangeTests exposing (..)

import Expect
import Fuzz exposing (..)
import List.Extra
import Poker.Combo as Combo exposing (Combo)
import Poker.Range as Range
import Random.List
import Shrink
import Test exposing (..)
import Test.Table as Table


rangeTests : Test
rangeTests =
    describe "range tests"
        [ normalizedRangesParsedAndNormalizedShouldBeSameAsOriginal
        , fuzz combosFuzzer "to normalized string and parsed to combos should be the same" <|
            \combos ->
                Expect.equal
                    (combos
                        |> List.map Combo.toString
                        |> String.join ","
                        |> Range.parseAndNormalize
                        |> Result.map (List.concatMap Range.combos)
                        |> Result.withDefault []
                        |> List.sortWith Combo.order
                    )
                    (combos |> List.sortWith Combo.order)
        ]


normalizedRangesParsedAndNormalizedShouldBeSameAsOriginal =
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
            ]
    in
    Table.testTable "Normalized ranges parsed and normalized should be same as original"
        (pairs ++ suited ++ offsuited ++ combos ++ (List.Extra.cartesianProduct [ pairs, suited, offsuited, combos ] |> List.map (String.join ",")))
        (\strRange -> Expect.equal (Range.parseAndNormalize strRange |> Result.map Range.toNormalizedString) (Ok strRange))


combosFuzzer : Fuzzer (List Combo)
combosFuzzer =
    Fuzz.custom (Random.List.shuffle Combo.all) Shrink.noShrink
        |> Fuzz.map2 List.take
            (Fuzz.intRange 1 Combo.total)
