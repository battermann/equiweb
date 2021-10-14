module Poker.Fuzzer exposing (combo, combos)

import Fuzz exposing (..)
import Poker.Combo as Combo exposing (Combo)
import Random
import Random.List
import Shrink
import Test exposing (..)


combos : Fuzzer (List Combo)
combos =
    Fuzz.custom (Random.List.shuffle Combo.all) Shrink.noShrink
        |> Fuzz.map2 List.take
            (Fuzz.intRange 1 Combo.total)


combo : Fuzzer Combo
combo =
    Fuzz.custom (Random.List.choose Combo.all |> Random.map Tuple.first) Shrink.noShrink
        |> Fuzz.map (Maybe.withDefault Combo.aceOfClubsAceOfspades)
