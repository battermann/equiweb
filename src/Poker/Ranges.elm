module Poker.Ranges exposing (Ranges(..), toString)

import Poker.Combo as Combo exposing (Combo)
import Poker.Rank as Rank exposing (Rank)


type Ranges
    = PairPlus Rank
    | PairRange Rank Rank
    | SuitedPlus Rank Rank
    | SuitedRange Rank Rank Rank
    | OffsuitPlus Rank Rank
    | OffsuitRange Rank Rank Rank
    | SingleCombo Combo


toString : Ranges -> String
toString ranges =
    case ranges of
        PairPlus Rank.Ace ->
            "AA"

        PairPlus rank ->
            Rank.toString rank ++ Rank.toString rank ++ "+"

        PairRange high low ->
            if high == low then
                Rank.toString high ++ Rank.toString high

            else
                Rank.toString high ++ Rank.toString high ++ "-" ++ Rank.toString low ++ Rank.toString low

        SuitedPlus high low ->
            if Rank.isConnected high low then
                Rank.toString high ++ Rank.toString low ++ "s"

            else
                Rank.toString high ++ Rank.toString low ++ "s+"

        SuitedRange high lowTo lowFrom ->
            if lowTo == lowFrom then
                Rank.toString high ++ Rank.toString lowFrom ++ "s"

            else
                Rank.toString high ++ Rank.toString lowTo ++ "s-" ++ Rank.toString high ++ Rank.toString lowFrom ++ "s"

        OffsuitPlus high low ->
            if Rank.isConnected high low then
                Rank.toString high ++ Rank.toString low ++ "o"

            else
                Rank.toString high ++ Rank.toString low ++ "o+"

        OffsuitRange high lowTo lowFrom ->
            if lowTo == lowFrom then
                Rank.toString high ++ Rank.toString lowFrom ++ "o"

            else
                Rank.toString high ++ Rank.toString lowTo ++ "o-" ++ Rank.toString high ++ Rank.toString lowFrom ++ "o"

        SingleCombo combo ->
            Combo.toString combo
