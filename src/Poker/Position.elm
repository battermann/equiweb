module Poker.Position exposing (Position(..), toString)


type Position
    = UTG
    | MP
    | CO
    | BU
    | SB
    | BB


toString : Position -> String
toString position =
    case position of
        UTG ->
            "UTG"

        MP ->
            "MP"

        CO ->
            "CO"

        BU ->
            "BU"

        SB ->
            "SB"

        BB ->
            "BB"
