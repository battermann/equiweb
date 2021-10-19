module Poker.Position exposing (Position(..), all, toString)


type Position
    = UTG
    | MP
    | CO
    | BU
    | SB
    | BB


all : List Position
all =
    [ UTG, MP, CO, BU, SB, BB ]


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
