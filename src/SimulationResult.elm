module SimulationResult exposing (ResultLine, SimulationResult)

import Poker.Card exposing (Card)
import Poker.Range exposing (HandRange)


type alias ResultLine =
    { range : List HandRange
    , equity : Float
    }


type alias SimulationResult =
    { board : List Card
    , utg : Maybe ResultLine
    , mp : Maybe ResultLine
    , co : Maybe ResultLine
    , bu : Maybe ResultLine
    , sb : Maybe ResultLine
    , bb : Maybe ResultLine
    }
