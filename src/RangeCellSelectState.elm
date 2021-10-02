module RangeCellSelectState exposing (RangeCellSelectState(..))


type RangeCellSelectState
    = Selected Int
    | PartiallySelected Int
    | NotSelected
    | MouseOver
    | MouseOverDuringSuitSelection Int Int Int
