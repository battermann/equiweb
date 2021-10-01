module RangeCellSelectState exposing (RangeCellSelectState(..))


type RangeCellSelectState
    = Selected
    | PartiallySelected Int
    | NotSelected
    | MouseOver
