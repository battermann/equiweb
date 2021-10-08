module View.RangeCellSelectState exposing (RangeCellSelectState(..))

-- move to view


type RangeCellSelectState
    = Selected Int
    | PartiallySelected Int
    | NotSelected
    | MouseOver
    | MouseOverDuringSuitSelection Int Int Int
