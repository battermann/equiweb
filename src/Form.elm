module Form exposing
    ( RangesForm
    , allRanges
    , allRangesExcept
    , board
    , boardField
    , clearBoard
    , clearRange
    , initialForm
    , numberOfCombos
    , range
    , rangeField
    , ranges
    , rewritable
    , rewrite
    , rewriteBoard
    , setBoard
    , setRange
    , updateNumberOfCombos
    , validateForm
    )

import Form.Field exposing (Field)
import List.Extra
import Maybe.Extra
import Poker.Board as Board
import Poker.Card as Card exposing (Card)
import Poker.CardRemoval as CardRemoval exposing (numberOfCombos)
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position as Position exposing (Position(..))
import Result.Extra


type alias NumberOfCombos =
    { utg : Int
    , mp : Int
    , co : Int
    , bu : Int
    , sb : Int
    , bb : Int
    }


initialNumberOfCombos : NumberOfCombos
initialNumberOfCombos =
    { utg = 0
    , mp = 0
    , co = 0
    , bu = 0
    , sb = 0
    , bb = 0
    }


type RangesForm
    = RangesForm
        { utg : Field (List HandOrCombo)
        , mp : Field (List HandOrCombo)
        , co : Field (List HandOrCombo)
        , bu : Field (List HandOrCombo)
        , sb : Field (List HandOrCombo)
        , bb : Field (List HandOrCombo)
        , board : Field (List Card)
        , numberOfCombos : NumberOfCombos
        }


board : RangesForm -> List Card
board (RangesForm form) =
    form.board.validated |> Result.withDefault []


setBoard : String -> RangesForm -> RangesForm
setBoard b (RangesForm form) =
    RangesForm { form | board = form.board |> Form.Field.setValue Board.validate b }


setRange : Position -> String -> RangesForm -> RangesForm
setRange position r (RangesForm form) =
    case position of
        UTG ->
            RangesForm { form | utg = form.utg |> Form.Field.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        MP ->
            RangesForm { form | mp = form.mp |> Form.Field.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        CO ->
            RangesForm { form | co = form.co |> Form.Field.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        BU ->
            RangesForm { form | bu = form.bu |> Form.Field.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        SB ->
            RangesForm { form | sb = form.sb |> Form.Field.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        BB ->
            RangesForm { form | bb = form.bb |> Form.Field.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }


initialForm : RangesForm
initialForm =
    RangesForm
        { utg = { name = "UTG", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
        , mp = { name = "MP", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
        , co = { name = "CO", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
        , bu = { name = "BU", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
        , sb = { name = "SB", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
        , bb = { name = "BB", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
        , board = { name = "Board", value = "", validated = Ok [], edited = False }
        , numberOfCombos = initialNumberOfCombos
        }


rewritable : Field (List HandOrCombo) -> Bool
rewritable field =
    field.value
        /= (field.validated |> Result.withDefault [] |> HandOrCombo.toNormalizedString)
        && (field.validated |> Result.Extra.isOk)


rangeField : Position -> RangesForm -> Field (List HandOrCombo)
rangeField position (RangesForm form) =
    case position of
        UTG ->
            form.utg

        MP ->
            form.mp

        CO ->
            form.co

        BU ->
            form.bu

        SB ->
            form.sb

        BB ->
            form.bb


boardField : RangesForm -> Field (List Card)
boardField (RangesForm form) =
    form.board


range : Position -> RangesForm -> List HandOrCombo
range position (RangesForm form) =
    case position of
        UTG ->
            form.utg.validated |> Result.withDefault []

        MP ->
            form.mp.validated |> Result.withDefault []

        CO ->
            form.co.validated |> Result.withDefault []

        BU ->
            form.bu.validated |> Result.withDefault []

        SB ->
            form.sb.validated |> Result.withDefault []

        BB ->
            form.bb.validated |> Result.withDefault []


allRangesExcept : Position -> RangesForm -> List (List HandOrCombo)
allRangesExcept position form =
    Position.all
        |> List.Extra.filterNot ((==) position)
        |> List.map (\p -> range p form)


allRanges : RangesForm -> List (List HandOrCombo)
allRanges form =
    Position.all
        |> List.map (\p -> range p form)


rewriteBoard : RangesForm -> RangesForm
rewriteBoard (RangesForm form) =
    RangesForm { form | board = Form.Field.rewrite form.board (List.map Card.toString >> String.concat) }


rewrite : Position -> RangesForm -> RangesForm
rewrite position (RangesForm form) =
    case position of
        UTG ->
            RangesForm { form | utg = Form.Field.rewrite form.utg HandOrCombo.toNormalizedString }

        MP ->
            RangesForm { form | mp = Form.Field.rewrite form.mp HandOrCombo.toNormalizedString }

        CO ->
            RangesForm { form | co = Form.Field.rewrite form.co HandOrCombo.toNormalizedString }

        BU ->
            RangesForm { form | bu = Form.Field.rewrite form.bu HandOrCombo.toNormalizedString }

        SB ->
            RangesForm { form | sb = Form.Field.rewrite form.sb HandOrCombo.toNormalizedString }

        BB ->
            RangesForm { form | bb = Form.Field.rewrite form.bb HandOrCombo.toNormalizedString }


validateForm : RangesForm -> Result (List String) RangesForm
validateForm (RangesForm form) =
    Ok (\_ _ _ _ _ _ _ -> RangesForm form)
        |> Form.Field.apply (form.utg.validated |> Result.Extra.mapBoth (always [ "The UTG range is not a valid range" ]) identity)
        |> Form.Field.apply (form.mp.validated |> Result.Extra.mapBoth (always [ "The MP range is not a valid range" ]) identity)
        |> Form.Field.apply (form.co.validated |> Result.Extra.mapBoth (always [ "The CO range is not a valid range" ]) identity)
        |> Form.Field.apply (form.bu.validated |> Result.Extra.mapBoth (always [ "The BU range is not a valid range" ]) identity)
        |> Form.Field.apply (form.sb.validated |> Result.Extra.mapBoth (always [ "The SB range is not a valid range" ]) identity)
        |> Form.Field.apply (form.bb.validated |> Result.Extra.mapBoth (always [ "The BB range is not a valid range" ]) identity)
        |> Form.Field.apply (form.board.validated |> Result.Extra.mapBoth (always [ "The board is not a valid board" ]) identity)
        |> Result.Extra.filter [ "The ranges are not valid due to conflicting card removal effects" ] noConflictingCardRemoval


noConflictingCardRemoval : RangesForm -> Bool
noConflictingCardRemoval rangesForm =
    let
        (RangesForm form) =
            updateNumberOfCombos rangesForm
    in
    (String.isEmpty form.utg.value || form.numberOfCombos.utg > 0)
        && (String.isEmpty form.mp.value || form.numberOfCombos.mp > 0)
        && (String.isEmpty form.co.value || form.numberOfCombos.co > 0)
        && (String.isEmpty form.bu.value || form.numberOfCombos.bu > 0)
        && (String.isEmpty form.sb.value || form.numberOfCombos.sb > 0)
        && (String.isEmpty form.bb.value || form.numberOfCombos.bb > 0)


ranges : RangesForm -> List (List HandOrCombo)
ranges form =
    [ (rangeField UTG form).validated
    , (rangeField MP form).validated
    , (rangeField CO form).validated
    , (rangeField BU form).validated
    , (rangeField SB form).validated
    , (rangeField BB form).validated
    ]
        |> List.map Result.toMaybe
        |> Maybe.Extra.values
        |> List.filter (not << List.isEmpty)


updateNumberOfCombos : RangesForm -> RangesForm
updateNumberOfCombos form =
    let
        (RangesForm f) =
            form
    in
    RangesForm
        { f
            | numberOfCombos =
                { initialNumberOfCombos
                    | utg = CardRemoval.numberOfCombos (range UTG form |> List.concatMap HandOrCombo.combos) (board form) (allRangesExcept UTG form)
                    , mp = CardRemoval.numberOfCombos (range MP form |> List.concatMap HandOrCombo.combos) (board form) (allRangesExcept MP form)
                    , co = CardRemoval.numberOfCombos (range CO form |> List.concatMap HandOrCombo.combos) (board form) (allRangesExcept CO form)
                    , bu = CardRemoval.numberOfCombos (range BU form |> List.concatMap HandOrCombo.combos) (board form) (allRangesExcept BU form)
                    , sb = CardRemoval.numberOfCombos (range SB form |> List.concatMap HandOrCombo.combos) (board form) (allRangesExcept SB form)
                    , bb = CardRemoval.numberOfCombos (range BB form |> List.concatMap HandOrCombo.combos) (board form) (allRangesExcept BB form)
                }
        }


numberOfCombos : Position -> RangesForm -> Int
numberOfCombos position (RangesForm form) =
    case position of
        UTG ->
            form.numberOfCombos.utg

        MP ->
            form.numberOfCombos.mp

        CO ->
            form.numberOfCombos.co

        BU ->
            form.numberOfCombos.bu

        SB ->
            form.numberOfCombos.sb

        BB ->
            form.numberOfCombos.bb


clearBoard : RangesForm -> RangesForm
clearBoard (RangesForm form) =
    RangesForm { form | board = Form.Field.clear [] form.board }


clearRange : Position -> RangesForm -> RangesForm
clearRange position (RangesForm form) =
    case position of
        UTG ->
            RangesForm { form | utg = Form.Field.clear [] form.utg }

        MP ->
            RangesForm { form | mp = Form.Field.clear [] form.mp }

        CO ->
            RangesForm { form | co = Form.Field.clear [] form.co }

        BU ->
            RangesForm { form | bu = Form.Field.clear [] form.bu }

        SB ->
            RangesForm { form | sb = Form.Field.clear [] form.sb }

        BB ->
            RangesForm { form | bb = Form.Field.clear [] form.bb }
