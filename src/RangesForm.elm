module RangesForm exposing
    ( RangesForm
    , board
    , boardField
    , initialForm
    , range
    , rangeField
    , rewritable
    , rewrite
    , rewriteBoard
    , setAllFormFieldsToEdited
    , setBoard
    , setRange
    , validateForm
    )

import Form
import Poker.Board as Board
import Poker.Card as Card exposing (Card)
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position exposing (Position(..))
import Result.Extra


type RangesForm
    = RangesForm
        { utg : Form.Field (List HandOrCombo)
        , mp : Form.Field (List HandOrCombo)
        , co : Form.Field (List HandOrCombo)
        , bu : Form.Field (List HandOrCombo)
        , sb : Form.Field (List HandOrCombo)
        , bb : Form.Field (List HandOrCombo)
        , board : Form.Field (List Card)
        }


board : RangesForm -> List Card
board (RangesForm form) =
    form.board.validated |> Result.withDefault []


setBoard : String -> RangesForm -> RangesForm
setBoard b (RangesForm form) =
    RangesForm { form | board = form.board |> Form.setValue Board.validate b }


setRange : Position -> String -> RangesForm -> RangesForm
setRange position r (RangesForm form) =
    case position of
        UTG ->
            RangesForm { form | utg = form.utg |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        MP ->
            RangesForm { form | mp = form.mp |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        CO ->
            RangesForm { form | co = form.co |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        BU ->
            RangesForm { form | bu = form.bu |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        SB ->
            RangesForm { form | sb = form.sb |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }

        BB ->
            RangesForm { form | bb = form.bb |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos r }


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
        }


setAllFormFieldsToEdited : RangesForm -> RangesForm
setAllFormFieldsToEdited (RangesForm form) =
    RangesForm
        { form
            | utg = form.utg |> Form.setEdited
            , mp = form.mp |> Form.setEdited
            , co = form.co |> Form.setEdited
            , bu = form.bu |> Form.setEdited
            , sb = form.sb |> Form.setEdited
            , bb = form.bb |> Form.setEdited
            , board = form.board |> Form.setEdited
        }


rewritable : Form.Field (List HandOrCombo) -> Bool
rewritable field =
    field.value
        /= (field.validated |> Result.withDefault [] |> HandOrCombo.toNormalizedString)
        && (field.validated |> Result.Extra.isOk)


rangeField : Position -> RangesForm -> Form.Field (List HandOrCombo)
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


boardField : RangesForm -> Form.Field (List Card)
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


rewriteBoard : RangesForm -> RangesForm
rewriteBoard (RangesForm form) =
    RangesForm { form | board = Form.rewrite form.board (List.map Card.toString >> String.concat) }


rewrite : Position -> RangesForm -> RangesForm
rewrite position (RangesForm form) =
    case position of
        UTG ->
            RangesForm { form | utg = Form.rewrite form.utg HandOrCombo.toNormalizedString }

        MP ->
            RangesForm { form | mp = Form.rewrite form.mp HandOrCombo.toNormalizedString }

        CO ->
            RangesForm { form | co = Form.rewrite form.co HandOrCombo.toNormalizedString }

        BU ->
            RangesForm { form | bu = Form.rewrite form.bu HandOrCombo.toNormalizedString }

        SB ->
            RangesForm { form | sb = Form.rewrite form.sb HandOrCombo.toNormalizedString }

        BB ->
            RangesForm { form | bb = Form.rewrite form.bb HandOrCombo.toNormalizedString }


validateForm : RangesForm -> Result (List String) RangesForm
validateForm (RangesForm form) =
    Ok (\_ _ _ _ _ _ _ -> RangesForm form)
        |> Form.apply (form.utg.validated |> Result.Extra.mapBoth (always [ "The UTG range is not a valid range." ]) identity)
        |> Form.apply (form.mp.validated |> Result.Extra.mapBoth (always [ "The MP range is not a valid range." ]) identity)
        |> Form.apply (form.co.validated |> Result.Extra.mapBoth (always [ "The CO range is not a valid range." ]) identity)
        |> Form.apply (form.bu.validated |> Result.Extra.mapBoth (always [ "The BU range is not a valid range." ]) identity)
        |> Form.apply (form.sb.validated |> Result.Extra.mapBoth (always [ "The SB range is not a valid range." ]) identity)
        |> Form.apply (form.bb.validated |> Result.Extra.mapBoth (always [ "The BB range is not a valid range." ]) identity)
        |> Form.apply (form.board.validated |> Result.Extra.mapBoth (always [ "The board is not a valid board." ]) identity)
