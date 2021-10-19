module Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Browser.Events
import Json.Decode as Decode
import Keyboard
import Model exposing (Model, Msg(..))
import Poker.Position exposing (Position(..))
import Ports exposing (SharingType(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Browser.Events.onMouseDown (Decode.succeed MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Dropdown.subscriptions model.rangeDropdownStateUtg (RangeDropdownMsg UTG)
        , Dropdown.subscriptions model.rangeDropdownStateMp (RangeDropdownMsg MP)
        , Dropdown.subscriptions model.rangeDropdownStateCo (RangeDropdownMsg CO)
        , Dropdown.subscriptions model.rangeDropdownStateBu (RangeDropdownMsg BU)
        , Dropdown.subscriptions model.rangeDropdownStateSb (RangeDropdownMsg SB)
        , Dropdown.subscriptions model.rangeDropdownStateBb (RangeDropdownMsg BB)
        , Ports.notifyCopyToClipboard (Decode.decodeValue Ports.copiedToClipboardMsgDecoder >> NotifyCopyToClipboard)
        , Dropdown.subscriptions model.rangeSelectionDropdown RangeSelectionDropdownMsg
        ]
