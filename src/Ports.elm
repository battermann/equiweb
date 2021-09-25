port module Ports exposing (CopyToClipboardMsg, copyToClipboard, notifyCopyToClipboard)


type alias CopyToClipboardMsg =
    { text : String
    , index : Int
    }


port copyToClipboard : CopyToClipboardMsg -> Cmd msg


port notifyCopyToClipboard : (Int -> msg) -> Sub msg
