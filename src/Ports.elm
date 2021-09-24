port module Ports exposing (copyToClipboard, notifyCopyToClipboard, ready)


port copyToClipboard : ( String, String ) -> Cmd msg


port notifyCopyToClipboard : (String -> msg) -> Sub msg


port ready : () -> Cmd msg
