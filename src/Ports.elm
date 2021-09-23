port module Ports exposing (copyToClipboard, notifyCopyToClipboard)


port copyToClipboard : ( String, String ) -> Cmd msg


port notifyCopyToClipboard : (String -> msg) -> Sub msg
