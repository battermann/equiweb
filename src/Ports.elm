port module Ports exposing (copyToClipboard, initTooltips, notifyCopyToClipboard)


port copyToClipboard : ( String, String ) -> Cmd msg


port notifyCopyToClipboard : (String -> msg) -> Sub msg


port initTooltips : () -> Cmd msg
