port module Ports exposing (CopiedToClipboardMsg, CopyToClipboardMsg, SharingType(..), copiedToClipboardMsgDecoder, copyToClipboard, copyToclipboardMsgEncoder, notifyCopyToClipboard)

import Json.Decode
import Json.Encode


type alias CopyToClipboardMsg =
    { text : String
    , index : Int
    , sharingType : SharingType
    }


type alias CopiedToClipboardMsg =
    { index : Int
    , sharingType : SharingType
    }


type SharingType
    = URL
    | Markdown
    | TwoPlusTwo
    | PokerStrategy


sharingTypeEncoder : SharingType -> Json.Encode.Value
sharingTypeEncoder sharingType =
    Json.Encode.string <|
        case sharingType of
            URL ->
                "url"

            Markdown ->
                "md"

            TwoPlusTwo ->
                "two-plus-two"

            PokerStrategy ->
                "poker-strategy"


copyToclipboardMsgEncoder : CopyToClipboardMsg -> Json.Encode.Value
copyToclipboardMsgEncoder root =
    Json.Encode.object
        [ ( "index", Json.Encode.int root.index )
        , ( "sharingType", sharingTypeEncoder root.sharingType )
        , ( "text", Json.Encode.string root.text )
        ]


copiedToClipboardMsgDecoder : Json.Decode.Decoder CopiedToClipboardMsg
copiedToClipboardMsgDecoder =
    Json.Decode.map2 CopiedToClipboardMsg
        (Json.Decode.field "index" Json.Decode.int)
        (Json.Decode.field "sharingType" sharingTypeDecoder)


sharingTypeDecoder : Json.Decode.Decoder SharingType
sharingTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "url" ->
                        Json.Decode.succeed URL

                    "md" ->
                        Json.Decode.succeed Markdown

                    "two-plus-two" ->
                        Json.Decode.succeed TwoPlusTwo

                    "poker-strategy" ->
                        Json.Decode.succeed PokerStrategy

                    _ ->
                        Json.Decode.fail "invalid sharing type"
            )


port copyToClipboard : Json.Encode.Value -> Cmd msg


port notifyCopyToClipboard : (Json.Encode.Value -> msg) -> Sub msg
