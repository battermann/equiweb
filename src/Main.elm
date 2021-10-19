module Main exposing (main)

import Browser
import Json.Decode as Decode
import Model exposing (Model, Msg(..), init)
import Subscriptions exposing (subscriptions)
import Update exposing (sendSimulationRequest, update)
import View exposing (view)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init sendSimulationRequest
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }
