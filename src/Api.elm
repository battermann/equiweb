module Api exposing (sendSimulationRequest)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as P
import Maybe.Extra
import Model exposing (ApiResponse, Msg(..))
import Poker.Card as Card exposing (Card)
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position exposing (Position(..))
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Process
import RemoteData
import Task
import Url.Builder


emptyApiResponse : Int -> ApiResponse
emptyApiResponse num =
    { equityPlayer1 = 0
    , equityPlayer2 = 0
    , equityPlayer3 = Just 0 |> Maybe.Extra.filter (always <| num > 2)
    , equityPlayer4 = Just 0 |> Maybe.Extra.filter (always <| num > 3)
    , equityPlayer5 = Just 0 |> Maybe.Extra.filter (always <| num > 4)
    , equityPlayer6 = Just 0 |> Maybe.Extra.filter (always <| num > 5)
    }


simulationResponseDecoder : Decode.Decoder ApiResponse
simulationResponseDecoder =
    Decode.succeed ApiResponse
        |> P.required "equity_player_1" Decode.float
        |> P.required "equity_player_2" Decode.float
        |> P.required "equity_player_3" (Decode.nullable Decode.float)
        |> P.required "equity_player_4" (Decode.nullable Decode.float)
        |> P.required "equity_player_5" (Decode.nullable Decode.float)
        |> P.required "equity_player_6" (Decode.nullable Decode.float)


sendSimulationRequest : Maybe String -> List Card -> List (List HandOrCombo) -> Cmd Msg
sendSimulationRequest maybeBaseUrl board ranges =
    case maybeBaseUrl of
        Just baseUrl ->
            Http.get
                { expect = Http.expectJson (RemoteData.fromResult >> ApiResponseReceived) simulationResponseDecoder
                , url =
                    Url.Builder.crossOrigin baseUrl
                        [ "simulation" ]
                        ([ Url.Builder.string "board" (board |> List.map Card.toString |> String.concat)
                         , Url.Builder.string "stdev_target" "0.001"
                         , Url.Builder.string "num_iterations" "500"
                         ]
                            ++ (ranges
                                    |> List.indexedMap
                                        (\i range ->
                                            Url.Builder.string ("range" ++ String.fromInt (i + 1)) (range |> List.map HandOrCombo.toString |> String.join ",")
                                        )
                               )
                        )
                }

        Nothing ->
            Process.sleep 1000 |> Task.perform (always (Model.ApiResponseReceived (RemoteData.succeed (emptyApiResponse (List.length ranges)))))
