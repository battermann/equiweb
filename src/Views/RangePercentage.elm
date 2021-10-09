module Views.RangePercentage exposing (view, viewIfNotEmpty)

import Bootstrap.Form as Form
import Bootstrap.Progress as Progress
import Bootstrap.Utilities.Spacing as Spacing
import Form
import Html exposing (Html)
import Html.Attributes
import Model exposing (Msg(..))
import Poker.Combo as Combo
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position exposing (Position(..))
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Round


viewIfNotEmpty : List HandOrCombo -> List (Html Msg)
viewIfNotEmpty ranges =
    if ranges |> List.isEmpty |> not then
        [ view ranges
        ]

    else
        []


view : List HandOrCombo -> Html Msg
view ranges =
    Form.help []
        [ Html.div [ Spacing.mt1 ]
            [ Progress.progress
                [ Progress.value (HandOrCombo.percentage ranges * 100)
                , Progress.info
                , Progress.wrapperAttrs [ Html.Attributes.style "height" "6px" ]
                ]
            , Html.text (((HandOrCombo.percentage ranges * 100) |> Round.round 1) ++ "%" ++ " " ++ "(" ++ ((HandOrCombo.numberOfCombos ranges |> String.fromInt) ++ "/" ++ (Combo.total |> String.fromInt) ++ ")"))
            ]
        ]
