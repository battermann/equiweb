module Views.RangePercentageCardRemoval exposing (view, viewIfNotEmpty, viewWithCardRemoval)

import Bootstrap.Form as Form
import Bootstrap.Progress as Progress
import Bootstrap.Utilities.Spacing as Spacing
import Form
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model, Msg(..))
import Poker.Combo as Combo
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position exposing (Position(..))
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Round


viewIfNotEmpty : List HandOrCombo -> List (Html Msg)
viewIfNotEmpty range =
    if range |> List.isEmpty |> not then
        [ view range
        ]

    else
        []


view : List HandOrCombo -> Html Msg
view range =
    Form.help []
        [ Html.div [ Spacing.mt1 ]
            [ Progress.progress
                [ Progress.value (HandOrCombo.percentage range * 100)
                , Progress.info
                , Progress.wrapperAttrs [ Html.Attributes.style "height" "6px" ]
                ]
            , Html.text (((HandOrCombo.percentage range * 100) |> Round.round 1) ++ "%" ++ " " ++ "(" ++ ((HandOrCombo.numberOfCombos range |> String.fromInt) ++ "/" ++ (Combo.total |> String.fromInt) ++ ")"))
            ]
        ]


viewWithCardRemoval : Bool -> Position -> Model -> List (Html Msg)
viewWithCardRemoval edited position model =
    let
        numberOfCombos =
            Form.numberOfCombos position model.form

        percentage =
            Combo.percentage numberOfCombos
    in
    if edited then
        [ Form.help []
            [ Html.div [ Spacing.mt1 ]
                [ Progress.progress
                    [ Progress.value (percentage * 100)
                    , Progress.info
                    , Progress.wrapperAttrs [ Html.Attributes.style "height" "6px" ]
                    ]
                , Html.text (((percentage * 100) |> Round.round 1) ++ "%" ++ " " ++ "(" ++ ((numberOfCombos |> String.fromInt) ++ "/" ++ (Combo.total |> String.fromInt) ++ ")"))
                ]
            ]
        ]

    else
        []
