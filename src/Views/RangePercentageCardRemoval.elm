module Views.RangePercentageCardRemoval exposing (view, viewWithCardRemoval)

import Bootstrap.Form as Form
import Bootstrap.Progress as Progress
import Bootstrap.Utilities.Spacing as Spacing
import Form
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model, Msg)
import Poker.Combo as Combo
import Poker.Position exposing (Position)
import Round


viewWithCardRemoval : Bool -> Position -> Model -> List (Html Msg)
viewWithCardRemoval edited position model =
    if edited then
        view (Form.numberOfCombos position model.form)

    else
        []


view : Int -> List (Html Msg)
view numberOfCombos =
    let
        percentage =
            Combo.percentage numberOfCombos
    in
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
