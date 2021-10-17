module Views.RangePercentageCardRemoval exposing (viewUnblocked, viewWithCardRemoval)

import Bootstrap.Form as Form
import Bootstrap.Progress as Progress
import Bootstrap.Utilities.Spacing as Spacing
import Form
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Model exposing (Model, Msg(..))
import Poker.Combo as Combo exposing (Combo)
import Poker.Position exposing (Position(..))
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Round


viewWithCardRemoval : Bool -> Position -> Model -> List (Html Msg)
viewWithCardRemoval edited position model =
    let
        numberOfCombos =
            Form.numberOfCombos position model.form
    in
    if edited then
        view numberOfCombos (Combo.percentage numberOfCombos)

    else
        []


viewUnblocked : List Combo -> List Combo -> List (Html Msg)
viewUnblocked combos blockers =
    let
        numberOfCombos =
            combos |> List.Extra.count (\combo -> List.member combo blockers |> not)
    in
    view numberOfCombos (Combo.percentage numberOfCombos)


view : Int -> Float -> List (Html Msg)
view numberOfCombos percentage =
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
