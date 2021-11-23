module Views.Modal.Board exposing (view)

import Bootstrap.Alt.Modal as Modal
import Bootstrap.Button as Button
import Bootstrap.Utilities.Flex as Flex
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model, Msg(..))
import Poker.Card exposing (Card)
import Poker.Rank as Rank
import Poker.Suit as Suit
import Views.Card exposing (SelectState(..))


view : Model -> Html Msg
view model =
    Modal.config CloseBoardSelectModal
        |> Modal.large
        |> Modal.attrs [ Html.Attributes.class "modal-fullscreen-lg-down" ]
        |> Modal.h4 [] [ Html.text "Board" ]
        |> Modal.body []
            [ Html.div [ Flex.block, Flex.col, Flex.justifyCenter, Flex.alignItemsCenter ]
                (Suit.all
                    |> List.map
                        (\suit ->
                            Html.div
                                [ Flex.block
                                , Flex.row
                                , Flex.wrap
                                ]
                                (Rank.all
                                    |> List.map
                                        (\rank ->
                                            Card rank suit |> (\card -> Views.Card.view (Just <| ToggleBoardSelection card) (cardSelectState card model) "pointer" "6vw" card)
                                        )
                                )
                        )
                )
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.light
                , Button.onClick ClearBoard
                ]
                [ Html.text "CLEAR ALL" ]
            , Button.button
                [ Button.light
                , Button.onClick CloseBoardSelectModal
                ]
                [ Html.text "CANCEL" ]
            , Button.button
                [ Button.success
                , Button.onClick ConfirmBoardSelection
                , Button.disabled (isBoardSelectionValid model |> not)
                ]
                [ Html.text "CONFIRM" ]
            ]
        |> Modal.view model.boardSelectModalVisibility


isBoardSelectionValid : Model -> Bool
isBoardSelectionValid model =
    case model.boardSelection |> List.length of
        0 ->
            True

        3 ->
            True

        4 ->
            True

        5 ->
            True

        _ ->
            False


cardSelectState : Card -> Model -> SelectState
cardSelectState card model =
    case model.cardUnderMouse of
        Just cardUnderMouse ->
            if
                cardUnderMouse
                    == card
                    && not model.ignoreCardHoverState
                    && (List.length model.boardSelection < 5 || (model.boardSelection |> List.member card))
            then
                MouseOver

            else if model.boardSelection |> List.member card then
                Selected

            else
                NotSelected

        Nothing ->
            if model.boardSelection |> List.member card then
                Selected

            else
                NotSelected
