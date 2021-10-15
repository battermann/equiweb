module Views.Modal.Range exposing (SelectState(..), view)

import Bootstrap.Alt.Modal as Modal
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import DoubleSlider as Slider
import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Model exposing (Model, Msg(..))
import Poker.Hand as Hand exposing (Hand)
import Poker.HandOrCombo as HandOrCombo
import Poker.Position as Position exposing (Position(..))
import Poker.Ranges as Ranges
import Poker.Suit as Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Round
import Svg
import Svg.Attributes
import Views.Board
import Views.RangePercentageCardRemoval


type SelectState
    = Selected Int
    | PartiallySelected Int
    | NotSelected
    | MouseOver
    | MouseOverDuringSuitSelection Int Int Int


view : Model -> Html Msg
view model =
    Modal.config CloseRangeSelectionModal
        |> Modal.attrs [ Html.Attributes.class "modal-xl modal-fullscreen-xxl-down" ]
        |> Modal.h2 [ Flex.block, Flex.row, Flex.alignItemsCenter, Flex.justifyBetween, Size.w50 ]
            [ Html.text (model.rangeSelectionPosition |> Position.toString)
            ]
        |> Modal.body []
            [ Grid.row []
                [ Grid.col []
                    [ Html.div [ Flex.row, Flex.block, Flex.justifyAround, Spacing.mb2 ]
                        [ Html.div [ Flex.block, Flex.col ]
                            ((Hand.grid
                                |> List.map
                                    (\row ->
                                        Html.div
                                            [ Flex.block, Flex.row ]
                                            (row
                                                |> List.map
                                                    (\hand ->
                                                        cellView
                                                            (rangeSelectState hand model)
                                                            "5vm"
                                                            hand
                                                    )
                                            )
                                    )
                             )
                                ++ [ Views.RangePercentageCardRemoval.view (model.rangeSelection |> List.map HandOrCombo.fromCombo) ]
                             -- ++ [ Views.RangePercentage.viewWithCardRemoval model.rangeSelectionPosition model ]
                            )
                        ]
                    ]
                , Grid.col []
                    ((case Form.board model.form of
                        [] ->
                            []

                        board ->
                            [ Html.div [ Flex.block, Flex.row, Spacing.mb2 ] [ Views.Board.view False "default" "27px" board ]
                            , Html.hr [] []
                            ]
                     )
                        ++ (case model.suitSelection of
                                Just suitSelection ->
                                    [ Button.button
                                        [ Button.attrs
                                            [ Spacing.mb2
                                            , Html.Attributes.style "height" "38px"
                                            ]
                                        , Button.light
                                        , Button.onClick ToggleSuitSelection
                                        ]
                                        [ Html.i [ Html.Attributes.class "fas fa-chevron-left" ] [] ]
                                    , Html.div [ Spacing.mb2, Flex.col, Flex.block, Html.Attributes.style "gap" "10px" ]
                                        [ suitedSuitSelectionView suitSelection.suited
                                        , pairsSuitSelectionView suitSelection.pairs
                                        , offsuitSuitSelectionView suitSelection.offsuit
                                        ]
                                    ]

                                Nothing ->
                                    [ suitSelectionButton
                                    , Html.div [ Size.w100 ]
                                        [ Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "10px" ]
                                            [ Html.div [] [ Html.text ("PFR: " ++ Round.round 2 (Slider.fetchLowValue model.slider) ++ "%") ]
                                            , Html.div [] [ Html.text ("VPIP: " ++ Round.round 2 (Slider.fetchHighValue model.slider) ++ "%") ]
                                            ]
                                        , Html.div [] [ Slider.view model.slider ]
                                        ]
                                    , Dropdown.dropdown
                                        model.rangeSelectionDropdown
                                        { options = [ Dropdown.attrs [ Spacing.mb2 ] ]
                                        , toggleMsg = RangeSelectionDropdownMsg
                                        , toggleButton =
                                            Dropdown.toggle [ Button.outlineSecondary ] [ Html.text "Preset Ranges" ]
                                        , items =
                                            Ranges.positionalRanges
                                                |> List.filter (.position >> (==) model.rangeSelectionPosition)
                                                |> List.map
                                                    (\r ->
                                                        Dropdown.buttonItem [ Html.Events.onClick (SelectRange r.range) ] [ Html.text r.label ]
                                                    )
                                        }
                                    , Html.div [ Flex.block, Flex.row, Spacing.mb2, Flex.wrap, Html.Attributes.style "gap" "8px" ]
                                        [ Button.button [ Button.outlineSecondary, Button.onClick SelectPairs ] [ Html.text "POCKET PAIRS" ]
                                        , Button.button [ Button.outlineSecondary, Button.onClick SelectSuitedAces ] [ Html.text "SUITED ACES" ]
                                        , Button.button [ Button.outlineSecondary, Button.onClick SelectSuitedBroadways ] [ Html.text "SUITED BROADWAYS" ]
                                        , Button.button [ Button.outlineSecondary, Button.onClick SelectOffsuitAces ] [ Html.text "OFFSUIT ACES" ]
                                        , Button.button [ Button.outlineSecondary, Button.onClick SelectOffsuitBroadways ] [ Html.text "OFFSUIT BROADWAYS" ]
                                        ]
                                    ]
                           )
                    )
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.light
                , Button.onClick ClearRange
                ]
                [ Html.text "CLEAR ALL" ]
            , Button.button [ Button.light, Button.onClick CloseRangeSelectionModal ] [ Html.text "CANCEL" ]
            , Button.button
                [ Button.success
                , Button.onClick ConfirmRangeSelection
                ]
                [ Html.text "CONFIRM" ]
            ]
        |> Modal.view model.rangeSelectionModalVisibility


suitSelectionButton : Html Msg
suitSelectionButton =
    Button.button
        [ Button.attrs
            [ Spacing.mb2
            , Html.Attributes.style "height" "38px"
            ]
        , Button.light
        , Button.onClick ToggleSuitSelection
        ]
        [ Html.div [ Flex.row, Flex.block, Html.Attributes.style "gap" "6px" ]
            (Suit.all |> List.map suitIconView)
        ]


suitIconView : Suit -> Html Msg
suitIconView suit =
    Html.img
        [ case suit of
            Suit.Clubs ->
                Html.Attributes.src "images/playing-card-club-shape.svg"

            Suit.Spades ->
                Html.Attributes.src "images/playing-card-spade-shape.svg"

            Suit.Diamonds ->
                Html.Attributes.src "images/playing-card-diamond-shape.svg"

            Suit.Hearts ->
                Html.Attributes.src "images/playing-card-heart-shape.svg"
        , Html.Attributes.height 20
        , Html.Attributes.width 20
        ]
        []


suitButtonView : Msg -> Bool -> Suit -> Suit -> Html Msg
suitButtonView msg isSelected suit1 suit2 =
    let
        ( opacity, button ) =
            if isSelected then
                ( "1"
                , Button.attrs
                    [ Html.Attributes.style "background-color" "#bbbbbb"
                    ]
                )

            else
                ( "0.6"
                , Button.attrs
                    []
                )
    in
    Button.button [ Button.light, Button.onClick msg, button ]
        [ Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "2px", Html.Attributes.style "opacity" opacity ]
            [ suitIconView suit1
            , suitIconView suit2
            ]
        ]


suitedSuitSelectionView : List Suit -> Html Msg
suitedSuitSelectionView suitSelection =
    Html.div []
        [ Html.text "Suited"
        , Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "10px", Flex.wrap ]
            (Suit.all |> List.map (\suit -> suitButtonView (ToggleSuitedSuitsSelection suit) (suitSelection |> List.member suit) suit suit))
        ]


pairsSuitSelectionView : List ( Suit, Suit ) -> Html Msg
pairsSuitSelectionView suits =
    Html.div []
        [ Html.text "Pocket Pairs"
        , Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "10px", Flex.wrap ]
            (Suit.suitCombinations |> List.map (\( suit1, suit2 ) -> suitButtonView (TogglePairsSuitsSelection suit1 suit2) (suits |> List.member ( suit1, suit2 )) suit1 suit2))
        ]


offsuitSuitSelectionView : List ( Suit, Suit ) -> Html Msg
offsuitSuitSelectionView suits =
    Html.div []
        [ Html.text "Offsuit"
        , Html.div [ Flex.block, Flex.col, Html.Attributes.style "gap" "10px" ]
            (Suit.all
                |> List.map
                    (\suit1 ->
                        Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "10px" ]
                            (Suit.all
                                |> List.filterMap
                                    (\suit2 ->
                                        if suit1 /= suit2 then
                                            Just <|
                                                suitButtonView (ToggleOffsuitSuitsSelection suit1 suit2) (suits |> List.member ( suit1, suit2 )) suit1 suit2

                                        else
                                            Nothing
                                    )
                            )
                    )
            )
        ]


rangeSelectState : Hand -> Model -> SelectState
rangeSelectState hand model =
    case model.handUnderMouse of
        Just handUnderMouse ->
            if handUnderMouse == hand && not model.ignoreRangeHoverState then
                case model.suitSelection of
                    Nothing ->
                        MouseOver

                    Just suitSelection ->
                        MouseOverDuringSuitSelection
                            (suitSelection.suited |> List.length)
                            (suitSelection.pairs |> List.length)
                            (suitSelection.offsuit |> List.length)

            else
                case model.rangeSelection |> Hand.numCombosOfHand hand of
                    Hand.All n ->
                        Selected n

                    Hand.None ->
                        NotSelected

                    Hand.Some n ->
                        PartiallySelected n

        Nothing ->
            case model.rangeSelection |> Hand.numCombosOfHand hand of
                Hand.All n ->
                    Selected n

                Hand.None ->
                    NotSelected

                Hand.Some n ->
                    PartiallySelected n


cellView : SelectState -> String -> Hand -> Html Msg
cellView cs size hand =
    let
        ( ( fontColor, color, opacity ), maybeNum ) =
            case cs of
                Selected num ->
                    ( ( "white", "#9b5378", "1" ), Just num )

                NotSelected ->
                    if hand |> Hand.isOffsuit then
                        ( ( "#aaaaaa", "#eeeeee", "1" ), Nothing )

                    else if hand |> Hand.isSuited then
                        ( ( "#aaaaaa", "#dddddd", "1" ), Nothing )

                    else
                        ( ( "#aaaaaa", "#cccccc", "1" ), Nothing )

                MouseOver ->
                    ( ( "white", "#9b5378", "0.5" ), Nothing )

                PartiallySelected num ->
                    ( ( "white", "#db9713", "1" ), Just num )

                MouseOverDuringSuitSelection numSuited numPairs numOffsuit ->
                    ( ( "white", "#db9713", "0.5" ), Just (hand |> Hand.fold (always numPairs) (always (always numSuited)) (always (always numOffsuit))) )
    in
    Html.div
        [ Html.Attributes.style "width" size
        , Html.Attributes.style "height" size
        , Html.Attributes.style "min-height" "20px"
        , Html.Attributes.style "min-width" "18px"
        , Html.Attributes.style "max-height" "38px"
        , Html.Attributes.style "max-width" "38px"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "margin" "1px"
        , Html.Attributes.style "user-select" "none"
        , Html.Attributes.style "opacity" opacity
        , Html.Events.onMouseEnter (HandHover (Just hand))
        , Html.Events.onMouseLeave (HandHover Nothing)
        ]
        [ Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.viewBox "0 0 100 100"
            ]
            ([ Svg.rect
                [ Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "100"
                , Svg.Attributes.height "100"
                , Svg.Attributes.rx "15"
                , Svg.Attributes.ry "15"
                , Svg.Attributes.fill color
                ]
                []
             , Svg.text_
                [ Svg.Attributes.x "50"
                , Svg.Attributes.y "50"
                , Svg.Attributes.fill fontColor
                , Svg.Attributes.fontSize "44"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.dominantBaseline "middle"
                ]
                [ Svg.text (hand |> Hand.toString) ]
             ]
                ++ (case maybeNum of
                        Just num ->
                            [ Svg.text_
                                [ Svg.Attributes.x "78"
                                , Svg.Attributes.y "20"
                                , Svg.Attributes.fill "#dddddd"
                                , Svg.Attributes.fontSize "27"
                                , Svg.Attributes.textAnchor "middle"
                                , Svg.Attributes.dominantBaseline "middle"
                                ]
                                [ Svg.text (num |> String.fromInt) ]
                            ]

                        Nothing ->
                            []
                   )
            )
        ]
