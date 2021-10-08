module View exposing (view)

import Bootstrap.Alt.Alert as Alert
import Bootstrap.Alt.Modal as Modal
import Bootstrap.Alt.Popover as Popover
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import DoubleSlider as Slider
import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Model exposing (Model, Msg(..), PopoverStates, ResultLine, SharingPopoverStates, SimulationResult)
import Poker.Card as Card exposing (Card)
import Poker.Combo as Combo
import Poker.Hand as Hand exposing (Hand)
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position as Position exposing (Position(..))
import Poker.Ranges as Ranges
import Poker.Rank as Rank
import Poker.Suit as Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import RangesForm
import RemoteData
import Result.Extra
import Round
import Svg
import Svg.Attributes
import Url.Builder
import View.CardSelectState as CardSelectState exposing (CardSelectState)
import View.RangeCellSelectState as RangeCellSelectState exposing (RangeCellSelectState)


view : Model -> Browser.Document Msg
view model =
    { title = "Equiweb"
    , body =
        [ Grid.container []
            [ calculatorView model
            , boardSelectionModalView model
            , rangeSelectionModalView model
            ]
        ]
    }


loadingView : Html Msg
loadingView =
    Html.div [ Html.Attributes.class "loading-view" ]
        [ Html.div [ Flex.block, Flex.row, Flex.alignItemsCenter, Flex.justifyAround ] [ Spinner.spinner [ Spinner.large ] [] ]
        ]


calculatorView : Model -> Html Msg
calculatorView model =
    Grid.row []
        [ Grid.col []
            [ Card.deck
                ((Card.config [ Card.attrs [ Spacing.mb3, Html.Attributes.class "shadow" ] ]
                    |> Card.headerH2 []
                        [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
                            [ Html.a [ Html.Attributes.href (Url.Builder.absolute [] []), Flex.block, Flex.row, Flex.alignItemsStart ]
                                [ Html.img [ Html.Attributes.src "images/chip-icon.svg", Html.Attributes.width 40 ] []
                                , Html.div [ Html.Attributes.style "margin-top" "auto", Html.Attributes.style "margin-left" "7px", Html.Attributes.style "margin-bottom" "auto" ] [ Html.text "Equiweb" ]
                                ]
                            ]
                        ]
                    |> Card.block []
                        [ Block.custom <|
                            case model.currentApiResponse of
                                RemoteData.Loading ->
                                    Html.div [] [ loadingView, inputFormView model ]

                                RemoteData.Failure _ ->
                                    inputFormView model

                                _ ->
                                    inputFormView model
                        ]
                 )
                    :: (model.results |> List.reverse |> List.indexedMap (\index ( popoverStates, _, res ) -> resultView index popoverStates res))
                )
            ]
        ]


equityValueView : Maybe ResultLine -> Input.Option msg
equityValueView result =
    case result of
        Nothing ->
            Input.value ""

        Just value ->
            Input.value (Round.round 2 (100 * value.equity) ++ " %")


validationFeedbackOutline : Form.Field a -> List (Input.Option msg)
validationFeedbackOutline field =
    case ( field.validated, field.edited ) of
        ( Ok _, True ) ->
            [ Input.success ]

        ( Err _, True ) ->
            [ Input.danger ]

        _ ->
            []


cardView : Maybe Msg -> CardSelectState -> String -> String -> Card -> Html Msg
cardView msg selectState cursor refWidth card =
    let
        ( color, icon ) =
            case card.suit of
                Club ->
                    ( "forestgreen", Icons.club )

                Spades ->
                    ( "darkslategrey", Icons.spade )

                Heart ->
                    ( "darkred", Icons.heart )

                Diamond ->
                    ( "royalblue", Icons.diamond )

        opacity =
            case selectState of
                CardSelectState.Selected ->
                    "1"

                CardSelectState.NotSelected ->
                    "0.5"

                CardSelectState.MouseOver ->
                    "0.7"

        width =
            60

        height =
            width * 7.0 / 5.0
    in
    Html.div
        ([ Html.Attributes.style "width" refWidth
         , Html.Attributes.style "min-height" "38px"
         , Html.Attributes.style "min-width" "25px"
         , Html.Attributes.style "max-height" "80px"
         , Html.Attributes.style "max-width" "57px"
         , Html.Attributes.style "cursor" cursor
         , Html.Attributes.style "opacity" opacity
         , Html.Events.onMouseEnter (CardHover <| Just card)
         , Html.Events.onMouseLeave (CardHover Nothing)
         ]
            ++ (msg |> Maybe.map (Html.Events.onClick >> List.singleton) |> Maybe.withDefault [])
        )
        [ Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.viewBox ("0 0" ++ " " ++ ((width + 1) |> String.fromFloat) ++ " " ++ ((height + 1) |> String.fromFloat))
            ]
            [ Svg.rect
                [ Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width (width |> String.fromFloat)
                , Svg.Attributes.height (height |> String.fromFloat)
                , Svg.Attributes.rx ((width / 5) |> String.fromFloat)
                , Svg.Attributes.ry ((width / 5) |> String.fromFloat)
                , Svg.Attributes.fill color
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x ((width * 0.5) |> String.fromFloat)
                , Svg.Attributes.y ((height * 0.65) |> String.fromFloat)
                , Svg.Attributes.fill "white"
                , Svg.Attributes.fontSize (width * 1.1 |> String.fromFloat)
                , Svg.Attributes.fontFamily "monospace"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.dominantBaseline "middle"
                ]
                [ Svg.text (card.rank |> Rank.toString) ]
            , Svg.g [ Svg.Attributes.transform "scale(0.16) translate(35,35)" ] [ icon "black" "0.5" ]
            ]
        ]


inputFormView : Model -> Html Msg
inputFormView model =
    Form.form []
        [ rangeInputView (Model.popoverState UTG model) UTG (RangesForm.rangeField UTG model.form) (model.currentApiResponse |> RemoteData.map .utg |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateUtg (Ranges.positionalRanges |> List.filter (.position >> (==) UTG) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (Model.popoverState MP model) MP (RangesForm.rangeField MP model.form) (model.currentApiResponse |> RemoteData.map .mp |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateMp (Ranges.positionalRanges |> List.filter (.position >> (==) MP) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (Model.popoverState CO model) CO (RangesForm.rangeField CO model.form) (model.currentApiResponse |> RemoteData.map .co |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateCo (Ranges.positionalRanges |> List.filter (.position >> (==) CO) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (Model.popoverState BU model) BU (RangesForm.rangeField BU model.form) (model.currentApiResponse |> RemoteData.map .bu |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateBu (Ranges.positionalRanges |> List.filter (.position >> (==) BU) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (Model.popoverState SB model) SB (RangesForm.rangeField SB model.form) (model.currentApiResponse |> RemoteData.map .sb |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateSb (Ranges.positionalRanges |> List.filter (.position >> (==) SB) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (Model.popoverState BB model) BB (RangesForm.rangeField BB model.form) (model.currentApiResponse |> RemoteData.map .bb |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateBb (Ranges.positionalRanges |> List.filter (.position >> (==) BB) |> List.map (\pr -> ( pr.label, pr.range )))
        , Form.row
            []
            [ Form.col [ Col.sm10 ]
                [ Form.group []
                    [ Form.label [] [ Html.text (RangesForm.boardField model.form).name ]
                    , InputGroup.config
                        (InputGroup.text
                            (validationFeedbackOutline (RangesForm.boardField model.form)
                                ++ [ Input.value (RangesForm.boardField model.form).value
                                   , Input.onInput BoardInput
                                   ]
                            )
                        )
                        |> InputGroup.attrs
                            (if (RangesForm.boardField model.form).validated |> Result.Extra.isOk then
                                [ Html.Attributes.class "is-valid" ]

                             else
                                [ Html.Attributes.class "is-invalid" ]
                            )
                        |> InputGroup.predecessors
                            [ InputGroup.span [ Html.Attributes.class "tooltip-wrapper" ]
                                [ Popover.config
                                    (Button.button
                                        [ Button.outlineSecondary
                                        , Button.onClick ShowBoardSelectModal
                                        , Button.attrs (Html.Attributes.tabindex -1 :: Popover.onHover model.popoverStateBoard PopoverStateBoard)
                                        ]
                                        [ Html.img [ Html.Attributes.src "images/apps_black_24dp.svg", Html.Attributes.width 20 ] [] ]
                                    )
                                    |> Popover.top
                                    |> Popover.content []
                                        [ Html.text "Open Board Dialog" ]
                                    |> Popover.view model.popoverStateBoard
                                ]
                            , InputGroup.span [ Html.Attributes.class "tooltip-wrapper" ]
                                [ Popover.config
                                    (Html.div (Popover.onHover model.popoverStateClearBoard PopoverStateClearBoard)
                                        [ Button.button
                                            [ Button.outlineSecondary
                                            , Button.attrs [ Html.Attributes.tabindex -1 ]
                                            , Button.onClick RemoveBoard
                                            , Button.disabled ((RangesForm.boardField model.form).value |> String.isEmpty)
                                            ]
                                            [ Html.i [ Html.Attributes.class "far fa-trash-alt" ] [] ]
                                        ]
                                    )
                                    |> Popover.top
                                    |> Popover.content []
                                        [ Html.text "Clear Board" ]
                                    |> Popover.view model.popoverStateClearBoard
                                ]
                            ]
                        |> InputGroup.view
                    , Form.invalidFeedback [] [ Html.text "The board is not a valid board" ]
                    ]
                ]
            ]
        , if (RangesForm.boardField model.form).validated == Ok [] then
            Html.text ""

          else
            Form.row [ Row.attrs [ Spacing.mt2 ] ] [ Form.col [] [ Button.button [ Button.light, Button.onClick ShowBoardSelectModal ] [ boardView True "pointer" "6vw" (RangesForm.board model.form) ] ] ]
        , if (RangesForm.ranges model.form |> List.length) < 2 then
            Alert.simpleInfo [ Spacing.mt2 ] [ Html.text "You must fill in at least 2 ranges." ]

          else
            Html.text ""
        , Form.row [ Row.attrs [ Spacing.mt2 ] ]
            [ Form.col []
                [ Html.div [ Flex.block, Flex.row ]
                    [ Button.linkButton
                        [ Button.light
                        , Button.attrs [ Size.w100, Html.Attributes.style "margin-right" "2px", Html.Attributes.href (Url.Builder.absolute [] []) ]
                        ]
                        [ Html.text "CLEAR ALL" ]
                    , Button.button
                        [ Button.success
                        , Button.attrs [ Size.w100, Html.Attributes.style "margin-left" "2px" ]
                        , Button.onClick SendSimulationRequest
                        ]
                        [ Html.text "RUN" ]
                    ]
                ]
            ]
        , Form.row [ Row.attrs [ Spacing.mt2 ] ] [ Form.col [ Col.md12 ] [ Html.div [ Html.Attributes.style "text-align" "end", Size.w100 ] [ Html.a [ Html.Attributes.href "https://docs.google.com/forms/d/e/1FAIpQLSdz078OCo4gZikCRU7EDc4BS1NYFWfXioXBPK06_ZL_7BQ4sw/viewform", Html.Attributes.target "blank" ] [ Html.i [ Html.Attributes.class "fas fa-external-link-alt", Html.Attributes.style "margin-right" "2px" ] [], Html.text "report a bug or leave feedback" ] ] ] ]
        ]


rangeInputView : PopoverStates -> Position -> Form.Field (List HandOrCombo) -> Maybe ResultLine -> Dropdown.State -> List ( String, String ) -> Html Msg
rangeInputView popoverStates position field result dropdownState ranges =
    Form.row []
        [ Form.col []
            [ Form.group []
                ([ Form.label []
                    [ Html.text field.name ]
                 , InputGroup.config
                    (InputGroup.text
                        (validationFeedbackOutline field
                            ++ [ Input.value field.value
                               , Input.onInput (RangeInput position)
                               ]
                        )
                    )
                    |> InputGroup.attrs
                        [ if field.validated |> Result.Extra.isOk then
                            Html.Attributes.class "is-valid"

                          else
                            Html.Attributes.class "is-invalid"
                        ]
                    |> InputGroup.predecessors
                        [ InputGroup.span [ Html.Attributes.class "tooltip-wrapper" ]
                            [ Popover.config
                                (Dropdown.dropdown
                                    dropdownState
                                    { options = [ Dropdown.attrs (Popover.onHover popoverStates.rangeSelect (PopoverStateSelectRange position)) ]
                                    , toggleMsg = RangeDropdownMsg position
                                    , toggleButton =
                                        Dropdown.toggle [ Button.outlineSecondary, Button.attrs [ Html.Attributes.tabindex -1 ] ] [ Html.i [ Html.Attributes.class "fas fa-bars" ] [] ]
                                    , items =
                                        ranges
                                            |> List.map
                                                (\( label, range ) ->
                                                    Dropdown.buttonItem [ Html.Events.onClick (SelectPresetRange position range) ] [ Html.text label ]
                                                )
                                    }
                                )
                                |> Popover.top
                                |> Popover.content []
                                    [ Html.text "Select Range" ]
                                |> Popover.view popoverStates.rangeSelect
                            ]
                        , InputGroup.span [ Html.Attributes.class "tooltip-wrapper", Html.Attributes.class "z-index-0" ]
                            [ Popover.config
                                (Button.button
                                    [ Button.outlineSecondary
                                    , Button.onClick (ShowRangeSelectionModal position)
                                    , Button.attrs ([ Html.Attributes.tabindex -1, Html.Attributes.type_ "button" ] ++ Popover.onHover popoverStates.openGrid (PopoverStateOpenGrid position))
                                    ]
                                    [ Html.img [ Html.Attributes.src "images/apps_black_24dp.svg", Html.Attributes.height 22 ] [] ]
                                )
                                |> Popover.top
                                |> Popover.content []
                                    [ Html.text "Open Grid Dialog" ]
                                |> Popover.view popoverStates.openGrid
                            ]
                        , InputGroup.span [ Html.Attributes.class "tooltip-wrapper", Html.Attributes.class "z-index-0" ]
                            [ Popover.config
                                (Html.div (Popover.onHover popoverStates.normalize (PopoverStateNormalize position))
                                    [ Button.button
                                        [ Button.outlineSecondary
                                        , Button.onClick (RewriteRange position)
                                        , Button.disabled (RangesForm.rewritable field |> not)
                                        , Button.attrs [ Html.Attributes.tabindex -1 ]
                                        ]
                                        [ Html.img [ Html.Attributes.src "images/auto_fix_high_black_24dp.svg", Html.Attributes.height 20 ] [] ]
                                    ]
                                )
                                |> Popover.top
                                |> Popover.content []
                                    [ Html.text "Normalize Range" ]
                                |> Popover.view popoverStates.normalize
                            ]
                        , InputGroup.span [ Html.Attributes.class "tooltip-wrapper", Html.Attributes.class "z-index-0" ]
                            [ Popover.config
                                (Html.div (Popover.onHover popoverStates.clear (PopoverStateClear position))
                                    [ Button.button
                                        [ Button.outlineSecondary
                                        , Button.attrs [ Html.Attributes.tabindex -1 ]
                                        , Button.onClick (RemoveRange position)
                                        , Button.disabled (field.value |> String.isEmpty)
                                        ]
                                        [ Html.i [ Html.Attributes.class "far fa-trash-alt" ] [] ]
                                    ]
                                )
                                |> Popover.top
                                |> Popover.content []
                                    [ Html.text "Clear Range" ]
                                |> Popover.view popoverStates.clear
                            ]
                        ]
                    |> InputGroup.view
                 ]
                    ++ numberOfCombosIfNotEmptyView (field.validated |> Result.withDefault [])
                    ++ [ Form.invalidFeedback [] [ Html.text ("The " ++ Position.toString position ++ " range is not a valid range") ] ]
                )
            ]
        , Form.col [ Col.sm2 ]
            [ Form.group []
                [ Form.label [] [ Html.text "Equity" ]
                , Input.text [ Input.readonly True, Input.attrs [ Html.Attributes.tabindex -1 ], equityValueView result ]
                ]
            ]
        ]


numberOfCombosIfNotEmptyView : List HandOrCombo -> List (Html Msg)
numberOfCombosIfNotEmptyView ranges =
    if ranges |> List.isEmpty |> not then
        [ numberOfCombosView ranges ]

    else
        []


numberOfCombosView : List HandOrCombo -> Html Msg
numberOfCombosView ranges =
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


boardCardView : String -> String -> Card -> Html Msg
boardCardView cursor width =
    cardView Nothing CardSelectState.Selected cursor width


boardView : Bool -> String -> String -> List Card -> Html Msg
boardView showLabel cursor width cards =
    Html.div [ Flex.block, Flex.row ] (streetsView showLabel cursor width cards)


streetView : String -> Maybe String -> String -> List Card -> Html Msg
streetView cursor maybeLabel width cards =
    Html.div [ Html.Attributes.style "margin-right" "10px" ]
        ((maybeLabel |> Maybe.Extra.unwrap [] (\label -> [ Html.h6 [ Flex.block, Flex.row, Flex.justifyCenter ] [ Html.text label ] ]))
            ++ [ Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "2px" ] (cards |> List.map (boardCardView cursor width))
               ]
        )


streetsView : Bool -> String -> String -> List Card -> List (Html Msg)
streetsView showLabel cursor width cards =
    case cards of
        _ :: _ :: _ :: [] ->
            [ streetView cursor (Just "Flop" |> Maybe.Extra.filter (always showLabel)) width cards ]

        f1 :: f2 :: f3 :: turn :: [] ->
            [ streetView cursor (Just "Flop" |> Maybe.Extra.filter (always showLabel)) width [ f1, f2, f3 ], streetView cursor (Just "Turn" |> Maybe.Extra.filter (always showLabel)) width [ turn ] ]

        f1 :: f2 :: f3 :: turn :: river :: [] ->
            [ streetView cursor (Just "Flop" |> Maybe.Extra.filter (always showLabel)) width [ f1, f2, f3 ], streetView cursor (Just "Turn" |> Maybe.Extra.filter (always showLabel)) width [ turn ], streetView cursor (Just "River" |> Maybe.Extra.filter (always showLabel)) width [ river ] ]

        _ ->
            []


boardSelectionModalView : Model -> Html Msg
boardSelectionModalView model =
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
                                            Card rank suit |> (\card -> cardView (Just <| ToggleBoardSelection card) (cardSelectState card model) "pointer" "6vw" card)
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


resultView : Int -> SharingPopoverStates -> SimulationResult -> Card.Config Msg
resultView index popoverStates result =
    Card.config [ Card.attrs [ Spacing.mb3, Html.Attributes.class "shadow" ] ]
        |> Card.headerH4 []
            [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween, Flex.alignItemsCenter ]
                [ if result.board |> List.isEmpty |> not then
                    boardView False "default" "30px" result.board

                  else
                    Html.text "Preflop"
                , Html.div []
                    [ Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "3px" ]
                        [ Popover.config
                            (Button.button
                                [ Button.outlineSecondary
                                , Button.onClick (CopyToClipboard index PokerStrategy)
                                , Button.attrs (Popover.onHover popoverStates.sharePs (PopoverStateSharing index PokerStrategy))
                                ]
                                [ Html.img [ Html.Attributes.src "images/pokerstrategy.svg", Html.Attributes.height 23 ] []
                                ]
                            )
                            |> Popover.top
                            |> Popover.content []
                                [ Html.text popoverStates.sharePsTooltipText ]
                            |> Popover.view popoverStates.sharePs
                        , Popover.config
                            (Button.button
                                [ Button.outlineSecondary
                                , Button.onClick (CopyToClipboard index TwoPlusTwo)
                                , Button.attrs (Popover.onHover popoverStates.share2plus2 (PopoverStateSharing index TwoPlusTwo))
                                ]
                                [ Html.text "2+2" ]
                            )
                            |> Popover.top
                            |> Popover.content []
                                [ Html.text popoverStates.share2plus2TooltipText ]
                            |> Popover.view popoverStates.share2plus2
                        , Popover.config
                            (Button.button
                                [ Button.outlineSecondary
                                , Button.onClick (CopyToClipboard index Markdown)
                                , Button.attrs (Popover.onHover popoverStates.shareMd (PopoverStateSharing index Markdown))
                                ]
                                [ Html.i [ Html.Attributes.class "fab fa-markdown" ] []
                                ]
                            )
                            |> Popover.top
                            |> Popover.content []
                                [ Html.text popoverStates.shareMdTooltipText ]
                            |> Popover.view popoverStates.shareMd
                        , Popover.config
                            (Button.button
                                [ Button.outlineSecondary
                                , Button.onClick (CopyToClipboard index URL)
                                , Button.attrs (Popover.onHover popoverStates.shareUrl (PopoverStateSharing index URL))
                                ]
                                [ Html.i [ Html.Attributes.class "fas fa-link" ] []
                                ]
                            )
                            |> Popover.top
                            |> Popover.content []
                                [ Html.text popoverStates.shareUrlTooltipText ]
                            |> Popover.view popoverStates.shareUrl
                        ]
                    ]
                ]
            ]
        |> Card.block []
            [ Block.custom <|
                Table.table
                    { options = [ Table.striped, Table.hover, Table.small ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [ Table.cellAttr (Html.Attributes.style "width" "20%") ] [ Html.text "Position" ]
                            , Table.th [ Table.cellAttr (Html.Attributes.style "width" "60%") ] [ Html.text "Range" ]
                            , Table.th [ Table.cellAttr (Html.Attributes.style "width" "20%") ] [ Html.text "Equity" ]
                            ]
                    , tbody =
                        Table.tbody []
                            (rowView UTG result.utg
                                ++ rowView MP result.mp
                                ++ rowView CO result.co
                                ++ rowView BU result.bu
                                ++ rowView SB result.sb
                                ++ rowView BB result.bb
                            )
                    }
            ]


rowView : Position -> Maybe ResultLine -> List (Table.Row Msg)
rowView position resultLine =
    case resultLine of
        Just result ->
            [ Table.tr []
                [ Table.td [] [ Html.text (position |> Position.toString) ]
                , Table.td [] [ Html.text (result.range |> HandOrCombo.toNormalizedString) ]
                , Table.td [] [ Html.text (Round.round 2 (result.equity * 100) ++ "%") ]
                ]
            ]

        Nothing ->
            []


cardSelectState : Card -> Model -> CardSelectState
cardSelectState card model =
    case model.cardUnderMouse of
        Just cum ->
            if
                cum
                    == card
                    && not model.ignoreCardHoverState
                    && (List.length model.boardSelection < 5 || (model.boardSelection |> List.member card))
            then
                CardSelectState.MouseOver

            else if model.boardSelection |> List.member card then
                CardSelectState.Selected

            else
                CardSelectState.NotSelected

        Nothing ->
            if model.boardSelection |> List.member card then
                CardSelectState.Selected

            else
                CardSelectState.NotSelected


rangeSelectionModalView : Model -> Html Msg
rangeSelectionModalView model =
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
                                ++ [ numberOfCombosView (model.rangeSelection |> List.map HandOrCombo.fromCombo) ]
                            )
                        ]
                    ]
                , Grid.col []
                    ((case RangesForm.board model.form of
                        [] ->
                            []

                        board ->
                            [ Html.div [ Flex.block, Flex.row, Spacing.mb2 ] [ boardView False "default" "27px" board ]
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
            Suit.Club ->
                Html.Attributes.src "images/playing-card-club-shape.svg"

            Suit.Spades ->
                Html.Attributes.src "images/playing-card-spade-shape.svg"

            Suit.Diamond ->
                Html.Attributes.src "images/playing-card-diamond-shape.svg"

            Suit.Heart ->
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


rangeSelectState : Hand -> Model -> RangeCellSelectState
rangeSelectState hand model =
    case model.handUnderMouse of
        Just handUnderMouse ->
            if handUnderMouse == hand && not model.ignoreRangeHoverState then
                case model.suitSelection of
                    Nothing ->
                        RangeCellSelectState.MouseOver

                    Just suitSelection ->
                        RangeCellSelectState.MouseOverDuringSuitSelection
                            (suitSelection.suited |> List.length)
                            (suitSelection.pairs |> List.length)
                            (suitSelection.offsuit |> List.length)

            else
                case model.rangeSelection |> Hand.numCombosOfHand hand of
                    Hand.All n ->
                        RangeCellSelectState.Selected n

                    Hand.None ->
                        RangeCellSelectState.NotSelected

                    Hand.Some n ->
                        RangeCellSelectState.PartiallySelected n

        Nothing ->
            case model.rangeSelection |> Hand.numCombosOfHand hand of
                Hand.All n ->
                    RangeCellSelectState.Selected n

                Hand.None ->
                    RangeCellSelectState.NotSelected

                Hand.Some n ->
                    RangeCellSelectState.PartiallySelected n


cellView : RangeCellSelectState -> String -> Hand -> Html Msg
cellView cs size hand =
    let
        ( ( fontColor, color, opacity ), maybeNum ) =
            case cs of
                RangeCellSelectState.Selected num ->
                    ( ( "white", "#9b5378", "1" ), Just num )

                RangeCellSelectState.NotSelected ->
                    if hand |> Hand.isOffsuit then
                        ( ( "#aaaaaa", "#eeeeee", "1" ), Nothing )

                    else if hand |> Hand.isSuited then
                        ( ( "#aaaaaa", "#dddddd", "1" ), Nothing )

                    else
                        ( ( "#aaaaaa", "#cccccc", "1" ), Nothing )

                RangeCellSelectState.MouseOver ->
                    ( ( "white", "#9b5378", "0.5" ), Nothing )

                RangeCellSelectState.PartiallySelected num ->
                    ( ( "white", "#db9713", "1" ), Just num )

                RangeCellSelectState.MouseOverDuringSuitSelection numSuited numPairs numOffsuit ->
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
