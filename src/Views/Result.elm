module Views.Result exposing (view)

import Bootstrap.Alt.Popover as Popover
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Table as Table
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Html
import Html.Attributes
import Model exposing (Msg(..), ResultLine, SharingPopoverStates, SimulationResult)
import Poker.HandOrCombo as HandOrCombo
import Poker.Position as Position exposing (Position(..))
import Ports exposing (SharingType(..))
import Round
import Views.Board


view : Int -> SharingPopoverStates -> SimulationResult -> Card.Config Msg
view index popoverStates result =
    Card.config [ Card.attrs [ Spacing.mb3, Html.Attributes.class "shadow" ] ]
        |> Card.headerH4 []
            [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween, Flex.alignItemsCenter, Html.Attributes.style "gap" "15px" ]
                [ if result.board |> List.isEmpty |> not then
                    Views.Board.view "default" "30px" result.board

                  else
                    Html.text "Preflop"
                , Html.div []
                    [ Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "3px", Flex.wrap, Flex.justifyEnd ]
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
            , Block.custom <|
                Html.div [ Flex.block, Flex.row, Flex.justifyEnd ]
                    [ Button.button [ Button.danger, Button.onClick (RemoveResult index) ]
                        [ Html.div
                            [ Flex.block
                            , Flex.row
                            , Flex.justifyCenter
                            , Flex.alignItemsCenter
                            , Html.Attributes.style "gap" "10px"
                            ]
                            [ Html.i [ Html.Attributes.class "far fa-trash-alt", Html.Attributes.style "color" "white" ] []
                            , Html.text "Remove"
                            ]
                        ]
                    ]
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
