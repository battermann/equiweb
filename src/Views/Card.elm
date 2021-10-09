module Views.Card exposing (SelectState(..), view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Model exposing (Msg(..))
import Poker.Card exposing (Card)
import Poker.Rank as Rank
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Svg
import Svg.Attributes
import Views.Icons


type SelectState
    = Selected
    | NotSelected
    | MouseOver


view : Maybe Msg -> SelectState -> String -> String -> Card -> Html Msg
view msg selectState cursor refWidth card =
    let
        ( color, icon ) =
            case card.suit of
                Club ->
                    ( "forestgreen", Views.Icons.club )

                Spades ->
                    ( "darkslategrey", Views.Icons.spade )

                Heart ->
                    ( "darkred", Views.Icons.heart )

                Diamond ->
                    ( "royalblue", Views.Icons.diamond )

        opacity =
            case selectState of
                Selected ->
                    "1"

                NotSelected ->
                    "0.5"

                MouseOver ->
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
