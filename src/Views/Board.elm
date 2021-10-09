module Views.Board exposing (view)

import Bootstrap.Utilities.Flex as Flex
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Model exposing (Msg(..))
import Poker.Card exposing (Card)
import Poker.Position exposing (Position(..))
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Views.Card exposing (SelectState(..))


view : Bool -> String -> String -> List Card -> Html Msg
view showLabel cursor width cards =
    Html.div [ Flex.block, Flex.row ]
        (case cards of
            _ :: _ :: _ :: [] ->
                [ streetView cursor (Just "Flop" |> Maybe.Extra.filter (always showLabel)) width cards ]

            f1 :: f2 :: f3 :: turn :: [] ->
                [ streetView cursor (Just "Flop" |> Maybe.Extra.filter (always showLabel)) width [ f1, f2, f3 ], streetView cursor (Just "Turn" |> Maybe.Extra.filter (always showLabel)) width [ turn ] ]

            f1 :: f2 :: f3 :: turn :: river :: [] ->
                [ streetView cursor (Just "Flop" |> Maybe.Extra.filter (always showLabel)) width [ f1, f2, f3 ], streetView cursor (Just "Turn" |> Maybe.Extra.filter (always showLabel)) width [ turn ], streetView cursor (Just "River" |> Maybe.Extra.filter (always showLabel)) width [ river ] ]

            _ ->
                []
        )


streetView : String -> Maybe String -> String -> List Card -> Html Msg
streetView cursor maybeLabel width cards =
    Html.div [ Html.Attributes.style "margin-right" "10px" ]
        ((maybeLabel |> Maybe.Extra.unwrap [] (\label -> [ Html.h6 [ Flex.block, Flex.row, Flex.justifyCenter ] [ Html.text label ] ]))
            ++ [ Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "2px" ] (cards |> List.map (Views.Card.view Nothing Selected cursor width))
               ]
        )
