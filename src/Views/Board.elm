module Views.Board exposing (view)

import Bootstrap.Utilities.Flex as Flex
import Html exposing (Html)
import Html.Attributes
import Model exposing (Msg(..))
import Poker.Card exposing (Card)
import Poker.Position exposing (Position(..))
import Poker.Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import Views.Card exposing (SelectState(..))


view : String -> String -> List Card -> Html Msg
view cursor width cards =
    Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "10px" ]
        (case cards of
            _ :: _ :: _ :: [] ->
                [ streetView cursor width (cards |> List.map Just)
                , streetView cursor width [ Nothing ]
                , streetView cursor width [ Nothing ]
                ]

            f1 :: f2 :: f3 :: turn :: [] ->
                [ streetView cursor width [ Just f1, Just f2, Just f3 ]
                , streetView cursor width [ Just turn ]
                , streetView cursor width [ Nothing ]
                ]

            f1 :: f2 :: f3 :: turn :: river :: [] ->
                [ streetView cursor width [ Just f1, Just f2, Just f3 ]
                , streetView cursor width [ Just turn ]
                , streetView cursor width [ Just river ]
                ]

            _ ->
                [ streetView cursor width [ Nothing, Nothing, Nothing ]
                , streetView cursor width [ Nothing ]
                , streetView cursor width [ Nothing ]
                ]
        )


streetView : String -> String -> List (Maybe Card) -> Html Msg
streetView cursor width cards =
    Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "2px" ] (cards |> List.map (Maybe.map (Views.Card.view Nothing Selected cursor width) >> Maybe.withDefault (Views.Card.empty Nothing "pointer" width)))
