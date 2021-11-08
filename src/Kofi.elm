module Kofi exposing (button)

import Html exposing (Html)
import Html.Attributes


button : Html msg
button =
    Html.a
        [ Html.Attributes.href "https://ko-fi.com/H2H16WAX3"
        , Html.Attributes.target "_blank"
        ]
        [ Html.img
            [ Html.Attributes.height 36
            , Html.Attributes.style "border" "0px"
            , Html.Attributes.style "height" "36px"
            , Html.Attributes.src "https://cdn.ko-fi.com/cdn/kofi4.png?v=3"
            , Html.Attributes.attribute "border" "0"
            , Html.Attributes.alt "Buy Me a Coffee at ko-fi.com"
            ]
            []
        ]
