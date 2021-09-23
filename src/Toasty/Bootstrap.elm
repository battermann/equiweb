module Toasty.Bootstrap exposing (Toast(..), config, view)

{-| This module provides a generic toast type with four variants (info, success, error and warning)
each one supports a title and optional secondary message.

**You need to load the provided `Defaults.css` file in your project**. `bounceInRight`
and `fadeOutRightBig` animations borrowed from [Animate.css](https://daneden.github.io/animate.css/)
project by Daniel Eden.

See a [demo](http://toasty-bootstrap.surge.sh/).


# Definition

@docs Toast, config, view

-}

import Bootstrap.Alert as Alert
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Toasty


{-| This theme defines toasts of four variants: "Info", "Success", "Warning" and "Error".
Each of them accepts an optional title and a message.
-}
type Toast
    = Info (Maybe String) String
    | Success (Maybe String) String
    | Warning (Maybe String) String
    | Error (Maybe String) String


{-| Default theme configuration.
-}
config : Toasty.Config msg
config =
    Toasty.config
        |> Toasty.transitionOutDuration 700
        |> Toasty.transitionOutAttrs transitionOutAttrs
        |> Toasty.transitionInAttrs transitionInAttrs
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.itemAttrs itemAttrs
        |> Toasty.delay 5000


containerAttrs : List (Html.Attribute msg)
containerAttrs =
    [ style "position" "fixed"
    , style "top" "0"
    , style "right" "0"
    , style "width" "100%"
    , style "max-width" "300px"
    , style "list-style-type" "none"
    , style "padding" "0"
    , style "margin" "0"
    ]


itemAttrs : List (Html.Attribute msg)
itemAttrs =
    [ style "margin" "1em 1em 0 1em"
    , style "max-height" "100px"
    , style "transition" "max-height 0.6s, margin-top 0.6s"
    ]


transitionInAttrs : List (Html.Attribute msg)
transitionInAttrs =
    [ class "animated bounceInRight"
    ]


transitionOutAttrs : List (Html.Attribute msg)
transitionOutAttrs =
    [ class "animated fadeOutRightBig"
    , style "max-height" "0"
    , style "margin-top" "0"
    ]


{-| Default theme view handling the three toast variants.
-}
view : Toast -> Html msg
view toast =
    case toast of
        Info title message ->
            simpleAlert Alert.simpleInfo title message

        Success title message ->
            simpleAlert Alert.simpleSuccess title message

        Warning title message ->
            simpleAlert Alert.simpleWarning title message

        Error title message ->
            simpleAlert Alert.simpleDanger title message


simpleAlert : (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg) -> Maybe String -> String -> Html msg
simpleAlert toHtml title message =
    div
        [ class "toasty-container" ]
        [ toHtml
            []
            [ case title of
                Just aTitle ->
                    Alert.h5 [] [ text aTitle ]

                Nothing ->
                    div [] []
            , text message
            ]
        ]
