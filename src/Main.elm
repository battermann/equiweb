module Main exposing (main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as P
import Maybe.Extra
import Poker.Board as Board
import Poker.Card as Card exposing (Card)
import Poker.Position as Position exposing (Position(..))
import Poker.Range as Range
import Poker.Rank as Rank
import Poker.Suit as Suit exposing (Suit(..))
import RemoteData exposing (WebData)
import Result.Extra
import Round
import Svg
import Svg.Attributes
import Url.Builder


type alias SimulationRequestForm =
    { utg : Form.Field String
    , mp : Form.Field String
    , co : Form.Field String
    , bu : Form.Field String
    , sb : Form.Field String
    , bb : Form.Field String
    , board : Form.Field (List Card)
    }


setBoard : String -> SimulationRequestForm -> SimulationRequestForm
setBoard board form =
    { form | board = form.board |> Form.setValue Board.validate board }


setRange : Position -> String -> SimulationRequestForm -> SimulationRequestForm
setRange position range form =
    case position of
        UTG ->
            { form | utg = form.utg |> Form.setValue Range.rewrite range }

        MP ->
            { form | mp = form.mp |> Form.setValue Range.rewrite range }

        CO ->
            { form | co = form.co |> Form.setValue Range.rewrite range }

        BU ->
            { form | bu = form.bu |> Form.setValue Range.rewrite range }

        SB ->
            { form | sb = form.sb |> Form.setValue Range.rewrite range }

        BB ->
            { form | bb = form.bb |> Form.setValue Range.rewrite range }


initialForm : SimulationRequestForm
initialForm =
    { utg = { name = "UTG", value = "", validated = Range.rewrite "", edited = False }
    , mp = { name = "MP", value = "", validated = Range.rewrite "", edited = False }
    , co = { name = "CO", value = "", validated = Range.rewrite "", edited = False }
    , bu = { name = "BU", value = "", validated = Range.rewrite "", edited = False }
    , sb = { name = "SB", value = "", validated = Range.rewrite "", edited = False }
    , bb = { name = "BB", value = "", validated = Range.rewrite "", edited = False }
    , board = { name = "Board", value = "", validated = Ok [], edited = False }
    }


setAllFormFieldsToEdited : SimulationRequestForm -> SimulationRequestForm
setAllFormFieldsToEdited form =
    { form
        | utg = form.utg |> Form.setEdited
        , mp = form.mp |> Form.setEdited
        , co = form.co |> Form.setEdited
        , bu = form.bu |> Form.setEdited
        , sb = form.sb |> Form.setEdited
        , bb = form.bb |> Form.setEdited
        , board = form.board |> Form.setEdited
    }


type alias ResultLine =
    { range : String
    , equity : Float
    }


type alias SimulationResult =
    { board : List Card
    , utg : Maybe ResultLine
    , mp : Maybe ResultLine
    , co : Maybe ResultLine
    , bu : Maybe ResultLine
    , sb : Maybe ResultLine
    , bb : Maybe ResultLine
    }


type alias Model =
    { simulationRequestForm : SimulationRequestForm
    , currentApiResponse : WebData SimulationResult
    , results : List SimulationResult
    , boardSelectModalVisibility : Modal.Visibility
    , boardSelection : List Card
    , alert : Maybe String
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Model
init =
    { simulationRequestForm = initialForm
    , currentApiResponse = RemoteData.NotAsked
    , results = []
    , boardSelectModalVisibility = Modal.hidden
    , boardSelection = []
    , alert = Nothing
    }


type Msg
    = SimulationRequestSend
    | ApiResponseReceived (WebData ApiResponse)
    | RangeInput Position String
    | BoardInput String
    | RewriteRange Position
    | ShowBoardSelectModal
    | CloseBoardSelectModal
    | ToggleBoardSelection Card
    | ConfirmBoardSelection
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponseReceived result ->
            let
                updateSimulationResult ( position, range, equity ) simulationResult =
                    case position of
                        UTG ->
                            { simulationResult | utg = Just (ResultLine range equity) }

                        MP ->
                            { simulationResult | mp = Just (ResultLine range equity) }

                        CO ->
                            { simulationResult | co = Just (ResultLine range equity) }

                        BU ->
                            { simulationResult | bu = Just (ResultLine range equity) }

                        SB ->
                            { simulationResult | sb = Just (ResultLine range equity) }

                        BB ->
                            { simulationResult | bb = Just (ResultLine range equity) }

                sr =
                    result
                        |> RemoteData.map
                            (\res ->
                                [ ( model.simulationRequestForm.utg.validated |> Result.withDefault "", UTG )
                                , ( model.simulationRequestForm.mp.validated |> Result.withDefault "", MP )
                                , ( model.simulationRequestForm.co.validated |> Result.withDefault "", CO )
                                , ( model.simulationRequestForm.bu.validated |> Result.withDefault "", BU )
                                , ( model.simulationRequestForm.sb.validated |> Result.withDefault "", SB )
                                , ( model.simulationRequestForm.bb.validated |> Result.withDefault "", BB )
                                ]
                                    |> List.filter (\( r, _ ) -> not <| String.isEmpty r)
                                    |> List.map2 (\e ( r, p ) -> ( p, r, e ))
                                        ([ Just res.equityPlayer1
                                         , Just res.equityPlayer2
                                         , res.equityPlayer3
                                         , res.equityPlayer4
                                         , res.equityPlayer5
                                         , res.equityPlayer6
                                         ]
                                            |> Maybe.Extra.values
                                        )
                                    |> List.foldl
                                        updateSimulationResult
                                        (SimulationResult
                                            (model.simulationRequestForm.board.validated |> Result.withDefault [])
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                        )
                            )
            in
            ( { model | currentApiResponse = sr, results = (sr |> RemoteData.map List.singleton |> RemoteData.withDefault []) ++ model.results }, Cmd.none )

        SimulationRequestSend ->
            let
                ranges =
                    [ model.simulationRequestForm.utg.validated
                    , model.simulationRequestForm.mp.validated
                    , model.simulationRequestForm.co.validated
                    , model.simulationRequestForm.bu.validated
                    , model.simulationRequestForm.sb.validated
                    , model.simulationRequestForm.bb.validated
                    ]
                        |> List.map Result.toMaybe
                        |> Maybe.Extra.values
                        |> List.filter (not << String.isEmpty)

                formValid =
                    (Ok (\_ _ _ _ _ _ _ -> True)
                        |> Form.apply model.simulationRequestForm.board.validated
                        |> Form.apply model.simulationRequestForm.utg.validated
                        |> Form.apply model.simulationRequestForm.mp.validated
                        |> Form.apply model.simulationRequestForm.co.validated
                        |> Form.apply model.simulationRequestForm.bu.validated
                        |> Form.apply model.simulationRequestForm.sb.validated
                        |> Form.apply model.simulationRequestForm.bb.validated
                    )
                        |> Result.Extra.isOk
            in
            if not <| formValid then
                ( { model
                    | simulationRequestForm = setAllFormFieldsToEdited model.simulationRequestForm
                    , alert = Just "Cannot not understand some of the inputs. Please check and try to correct."
                  }
                , Cmd.none
                )

            else if (ranges |> List.length) < 2 then
                ( { model | alert = Just "Please enter at least 2 ranges." }, Cmd.none )

            else
                ( { model
                    | currentApiResponse = RemoteData.Loading
                    , simulationRequestForm = setAllFormFieldsToEdited model.simulationRequestForm
                    , alert = Nothing
                  }
                , sendSimulationRequest (model.simulationRequestForm.board.validated |> Result.withDefault []) ranges
                )

        RangeInput position str ->
            ( { model | simulationRequestForm = setRange position str model.simulationRequestForm }, Cmd.none )

        BoardInput str ->
            ( { model | simulationRequestForm = setBoard str model.simulationRequestForm }, Cmd.none )

        RewriteRange position ->
            let
                form =
                    model.simulationRequestForm
            in
            case position of
                UTG ->
                    ( { model | simulationRequestForm = { form | utg = Form.rewrite form.utg identity } }, Cmd.none )

                MP ->
                    ( { model | simulationRequestForm = { form | mp = Form.rewrite form.mp identity } }, Cmd.none )

                CO ->
                    ( { model | simulationRequestForm = { form | co = Form.rewrite form.co identity } }, Cmd.none )

                BU ->
                    ( { model | simulationRequestForm = { form | bu = Form.rewrite form.bu identity } }, Cmd.none )

                SB ->
                    ( { model | simulationRequestForm = { form | sb = Form.rewrite form.sb identity } }, Cmd.none )

                BB ->
                    ( { model | simulationRequestForm = { form | bb = Form.rewrite form.bb identity } }, Cmd.none )

        CloseBoardSelectModal ->
            ( { model | boardSelectModalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowBoardSelectModal ->
            ( { model | boardSelectModalVisibility = Modal.shown, boardSelection = model.simulationRequestForm.board.validated |> Result.withDefault [] }
            , Cmd.none
            )

        ToggleBoardSelection card ->
            if model.boardSelection |> List.member card then
                ( { model | boardSelection = model.boardSelection |> List.filter ((/=) card) }, Cmd.none )

            else if (model.boardSelection |> List.length) < 5 then
                ( { model | boardSelection = model.boardSelection ++ [ card ] }, Cmd.none )

            else
                ( model, Cmd.none )

        ConfirmBoardSelection ->
            ( { model
                | boardSelectModalVisibility = Modal.hidden
                , simulationRequestForm = setBoard (model.boardSelection |> List.map Card.toString |> String.concat) model.simulationRequestForm
              }
            , Cmd.none
            )

        Reset ->
            ( init |> (\m -> { m | results = model.results }), Cmd.none )



---- HTTP ----


type alias ApiResponse =
    { equityPlayer1 : Float
    , equityPlayer2 : Float
    , equityPlayer3 : Maybe Float
    , equityPlayer4 : Maybe Float
    , equityPlayer5 : Maybe Float
    , equityPlayer6 : Maybe Float
    }


simulationResponseDecoder : Decode.Decoder ApiResponse
simulationResponseDecoder =
    Decode.succeed ApiResponse
        |> P.required "equity_player_1" Decode.float
        |> P.required "equity_player_2" Decode.float
        |> P.required "equity_player_3" (Decode.nullable Decode.float)
        |> P.required "equity_player_4" (Decode.nullable Decode.float)
        |> P.required "equity_player_5" (Decode.nullable Decode.float)
        |> P.required "equity_player_6" (Decode.nullable Decode.float)


sendSimulationRequest : List Card -> List String -> Cmd Msg
sendSimulationRequest board ranges =
    Http.get
        { expect = Http.expectJson (RemoteData.fromResult >> ApiResponseReceived) simulationResponseDecoder
        , url =
            Url.Builder.crossOrigin "https://safe-shore-53897.herokuapp.com"
                [ "simulation" ]
                ([ Url.Builder.string "board" (board |> List.map Card.toString |> String.concat)
                 , Url.Builder.string "stdev_target" "0.001"
                 ]
                    ++ (ranges |> List.indexedMap (\i range -> Url.Builder.string ("range" ++ String.fromInt (i + 1)) range))
                )
        }



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Poker Equity Calculator"
    , body =
        [ Grid.container []
            [ Html.div []
                [ calculatorView model
                , modalView model
                ]
            ]
        ]
    }


loadingView : Html Msg
loadingView =
    Html.div []
        [ Html.div [ Flex.block, Flex.row, Flex.alignItemsCenter, Flex.justifyAround ] [ Spinner.spinner [ Spinner.large ] [] ]

        -- , Html.div [ Html.Attributes.align "center", Spacing.mt2 ] [ Html.text "You're request is processing. Sometimes this takes a while. But no worries, susequent requests will be faster. Thanks for your patience." ]
        ]


calculatorView : Model -> Html Msg
calculatorView model =
    Grid.row []
        [ Grid.col []
            [ Card.deck
                ((Card.config [ Card.attrs [ Spacing.mb3 ], Card.outlineDark ]
                    |> Card.headerH2 []
                        [ Html.div [ Flex.block, Flex.row, Flex.alignItemsStart ]
                            [ Html.img [ Html.Attributes.src "images/chip-icon.svg", Html.Attributes.width 40 ] []
                            , Html.div [ Html.Attributes.style "margin-top" "auto", Html.Attributes.style "margin-left" "7px", Html.Attributes.style "margin-bottom" "auto" ] [ Html.text "Equiweb" ]
                            ]
                        ]
                    |> Card.block []
                        [ Block.custom <|
                            case model.currentApiResponse of
                                RemoteData.Loading ->
                                    loadingView

                                RemoteData.Failure _ ->
                                    Html.div []
                                        [ Alert.simpleDanger [] [ Html.text "Something went wrong. Please try again." ]
                                        , inputFormView model
                                        ]

                                _ ->
                                    Html.div []
                                        ((model.alert |> Maybe.Extra.toList |> List.map (\msg -> Alert.simpleDanger [] [ Html.text msg ]))
                                            ++ [ inputFormView model
                                               ]
                                        )
                        ]
                 )
                    :: (model.results |> List.map resultView)
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


handRangePlaceholder : String
handRangePlaceholder =
    ""


validationFeedbackOutline : Form.Field a -> List (Input.Option msg)
validationFeedbackOutline field =
    case ( field.validated, field.edited ) of
        ( Ok _, True ) ->
            [ Input.success ]

        ( Err _, True ) ->
            [ Input.danger ]

        _ ->
            []


cardView : Maybe Msg -> String -> String -> String -> Card -> Html Msg
cardView msg opacity cursor refWidth card =
    let
        width =
            60

        height =
            width * 7.0 / 5.0

        color =
            case card.suit of
                Club ->
                    "forestgreen"

                Spades ->
                    "darkslategrey"

                Heart ->
                    "darkred"

                Diamond ->
                    "royalblue"
    in
    Html.div
        ([ Html.Attributes.style "width" refWidth
         , Html.Attributes.style "min-height" "40px"
         , Html.Attributes.style "min-width" "18px"
         , Html.Attributes.style "max-height" "80px"
         , Html.Attributes.style "max-width" "57px"
         , Html.Attributes.style "cursor" cursor
         , Html.Attributes.style "opacity" opacity
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
                , Svg.Attributes.y ((height * 0.6) |> String.fromFloat)
                , Svg.Attributes.fill "white"
                , Svg.Attributes.fontSize (width * 1.2 |> String.fromFloat)
                , Svg.Attributes.fontFamily "monospace"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.dominantBaseline "middle"
                ]
                [ Svg.text (card.rank |> Rank.toString) ]
            ]
        ]


inputFormView : Model -> Html Msg
inputFormView model =
    Form.form []
        [ formRow UTG model.simulationRequestForm.utg (model.currentApiResponse |> RemoteData.map (\r -> r.utg) |> RemoteData.toMaybe |> Maybe.andThen identity)
        , formRow MP model.simulationRequestForm.mp (model.currentApiResponse |> RemoteData.map (\r -> r.mp) |> RemoteData.toMaybe |> Maybe.andThen identity)
        , formRow CO model.simulationRequestForm.co (model.currentApiResponse |> RemoteData.map (\r -> r.co) |> RemoteData.toMaybe |> Maybe.andThen identity)
        , formRow BU model.simulationRequestForm.bu (model.currentApiResponse |> RemoteData.map (\r -> r.bu) |> RemoteData.toMaybe |> Maybe.andThen identity)
        , formRow SB model.simulationRequestForm.sb (model.currentApiResponse |> RemoteData.map (\r -> r.sb) |> RemoteData.toMaybe |> Maybe.andThen identity)
        , formRow BB model.simulationRequestForm.bb (model.currentApiResponse |> RemoteData.map (\r -> r.bb) |> RemoteData.toMaybe |> Maybe.andThen identity)
        , Form.row
            []
            [ Form.col [ Col.sm10 ]
                [ Form.group []
                    [ Form.label [] [ Html.text model.simulationRequestForm.board.name ]
                    , InputGroup.config
                        (InputGroup.text
                            (validationFeedbackOutline model.simulationRequestForm.board
                                ++ [ Input.value model.simulationRequestForm.board.value
                                   , Input.onInput BoardInput
                                   ]
                            )
                        )
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.outlineSecondary
                                , Button.onClick ShowBoardSelectModal
                                , Button.attrs [ Html.Attributes.tabindex -1 ]
                                ]
                                [ Html.img [ Html.Attributes.src "images/cards-icon.svg", Html.Attributes.width 20 ] [] ]
                            ]
                        |> InputGroup.view
                    ]
                ]
            ]
        , Form.row [ Row.attrs [ Spacing.mt2 ] ]
            [ Form.col []
                [ boardView "6vw" (model.simulationRequestForm.board.validated |> Result.withDefault []) ]
            ]
        , Form.row [ Row.attrs [ Spacing.mt2 ] ]
            [ Form.col []
                [ Html.div [ Flex.block, Flex.row ]
                    [ Button.button
                        [ Button.light
                        , Button.attrs [ Size.w100, Html.Attributes.style "margin-right" "2px" ]
                        , Button.onClick Reset
                        ]
                        [ Html.text "CLEAR ALL" ]
                    , Button.button
                        [ Button.success
                        , Button.attrs [ Size.w100, Html.Attributes.style "margin-left" "2px" ]
                        , Button.onClick SimulationRequestSend
                        ]
                        [ Html.text "RUN" ]
                    ]
                ]
            ]
        ]


formRow : Position -> Form.Field String -> Maybe ResultLine -> Html Msg
formRow position field result =
    Form.row []
        [ Form.col []
            [ Form.group []
                [ Form.label [] [ Html.text field.name ]
                , InputGroup.config
                    (InputGroup.text
                        ((if field.validated == Ok "" then
                            []

                          else
                            validationFeedbackOutline field
                         )
                            ++ [ Input.attrs [ Html.Attributes.placeholder handRangePlaceholder ]
                               , Input.value field.value
                               , Input.onInput (RangeInput position)
                               ]
                        )
                    )
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.outlineSecondary
                            , Button.onClick (RewriteRange position)
                            , Button.disabled (rewritable field |> not)
                            , Button.attrs [ Html.Attributes.tabindex -1 ]
                            ]
                            [ Html.text "Rewrite" ]
                        ]
                    |> InputGroup.view
                ]
            ]
        , Form.col [ Col.sm2 ]
            [ Form.group []
                [ Form.label [] [ Html.text "Equity" ]
                , Input.text [ Input.readonly True, Input.attrs [ Html.Attributes.tabindex -1 ], equityValueView result ]
                ]
            ]
        ]


rewritable : Form.Field String -> Bool
rewritable field =
    field.value /= (field.validated |> Result.withDefault "")


boardCardView : String -> Card -> Html Msg
boardCardView hight =
    cardView Nothing "1" "default" hight


boardView : String -> List Card -> Html Msg
boardView height cards =
    Html.div [ Flex.block, Flex.row ] (streetsView height cards)


streetView : String -> String -> List Card -> Html Msg
streetView label height cards =
    Html.div [ Html.Attributes.style "margin-right" "10px" ] [ Html.h5 [] [ Html.text label ], Html.div [ Flex.block, Flex.row ] (cards |> List.map (boardCardView height)) ]


streetsView : String -> List Card -> List (Html Msg)
streetsView height cards =
    case cards of
        _ :: _ :: _ :: [] ->
            [ streetView "Flop" height cards ]

        f1 :: f2 :: f3 :: turn :: [] ->
            [ streetView "Flop" height [ f1, f2, f3 ], streetView "Turn" height [ turn ] ]

        f1 :: f2 :: f3 :: turn :: river :: [] ->
            [ streetView "Flop" height [ f1, f2, f3 ], streetView "Turn" height [ turn ], streetView "River" height [ river ] ]

        _ ->
            []


modalView : Model -> Html Msg
modalView model =
    Modal.config CloseBoardSelectModal
        |> Modal.large
        |> Modal.attrs [ Html.Attributes.class "modal-fullscreen-lg-down" ]
        |> Modal.body []
            (Suit.all
                |> List.map
                    (\suit ->
                        Html.div
                            [ Flex.block
                            , Flex.row
                            , Flex.justifyCenter
                            , Flex.alignItemsCenter
                            ]
                            (Rank.all
                                |> List.reverse
                                |> List.map
                                    (\rank ->
                                        Html.div
                                            [ Flex.block
                                            , Flex.col
                                            , Flex.justifyAround
                                            , Flex.alignItemsCenter
                                            , Html.Attributes.style "user-select" "none"
                                            , Html.Attributes.style "margin" "1px"
                                            ]
                                            [ Card rank suit |> (\card -> cardView (Just <| ToggleBoardSelection card) (cardOpacity model card) "pointer" "6vw" card) ]
                                    )
                            )
                    )
            )
        |> Modal.footer []
            [ Button.button [ Button.light, Button.onClick CloseBoardSelectModal ] [ Html.text "CANCEL" ]
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


cardOpacity : Model -> Card -> String
cardOpacity model card =
    if model.boardSelection |> List.member card then
        "0.3"

    else
        "1"


resultView : SimulationResult -> Card.Config Msg
resultView result =
    Card.config [ Card.attrs [ Spacing.mb3 ], Card.outlineDark ]
        |> Card.headerH4 []
            [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
                [ if result.board |> List.isEmpty |> not then
                    Html.div [ Size.h100 ]
                        [ Html.text "Board: "
                        , Html.text (result.board |> List.map Card.toString |> String.concat)
                        ]

                  else
                    Html.text "Preflop"
                , boardView "10px" result.board
                ]
            ]
        |> Card.block []
            [ Block.custom <|
                Table.table
                    { options = [ Table.striped, Table.hover, Table.small ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [] [ Html.text "Position" ]
                            , Table.th [] [ Html.text "Range" ]
                            , Table.th [] [ Html.text "Equity" ]
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
                , Table.td [] [ Html.text result.range ]
                , Table.td [] [ Html.text (Round.round 2 (result.equity * 100) ++ "%") ]
                ]
            ]

        Nothing ->
            []
