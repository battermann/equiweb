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
import Poker.Board as Board
import Poker.Card as Card exposing (Card)
import Poker.Range as Range
import Poker.Rank as Rank
import Poker.Suit as Suit exposing (Suit(..))
import RemoteData exposing (WebData)
import Round
import Svg
import Svg.Attributes
import Url.Builder


type alias SimulationResult =
    { equityPlayer1 : Float
    , equityPlayer2 : Float
    , equityPlayer3 : Maybe Float
    , equityPlayer4 : Maybe Float
    , equityPlayer5 : Maybe Float
    , equityPlayer6 : Maybe Float
    }


type Position
    = UTG
    | MP
    | CO
    | BU
    | SB
    | BB


type alias SimulationRequestForm =
    { utg : Form.Field String
    , mp : Form.Field String
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
            form

        BU ->
            form

        SB ->
            form

        BB ->
            form


initialForm : SimulationRequestForm
initialForm =
    { utg = { name = "UTG", value = "", validated = Range.rewrite "", edited = False }
    , mp = { name = "MP", value = "", validated = Range.rewrite "", edited = False }
    , board = { name = "Board", value = "", validated = Ok [], edited = False }
    }


setAllFormFieldsToEdited : SimulationRequestForm -> SimulationRequestForm
setAllFormFieldsToEdited form =
    { form
        | utg = form.utg |> Form.setEdited
        , mp = form.mp |> Form.setEdited
        , board = form.board |> Form.setEdited
    }


type alias ResultLine =
    { range : String
    , equity : Float
    }


type alias Model =
    { simulationRequestForm : SimulationRequestForm
    , currentSimulationResult : WebData SimulationResult
    , results : List (List ResultLine)
    , boardSelectModalVisibility : Modal.Visibility
    , boardSelection : List Card
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
    , currentSimulationResult = RemoteData.NotAsked
    , results = []
    , boardSelectModalVisibility = Modal.hidden
    , boardSelection = []
    }


type Msg
    = SimulationRequestSend
    | SimulationResultReceived (WebData SimulationResult)
    | RangeInput Position String
    | BoardInput String
    | RewriteRange Position
    | ShowBoardSelectModal
    | CloseBoardSelectModal
    | ToggleBoardSelection Card
    | ConfirmBoardSelection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SimulationResultReceived (RemoteData.Success result) ->
            ( { model
                | currentSimulationResult = RemoteData.Success result
              }
            , Cmd.none
            )

        SimulationResultReceived result ->
            ( { model | currentSimulationResult = result }, Cmd.none )

        SimulationRequestSend ->
            let
                validatedRequest =
                    Ok SimulationRequest
                        |> Form.apply model.simulationRequestForm.board.validated
                        |> Form.apply model.simulationRequestForm.utg.validated
                        |> Form.apply model.simulationRequestForm.mp.validated
            in
            case validatedRequest of
                Ok req ->
                    ( { model
                        | currentSimulationResult = RemoteData.Loading
                        , simulationRequestForm = setAllFormFieldsToEdited model.simulationRequestForm
                      }
                    , sendSimulationRequest req
                    )

                Err _ ->
                    ( { model | simulationRequestForm = setAllFormFieldsToEdited model.simulationRequestForm }, Cmd.none )

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
                    ( model, Cmd.none )

                BU ->
                    ( model, Cmd.none )

                SB ->
                    ( model, Cmd.none )

                BB ->
                    ( model, Cmd.none )

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
                ( { model | boardSelection = card :: model.boardSelection }, Cmd.none )

            else
                ( model, Cmd.none )

        ConfirmBoardSelection ->
            ( { model
                | boardSelectModalVisibility = Modal.hidden
                , simulationRequestForm = setBoard (model.boardSelection |> List.map Card.toString |> String.concat) model.simulationRequestForm
              }
            , Cmd.none
            )



---- HTTP ----


type alias SimulationRequest =
    { board : List Card
    , range1 : String
    , range2 : String
    }


simulationResponseDecoder : Decode.Decoder SimulationResult
simulationResponseDecoder =
    Decode.succeed SimulationResult
        |> P.required "equity_player_1" Decode.float
        |> P.required "equity_player_2" Decode.float
        |> P.required "equity_player_3" (Decode.nullable Decode.float)
        |> P.required "equity_player_4" (Decode.nullable Decode.float)
        |> P.required "equity_player_5" (Decode.nullable Decode.float)
        |> P.required "equity_player_6" (Decode.nullable Decode.float)


sendSimulationRequest : SimulationRequest -> Cmd Msg
sendSimulationRequest request =
    Http.get
        { expect = Http.expectJson (RemoteData.fromResult >> SimulationResultReceived) simulationResponseDecoder
        , url =
            Url.Builder.crossOrigin "https://safe-shore-53897.herokuapp.com"
                [ "simulation" ]
                [ Url.Builder.string "range1" request.range1
                , Url.Builder.string "range2" request.range2
                , Url.Builder.string "board" (request.board |> List.map Card.toString |> String.concat)
                , Url.Builder.string "stdev_target" "0.001"
                ]
        }



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Poker Equity Calculator"
    , body =
        [ Html.div []
            [ calculatorView model
            , modalView model
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
                [ Card.config []
                    |> Card.headerH2 []
                        [ Html.div [ Flex.block, Flex.row, Flex.alignItemsStart ]
                            [ Html.img [ Html.Attributes.src "images/chip-icon.svg", Html.Attributes.width 40 ] []
                            , Html.div [ Html.Attributes.style "margin-top" "auto", Html.Attributes.style "margin-left" "7px", Html.Attributes.style "margin-bottom" "auto" ] [ Html.text "Equiweb" ]
                            ]
                        ]
                    |> Card.block []
                        [ Block.custom <|
                            case model.currentSimulationResult of
                                RemoteData.Loading ->
                                    loadingView

                                RemoteData.Failure _ ->
                                    Html.div []
                                        [ Alert.simpleDanger [] [ Html.text "Something went wrong. Please try again." ]
                                        , inputFormView model
                                        ]

                                _ ->
                                    inputFormView model
                        ]

                -- , Card.config []
                --     |> Card.block []
                --         []
                ]
            ]
        ]


equityValue : Position -> Model -> Maybe Float
equityValue position model =
    case model.currentSimulationResult of
        RemoteData.NotAsked ->
            Nothing

        RemoteData.Loading ->
            Nothing

        RemoteData.Failure _ ->
            Nothing

        RemoteData.Success { equityPlayer1, equityPlayer2, equityPlayer3, equityPlayer4, equityPlayer5, equityPlayer6 } ->
            case position of
                UTG ->
                    Just equityPlayer1

                MP ->
                    Just equityPlayer2

                CO ->
                    equityPlayer3

                BU ->
                    equityPlayer4

                SB ->
                    equityPlayer5

                BB ->
                    equityPlayer6


equityValueView : Position -> Model -> Input.Option msg
equityValueView position model =
    case equityValue position model of
        Nothing ->
            Input.value ""

        Just value ->
            Input.value (Round.round 2 (100 * value) ++ " %")


handRangePlaceholder : String
handRangePlaceholder =
    "Hand Range (e.g. QQ+, AK)"


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
         , Html.Attributes.style "min-width" "29px"
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
                [ Svg.Attributes.x ((width * 0.13) |> String.fromFloat)
                , Svg.Attributes.y ((width * 1.2) |> String.fromFloat)
                , Svg.Attributes.fill "white"
                , Svg.Attributes.fontSize (width * 1.2 |> String.fromFloat)
                , Svg.Attributes.fontFamily "monospace"
                ]
                [ Svg.text (card.rank |> Rank.toString) ]
            ]
        ]


inputFormView : Model -> Html Msg
inputFormView model =
    Form.form []
        [ Form.row []
            [ Form.col []
                [ Form.group []
                    [ Form.label [] [ Html.text model.simulationRequestForm.utg.name ]
                    , InputGroup.config
                        (InputGroup.text
                            (validationFeedbackOutline model.simulationRequestForm.utg
                                ++ [ Input.attrs [ Html.Attributes.placeholder handRangePlaceholder ]
                                   , Input.value model.simulationRequestForm.utg.value
                                   , Input.onInput (RangeInput UTG)
                                   ]
                            )
                        )
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.outlineSecondary
                                , Button.onClick (RewriteRange UTG)
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
                    , Input.text
                        [ Input.readonly True
                        , Input.attrs [ Html.Attributes.tabindex -1 ]
                        , equityValueView UTG model
                        ]
                    ]
                ]
            ]
        , Form.row []
            [ Form.col []
                [ Form.group []
                    [ Form.label [] [ Html.text model.simulationRequestForm.mp.name ]
                    , InputGroup.config
                        (InputGroup.text
                            (validationFeedbackOutline model.simulationRequestForm.mp
                                ++ [ Input.attrs [ Html.Attributes.placeholder handRangePlaceholder ]
                                   , Input.value model.simulationRequestForm.mp.value
                                   , Input.onInput (RangeInput MP)
                                   ]
                            )
                        )
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.outlineSecondary
                                , Button.onClick (RewriteRange MP)
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
                    , Input.text [ Input.readonly True, Input.attrs [ Html.Attributes.tabindex -1 ], equityValueView MP model ]
                    ]
                ]
            ]
        , Form.row
            []
            [ Form.col [ Col.sm10 ]
                [ Form.group []
                    [ Form.label [] [ Html.text model.simulationRequestForm.board.name ]
                    , InputGroup.config
                        (InputGroup.text
                            (validationFeedbackOutline model.simulationRequestForm.board
                                ++ [ Input.attrs [ Html.Attributes.placeholder "Board (e.g. 3h4h4c)" ]
                                   , Input.value model.simulationRequestForm.board.value
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
                [ Html.div
                    [ Flex.block
                    , Flex.row
                    ]
                    (model.simulationRequestForm.board.validated |> Result.withDefault [] |> List.map (cardView Nothing "1" "default" "6vmin"))
                ]
            ]
        , Form.row [ Row.attrs [ Spacing.mt2 ] ]
            [ Form.col []
                [ Button.button
                    [ Button.success
                    , Button.attrs [ Size.w100 ]
                    , Button.onClick SimulationRequestSend
                    ]
                    [ Html.text "Run" ]
                ]
            ]
        ]


modalView : Model -> Html Msg
modalView model =
    Modal.config CloseBoardSelectModal
        |> Modal.large
        |> Modal.body []
            (Suit.all
                |> List.map
                    (\suit ->
                        Html.div
                            [ Flex.block
                            , Flex.row
                            , Flex.justifyCenter
                            , Flex.alignItemsCenter
                            , Spacing.mb1
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
                                            ]
                                            [ Card rank suit |> (\card -> cardView (Just <| ToggleBoardSelection card) (cardOpacity model card) "pointer" "6vmin" card) ]
                                    )
                            )
                    )
            )
        |> Modal.footer []
            [ Button.button [ Button.light, Button.onClick CloseBoardSelectModal ] [ Html.text "Cancel" ]
            , Button.button
                [ Button.success
                , Button.onClick ConfirmBoardSelection
                , Button.disabled (isBoardSelectionValid model |> not)
                ]
                [ Html.text "Confirm" ]
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
