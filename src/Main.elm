module Main exposing (main)

import Bootstrap.Alert as Alert
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
import Bootstrap.Modal as Modal
import Bootstrap.Progress as Progress
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (UrlRequest)
import Browser.Events
import Browser.Navigation as Navigation
import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as P
import Keyboard exposing (RawKey)
import List.Extra
import Maybe.Extra
import Poker.Board as Board
import Poker.Card as Card exposing (Card)
import Poker.Combo as Combo
import Poker.Data as Data
import Poker.Hand as Hand exposing (Hand)
import Poker.Position as Position exposing (Position(..))
import Poker.Range as Range exposing (HandRange)
import Poker.Rank as Rank
import Poker.Suit as Suit exposing (Suit(..))
import Ports
import RemoteData exposing (WebData)
import Result.Extra
import Round
import Svg
import Svg.Attributes
import Url exposing (Url)
import Url.Builder
import Url.Parser as UrlParser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias SimulationRequestForm =
    { utg : Form.Field (List HandRange)
    , mp : Form.Field (List HandRange)
    , co : Form.Field (List HandRange)
    , bu : Form.Field (List HandRange)
    , sb : Form.Field (List HandRange)
    , bb : Form.Field (List HandRange)
    , board : Form.Field (List Card)
    }


setBoard : String -> SimulationRequestForm -> SimulationRequestForm
setBoard board form =
    { form | board = form.board |> Form.setValue Board.validate board }


setRange : Position -> String -> SimulationRequestForm -> SimulationRequestForm
setRange position range form =
    case position of
        UTG ->
            { form | utg = form.utg |> Form.setValue Range.parseAndNormalize range }

        MP ->
            { form | mp = form.mp |> Form.setValue Range.parseAndNormalize range }

        CO ->
            { form | co = form.co |> Form.setValue Range.parseAndNormalize range }

        BU ->
            { form | bu = form.bu |> Form.setValue Range.parseAndNormalize range }

        SB ->
            { form | sb = form.sb |> Form.setValue Range.parseAndNormalize range }

        BB ->
            { form | bb = form.bb |> Form.setValue Range.parseAndNormalize range }


initialForm : SimulationRequestForm
initialForm =
    { utg = { name = "UTG", value = "", validated = Range.parseAndNormalize "", edited = False }
    , mp = { name = "MP", value = "", validated = Range.parseAndNormalize "", edited = False }
    , co = { name = "CO", value = "", validated = Range.parseAndNormalize "", edited = False }
    , bu = { name = "BU", value = "", validated = Range.parseAndNormalize "", edited = False }
    , sb = { name = "SB", value = "", validated = Range.parseAndNormalize "", edited = False }
    , bb = { name = "BB", value = "", validated = Range.parseAndNormalize "", edited = False }
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
    { range : List HandRange
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


type Mouse
    = Released
    | Pressed


type alias Model =
    { navKey : Navigation.Key
    , simulationRequestForm : SimulationRequestForm
    , currentApiResponse : WebData SimulationResult
    , results : List SimulationResult
    , boardSelectModalVisibility : Modal.Visibility
    , rangeSelectionModalVisibility : Modal.Visibility
    , boardSelection : List Card
    , rangeSelection : List HandRange
    , rangeSelectionPosition : Position
    , alert : Maybe String
    , cardUnderMouse : Maybe Card
    , ignoreCardHoverState : Bool
    , mouse : Mouse
    , handUnderMouse : Maybe Hand
    , ignoreRangeHoverState : Bool
    , rangeDropdownStateUtg : Dropdown.State
    , rangeDropdownStateMp : Dropdown.State
    , rangeDropdownStateCo : Dropdown.State
    , rangeDropdownStateBu : Dropdown.State
    , rangeDropdownStateSb : Dropdown.State
    , rangeDropdownStateBb : Dropdown.State
    , location : Url
    }


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url key =
    let
        maybeForm =
            UrlParser.parse urlParser url
    in
    Cmd.none
        |> Tuple.pair
            { simulationRequestForm = maybeForm |> Maybe.withDefault initialForm
            , currentApiResponse = RemoteData.NotAsked
            , results = []
            , boardSelectModalVisibility = Modal.hidden
            , rangeSelectionModalVisibility = Modal.hidden
            , boardSelection = []
            , rangeSelection = []
            , rangeSelectionPosition = UTG
            , alert = Nothing
            , cardUnderMouse = Nothing
            , ignoreCardHoverState = False
            , mouse = Released
            , handUnderMouse = Nothing
            , ignoreRangeHoverState = False
            , navKey = key
            , rangeDropdownStateUtg = Dropdown.initialState
            , rangeDropdownStateMp = Dropdown.initialState
            , rangeDropdownStateCo = Dropdown.initialState
            , rangeDropdownStateBu = Dropdown.initialState
            , rangeDropdownStateSb = Dropdown.initialState
            , rangeDropdownStateBb = Dropdown.initialState
            , location = url
            }


type Msg
    = ApiResponseReceived (WebData ApiResponse)
    | BoardInput String
    | CardHover (Maybe Card)
    | ClearBoard
    | ClearRange
    | ClickedLink UrlRequest
    | CloseBoardSelectModal
    | CloseRangeSelectionModal
    | ConfirmBoardSelection
    | ConfirmRangeSelection
    | CopyUrlToClipboard
    | HandHover (Maybe Hand)
    | KeyDown RawKey
    | MouseDown
    | MouseUp
    | RangeDropDownMsg Position Dropdown.State
    | RangeInput Position String
    | RewriteRange Position
    | SelectOffsuitAces
    | SelectOffsuitBroadways
    | SelectPairs
    | SelectPresetRange Position String
    | SelectSuitedAces
    | SelectSuitedBroadways
    | SendSimulationRequest
    | ShowBoardSelectModal
    | ShowRangeSelectionModal Position
    | ToggleBoardSelection Card
    | UrlChange Url


handleApiResponse : Model -> WebData ApiResponse -> ( Model, Cmd Msg )
handleApiResponse model result =
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
                        [ ( model.simulationRequestForm.utg.validated |> Result.withDefault [], UTG )
                        , ( model.simulationRequestForm.mp.validated |> Result.withDefault [], MP )
                        , ( model.simulationRequestForm.co.validated |> Result.withDefault [], CO )
                        , ( model.simulationRequestForm.bu.validated |> Result.withDefault [], BU )
                        , ( model.simulationRequestForm.sb.validated |> Result.withDefault [], SB )
                        , ( model.simulationRequestForm.bb.validated |> Result.withDefault [], BB )
                        ]
                            |> List.filter (\( r, _ ) -> not <| List.isEmpty r)
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


sendSimulationRequest : Model -> ( Model, Cmd Msg )
sendSimulationRequest model =
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
                |> List.filter (not << List.isEmpty)
    in
    if not <| isFormValid model.simulationRequestForm then
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
        , sendSimulationRequestHttp (model.simulationRequestForm.board.validated |> Result.withDefault []) ranges
        )


isFormValid : SimulationRequestForm -> Bool
isFormValid form =
    (Ok (\_ _ _ _ _ _ _ -> True)
        |> Form.apply form.board.validated
        |> Form.apply form.utg.validated
        |> Form.apply form.mp.validated
        |> Form.apply form.co.validated
        |> Form.apply form.bu.validated
        |> Form.apply form.sb.validated
        |> Form.apply form.bb.validated
    )
        |> Result.Extra.isOk


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            Cmd.none
                |> Tuple.pair
                    { model
                        | simulationRequestForm = UrlParser.parse urlParser url |> Maybe.withDefault initialForm
                        , currentApiResponse = RemoteData.NotAsked
                        , boardSelectModalVisibility = Modal.hidden
                        , rangeSelectionModalVisibility = Modal.hidden
                        , boardSelection = []
                        , rangeSelection = []
                        , rangeSelectionPosition = UTG
                        , cardUnderMouse = Nothing
                        , ignoreCardHoverState = False
                        , mouse = Released
                        , handUnderMouse = Nothing
                        , ignoreRangeHoverState = False
                        , location = url
                    }

        ApiResponseReceived result ->
            handleApiResponse model result

        SendSimulationRequest ->
            sendSimulationRequest model
                |> updateUrl

        RangeInput position str ->
            ( { model | simulationRequestForm = setRange position str model.simulationRequestForm, currentApiResponse = RemoteData.NotAsked }, Cmd.none )

        BoardInput str ->
            ( { model | simulationRequestForm = setBoard str model.simulationRequestForm, currentApiResponse = RemoteData.NotAsked }, Cmd.none )

        RewriteRange position ->
            ( { model | simulationRequestForm = rewrite position model.simulationRequestForm }, Cmd.none )

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
                ( { model | boardSelection = model.boardSelection |> List.filter ((/=) card), ignoreCardHoverState = True }, Cmd.none )

            else if (model.boardSelection |> List.length) < 5 then
                ( { model | boardSelection = model.boardSelection ++ [ card ], ignoreCardHoverState = True }, Cmd.none )

            else
                ( model, Cmd.none )

        ConfirmBoardSelection ->
            confirmBoardSelection model

        KeyDown rawKey ->
            case Keyboard.anyKeyUpper rawKey of
                Just Keyboard.Escape ->
                    ( { model
                        | boardSelectModalVisibility = Modal.hidden
                        , rangeSelectionModalVisibility = Modal.hidden
                      }
                    , Cmd.none
                    )

                Just Keyboard.Enter ->
                    if model.boardSelectModalVisibility == Modal.shown then
                        confirmBoardSelection model

                    else if model.rangeSelectionModalVisibility == Modal.shown then
                        confirmRangeSelection model.rangeSelectionPosition model

                    else
                        sendSimulationRequest model
                            |> updateUrl

                _ ->
                    ( model, Cmd.none )

        CardHover maybeCard ->
            ( { model | cardUnderMouse = maybeCard, ignoreCardHoverState = False }, Cmd.none )

        ClearBoard ->
            ( { model | boardSelection = [] }, Cmd.none )

        ShowRangeSelectionModal position ->
            let
                range =
                    case position of
                        UTG ->
                            model.simulationRequestForm.utg.validated |> Result.withDefault []

                        MP ->
                            model.simulationRequestForm.mp.validated |> Result.withDefault []

                        CO ->
                            model.simulationRequestForm.co.validated |> Result.withDefault []

                        BU ->
                            model.simulationRequestForm.bu.validated |> Result.withDefault []

                        SB ->
                            model.simulationRequestForm.sb.validated |> Result.withDefault []

                        BB ->
                            model.simulationRequestForm.bb.validated |> Result.withDefault []
            in
            ( { model
                | rangeSelectionModalVisibility = Modal.shown
                , rangeSelection = range
                , rangeSelectionPosition = position
              }
            , Cmd.none
            )

        CloseRangeSelectionModal ->
            ( { model | rangeSelectionModalVisibility = Modal.hidden, rangeSelection = [] }, Cmd.none )

        ConfirmRangeSelection ->
            confirmRangeSelection model.rangeSelectionPosition model

        MouseDown ->
            case model.handUnderMouse of
                Just hand ->
                    ( toggleHandSelection (Range.fromHand hand) { model | mouse = Pressed }, Cmd.none )

                Nothing ->
                    ( { model | mouse = Pressed }, Cmd.none )

        MouseUp ->
            ( { model | mouse = Released }, Cmd.none )

        ClearRange ->
            ( { model | rangeSelection = [] }, Cmd.none )

        HandHover (Just hand) ->
            if model.mouse == Pressed then
                ( toggleHandSelection (Range.fromHand hand) { model | handUnderMouse = Just hand, ignoreRangeHoverState = False }, Cmd.none )

            else
                ( { model | handUnderMouse = Just hand, ignoreRangeHoverState = False }, Cmd.none )

        HandHover Nothing ->
            ( { model | handUnderMouse = Nothing, ignoreRangeHoverState = False }, Cmd.none )

        SelectPairs ->
            ( { model | rangeSelection = model.rangeSelection ++ Range.pairs |> List.Extra.unique }, Cmd.none )

        SelectSuitedAces ->
            ( { model | rangeSelection = model.rangeSelection ++ Range.suitedAces |> List.Extra.unique }, Cmd.none )

        SelectSuitedBroadways ->
            ( { model | rangeSelection = model.rangeSelection ++ Range.suitedBroadways |> List.Extra.unique }, Cmd.none )

        SelectOffsuitAces ->
            ( { model | rangeSelection = model.rangeSelection ++ Range.offsuitAces |> List.Extra.unique }, Cmd.none )

        SelectOffsuitBroadways ->
            ( { model | rangeSelection = model.rangeSelection ++ Range.offsuitBroadways |> List.Extra.unique }, Cmd.none )

        RangeDropDownMsg position state ->
            case position of
                UTG ->
                    ( { model | rangeDropdownStateUtg = state }, Cmd.none )

                MP ->
                    ( { model | rangeDropdownStateMp = state }, Cmd.none )

                CO ->
                    ( { model | rangeDropdownStateCo = state }, Cmd.none )

                BU ->
                    ( { model | rangeDropdownStateBu = state }, Cmd.none )

                SB ->
                    ( { model | rangeDropdownStateSb = state }, Cmd.none )

                BB ->
                    ( { model | rangeDropdownStateBb = state }, Cmd.none )

        SelectPresetRange position range ->
            ( { model
                | simulationRequestForm = setRange position range model.simulationRequestForm |> rewrite position
                , currentApiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )

        CopyUrlToClipboard ->
            ( model, Ports.copyToClipboard (model.location |> Url.toString) )


toggleHandSelection : HandRange -> Model -> Model
toggleHandSelection handRange model =
    if model.rangeSelection |> List.member handRange then
        { model | rangeSelection = model.rangeSelection |> List.filter ((/=) handRange), ignoreRangeHoverState = True }

    else
        { model | rangeSelection = model.rangeSelection ++ [ handRange ], ignoreRangeHoverState = True }


rewriteBoard : SimulationRequestForm -> SimulationRequestForm
rewriteBoard form =
    { form | board = Form.rewrite form.board (List.map Card.toString >> String.concat) }


rewrite : Position -> SimulationRequestForm -> SimulationRequestForm
rewrite position form =
    case position of
        UTG ->
            { form | utg = Form.rewrite form.utg Range.rangesToNormalizedString }

        MP ->
            { form | mp = Form.rewrite form.mp Range.rangesToNormalizedString }

        CO ->
            { form | co = Form.rewrite form.co Range.rangesToNormalizedString }

        BU ->
            { form | bu = Form.rewrite form.bu Range.rangesToNormalizedString }

        SB ->
            { form | sb = Form.rewrite form.sb Range.rangesToNormalizedString }

        BB ->
            { form | bb = Form.rewrite form.bb Range.rangesToNormalizedString }


confirmRangeSelection : Position -> Model -> ( Model, Cmd Msg )
confirmRangeSelection position model =
    let
        form =
            setRange position (model.rangeSelection |> List.map Range.toString |> String.join ",") model.simulationRequestForm
                |> rewrite position
    in
    ( { model
        | rangeSelectionModalVisibility = Modal.hidden
        , simulationRequestForm = form
        , rangeSelection = []
        , currentApiResponse = RemoteData.NotAsked
      }
    , Cmd.none
    )


toSimulationRequestForm :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> SimulationRequestForm
toSimulationRequestForm maybeUtg maybeMp maybeCo maybeBu maybeSb maybeBb maybeBaord =
    initialForm
        |> setRange UTG (maybeUtg |> Maybe.withDefault "")
        |> rewrite UTG
        |> setRange MP (maybeMp |> Maybe.withDefault "")
        |> rewrite MP
        |> setRange CO (maybeCo |> Maybe.withDefault "")
        |> rewrite CO
        |> setRange BU (maybeBu |> Maybe.withDefault "")
        |> rewrite BU
        |> setRange SB (maybeSb |> Maybe.withDefault "")
        |> rewrite SB
        |> setRange BB (maybeBb |> Maybe.withDefault "")
        |> rewrite BB
        |> setBoard (maybeBaord |> Maybe.withDefault "")
        |> rewriteBoard


urlParser : Parser (SimulationRequestForm -> a) a
urlParser =
    UrlParser.map toSimulationRequestForm
        (UrlParser.top
            <?> Query.string "utg"
            <?> Query.string "mp"
            <?> Query.string "co"
            <?> Query.string "bu"
            <?> Query.string "sb"
            <?> Query.string "bb"
            <?> Query.string "board"
        )


updateUrl : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateUrl ( model, cmd ) =
    if model.simulationRequestForm |> isFormValid then
        Cmd.batch
            [ cmd
            , Navigation.pushUrl model.navKey
                (Url.Builder.absolute []
                    (rangeQueryParameter UTG model.simulationRequestForm.utg
                        ++ rangeQueryParameter MP model.simulationRequestForm.mp
                        ++ rangeQueryParameter CO model.simulationRequestForm.co
                        ++ rangeQueryParameter BU model.simulationRequestForm.bu
                        ++ rangeQueryParameter SB model.simulationRequestForm.sb
                        ++ rangeQueryParameter BB model.simulationRequestForm.bb
                        ++ boardQueryParameter model.simulationRequestForm.board
                    )
                )
            ]
            |> Tuple.pair model

    else
        ( model, cmd )


boardQueryParameter : Form.Field (List Card) -> List Url.Builder.QueryParameter
boardQueryParameter board =
    case board.validated of
        Err _ ->
            []

        Ok [] ->
            []

        Ok cards ->
            [ Url.Builder.string "board" (cards |> List.map Card.toString |> String.concat |> String.toLower) ]


rangeQueryParameter : Position -> Form.Field (List HandRange) -> List Url.Builder.QueryParameter
rangeQueryParameter position field =
    case field.validated of
        Err _ ->
            []

        Ok [] ->
            []

        Ok range ->
            [ Url.Builder.string (position |> Position.toString |> String.toLower) (range |> Range.rangesToNormalizedString |> String.toLower) ]


confirmBoardSelection : Model -> ( Model, Cmd Msg )
confirmBoardSelection model =
    ( { model
        | boardSelectModalVisibility = Modal.hidden
        , simulationRequestForm = setBoard (model.boardSelection |> List.map Card.toString |> String.concat) model.simulationRequestForm
        , boardSelection = []
        , currentApiResponse = RemoteData.NotAsked
      }
    , Cmd.none
    )



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


sendSimulationRequestHttp : List Card -> List (List HandRange) -> Cmd Msg
sendSimulationRequestHttp board ranges =
    Http.get
        { expect = Http.expectJson (RemoteData.fromResult >> ApiResponseReceived) simulationResponseDecoder
        , url =
            Url.Builder.crossOrigin "https://safe-shore-53897.herokuapp.com"
                [ "simulation" ]
                ([ Url.Builder.string "board" (board |> List.map Card.toString |> String.concat)
                 , Url.Builder.string "stdev_target" "0.001"
                 ]
                    ++ (ranges
                            |> List.indexedMap
                                (\i range ->
                                    Url.Builder.string ("range" ++ String.fromInt (i + 1)) (range |> List.map Range.toString |> String.join ",")
                                )
                       )
                )
        }



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Equiweb"
    , body =
        [ Grid.container []
            [ Html.div []
                [ calculatorView model
                , boardSelectionModalView model
                , rangeSelectionModalView model
                ]
            ]
        ]
    }


loadingView : Html Msg
loadingView =
    Html.div []
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
                            (Html.a [ Html.Attributes.href (Url.Builder.absolute [] []), Flex.block, Flex.row, Flex.alignItemsStart ]
                                [ Html.img [ Html.Attributes.src "images/chip-icon.svg", Html.Attributes.width 40 ] []
                                , Html.div [ Html.Attributes.style "margin-top" "auto", Html.Attributes.style "margin-left" "7px", Html.Attributes.style "margin-bottom" "auto" ] [ Html.text "Equiweb" ]
                                ]
                                :: (if model.currentApiResponse |> RemoteData.isSuccess then
                                        [ Button.button [ Button.outlineSecondary, Button.onClick CopyUrlToClipboard ]
                                            [ Html.img [ Html.Attributes.src "images/share_black_24dp.svg", Html.Attributes.style "margin-right" "2px" ] []
                                            , Html.text "SHARE SCENARIO"
                                            ]
                                        ]

                                    else
                                        []
                                   )
                            )
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


cardView : Maybe Msg -> SelectState -> String -> String -> Card -> Html Msg
cardView msg selectState cursor refWidth card =
    let
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
         , Html.Attributes.style "min-height" "40px"
         , Html.Attributes.style "min-width" "35px"
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
        [ rangeInputView UTG model.simulationRequestForm.utg (model.currentApiResponse |> RemoteData.map (\r -> r.utg) |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateUtg (Data.positionalRanges |> List.filter (.position >> (==) UTG) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView MP model.simulationRequestForm.mp (model.currentApiResponse |> RemoteData.map (\r -> r.mp) |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateMp (Data.positionalRanges |> List.filter (.position >> (==) MP) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView CO model.simulationRequestForm.co (model.currentApiResponse |> RemoteData.map (\r -> r.co) |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateCo (Data.positionalRanges |> List.filter (.position >> (==) CO) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView BU model.simulationRequestForm.bu (model.currentApiResponse |> RemoteData.map (\r -> r.bu) |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateBu (Data.positionalRanges |> List.filter (.position >> (==) BU) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView SB model.simulationRequestForm.sb (model.currentApiResponse |> RemoteData.map (\r -> r.sb) |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateSb (Data.positionalRanges |> List.filter (.position >> (==) SB) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView BB model.simulationRequestForm.bb (model.currentApiResponse |> RemoteData.map (\r -> r.bb) |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateBb (Data.positionalRanges |> List.filter (.position >> (==) BB) |> List.map (\pr -> ( pr.label, pr.range )))
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
        ]


rangeInputView : Position -> Form.Field (List HandRange) -> Maybe ResultLine -> Dropdown.State -> List ( String, String ) -> Html Msg
rangeInputView position field result dropdownState ranges =
    Form.row []
        [ Form.col []
            [ Form.group []
                ([ Form.label [] [ Html.text field.name ]
                 , InputGroup.config
                    (InputGroup.text
                        ((if field.validated == Ok [] then
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
                            [ Html.img [ Html.Attributes.src "images/auto_fix_high_black_24dp.svg", Html.Attributes.height 20 ] [] ]
                        , InputGroup.button
                            [ Button.outlineSecondary
                            , Button.onClick (ShowRangeSelectionModal position)
                            , Button.attrs [ Html.Attributes.tabindex -1 ]
                            ]
                            [ Html.img [ Html.Attributes.src "images/apps_black_24dp.svg", Html.Attributes.height 22 ] [] ]
                        , InputGroup.dropdown
                            dropdownState
                            { options = []
                            , toggleMsg = RangeDropDownMsg position
                            , toggleButton =
                                Dropdown.toggle [ Button.outlineSecondary, Button.attrs [ Html.Attributes.tabindex -1 ] ] [ Html.text "Ranges" ]
                            , items =
                                ranges
                                    |> List.map
                                        (\( label, range ) ->
                                            Dropdown.buttonItem [ Html.Events.onClick (SelectPresetRange position range) ] [ Html.text label ]
                                        )
                            }
                        ]
                    |> InputGroup.view
                 ]
                    ++ numberOfCombosView (field.validated |> Result.withDefault [])
                )
            ]
        , Form.col [ Col.sm2 ]
            [ Form.group []
                [ Form.label [] [ Html.text "Equity" ]
                , Input.text [ Input.readonly True, Input.attrs [ Html.Attributes.tabindex -1 ], equityValueView result ]
                ]
            ]
        ]


numberOfCombosView : List HandRange -> List (Html Msg)
numberOfCombosView ranges =
    if ranges |> List.isEmpty |> not then
        [ Form.help []
            [ Html.div [ Spacing.mt1 ]
                [ Progress.progress
                    [ Progress.value (Range.percentage ranges * 100)
                    , Progress.info
                    , Progress.wrapperAttrs [ Html.Attributes.style "height" "6px" ]
                    ]
                , Html.text (((Range.percentage ranges * 100) |> Round.round 1) ++ "%" ++ " " ++ "(" ++ ((Range.numberOfCombos ranges |> String.fromInt) ++ "/" ++ (Combo.total |> String.fromInt) ++ ")"))
                ]
            ]
        ]

    else
        []


rewritable : Form.Field (List HandRange) -> Bool
rewritable field =
    field.value /= (field.validated |> Result.withDefault [] |> Range.rangesToNormalizedString)


boardCardView : String -> Card -> Html Msg
boardCardView height =
    cardView Nothing Selected "default" height


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


boardSelectionModalView : Model -> Html Msg
boardSelectionModalView model =
    Modal.config CloseBoardSelectModal
        |> Modal.large
        |> Modal.attrs [ Html.Attributes.class "modal-fullscreen-lg-down" ]
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
                                    |> List.reverse
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


resultView : SimulationResult -> Card.Config Msg
resultView result =
    Card.config [ Card.attrs [ Spacing.mb3, Html.Attributes.class "shadow" ] ]
        |> Card.headerH4 []
            [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween, Flex.alignItemsCenter ]
                [ if result.board |> List.isEmpty |> not then
                    Html.div []
                        [ Html.text "Board: "
                        , Html.text (result.board |> List.map Card.toString |> String.concat)
                        ]

                  else
                    Html.text "Preflop"
                , boardView "30px" result.board
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
                , Table.td [] [ Html.text (result.range |> Range.rangesToNormalizedString) ]
                , Table.td [] [ Html.text (Round.round 2 (result.equity * 100) ++ "%") ]
                ]
            ]

        Nothing ->
            []


cardSelectState : Card -> Model -> SelectState
cardSelectState card model =
    case model.cardUnderMouse of
        Just cum ->
            if
                cum
                    == card
                    && not model.ignoreCardHoverState
                    && (List.length model.boardSelection < 5 || (model.boardSelection |> List.member card))
            then
                MouseOver

            else if model.boardSelection |> List.member card then
                Selected

            else
                NotSelected

        Nothing ->
            if model.boardSelection |> List.member card then
                Selected

            else
                NotSelected


rangeSelectionModalView : Model -> Html Msg
rangeSelectionModalView model =
    Modal.config CloseRangeSelectionModal
        |> Modal.large
        |> Modal.attrs [ Html.Attributes.class "modal-fullscreen-lg-down" ]
        |> Modal.body []
            [ Html.div [ Flex.block, Flex.row, Spacing.mb3, Flex.wrap, Flex.justifyAround, Html.Attributes.style "gap" "8px" ]
                [ Button.button [ Button.outlineSecondary, Button.onClick SelectPairs ] [ Html.text "Pocket Pairs" ]
                , Button.button [ Button.outlineSecondary, Button.onClick SelectSuitedAces ] [ Html.text "Suited Aces" ]
                , Button.button [ Button.outlineSecondary, Button.onClick SelectSuitedBroadways ] [ Html.text "Suited Broadways" ]
                , Button.button [ Button.outlineSecondary, Button.onClick SelectOffsuitAces ] [ Html.text "Offsuit Aces" ]
                , Button.button [ Button.outlineSecondary, Button.onClick SelectOffsuitBroadways ] [ Html.text "Offsuit Broadways" ]
                ]
            , Html.div [ Flex.row, Flex.block, Flex.justifyAround ]
                [ Html.div []
                    (Hand.grid
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


rangeSelectState : Hand -> Model -> SelectState
rangeSelectState hand model =
    case model.handUnderMouse of
        Just hum ->
            if hum == hand && not model.ignoreRangeHoverState then
                MouseOver

            else if model.rangeSelection |> List.member (Range.fromHand hand) then
                Selected

            else
                NotSelected

        Nothing ->
            if model.rangeSelection |> List.member (Range.fromHand hand) then
                Selected

            else
                NotSelected


type SelectState
    = Selected
    | NotSelected
    | MouseOver


cellView : SelectState -> String -> Hand -> Html Msg
cellView cs size hand =
    let
        ( fontColor, color, opacity ) =
            case cs of
                Selected ->
                    ( "white", "#9b5378", "1" )

                NotSelected ->
                    if hand |> Hand.isOffsuit then
                        ( "#aaaaaa", "#eeeeee", "1" )

                    else if hand |> Hand.isSuited then
                        ( "#aaaaaa", "#dddddd", "1" )

                    else
                        ( "#aaaaaa", "#cccccc", "1" )

                MouseOver ->
                    ( "white", "#9b5378", "0.5" )
    in
    Html.div
        [ Html.Attributes.style "width" size
        , Html.Attributes.style "height" size
        , Html.Attributes.style "min-height" "20px"
        , Html.Attributes.style "min-width" "18px"
        , Html.Attributes.style "max-height" "50px"
        , Html.Attributes.style "max-width" "50px"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "margin" "1px"
        , Html.Attributes.style "user-select" "none"
        , Html.Attributes.style "opacity" opacity

        -- , Html.Events.onClick msg
        , Html.Events.onMouseEnter (HandHover (Just hand))
        , Html.Events.onMouseLeave (HandHover Nothing)
        ]
        [ Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.viewBox "0 0 100 100"
            ]
            [ Svg.rect
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
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Browser.Events.onMouseDown (Decode.succeed MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Dropdown.subscriptions model.rangeDropdownStateUtg (RangeDropDownMsg UTG)
        , Dropdown.subscriptions model.rangeDropdownStateMp (RangeDropDownMsg MP)
        , Dropdown.subscriptions model.rangeDropdownStateCo (RangeDropDownMsg CO)
        , Dropdown.subscriptions model.rangeDropdownStateBu (RangeDropDownMsg BU)
        , Dropdown.subscriptions model.rangeDropdownStateSb (RangeDropDownMsg SB)
        , Dropdown.subscriptions model.rangeDropdownStateBb (RangeDropDownMsg BB)
        ]
