module Main exposing (main)

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
import Browser exposing (UrlRequest)
import Browser.Events
import Browser.Navigation as Navigation
import DoubleSlider as Slider
import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode exposing (Error)
import Json.Decode.Pipeline as P
import Keyboard exposing (RawKey)
import List.Extra
import Maybe.Extra
import Poker.Board as Board
import Poker.Card as Card exposing (Card)
import Poker.Combo as Combo exposing (Combo)
import Poker.Hand as Hand exposing (Hand)
import Poker.HandOrCombo as HandOrCombo exposing (HandOrCombo)
import Poker.Position as Position exposing (Position(..))
import Poker.Ranges as Ranges
import Poker.Rank as Rank
import Poker.Suit as Suit exposing (Suit(..))
import Ports exposing (CopiedToClipboardMsg, SharingType(..))
import RangeCellSelectState exposing (RangeCellSelectState)
import RemoteData exposing (WebData)
import Result.Extra
import Round
import SelectState exposing (SelectState)
import Sharing
import SimulationResult exposing (ResultLine, SimulationResult)
import Svg
import Svg.Attributes
import Url exposing (Url)
import Url.Builder
import Url.Parser as UrlParser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias SimulationRequestForm =
    { utg : Form.Field (List HandOrCombo)
    , mp : Form.Field (List HandOrCombo)
    , co : Form.Field (List HandOrCombo)
    , bu : Form.Field (List HandOrCombo)
    , sb : Form.Field (List HandOrCombo)
    , bb : Form.Field (List HandOrCombo)
    , board : Form.Field (List Card)
    }


setBoard : String -> SimulationRequestForm -> SimulationRequestForm
setBoard board form =
    { form | board = form.board |> Form.setValue Board.validate board }


setRange : Position -> String -> SimulationRequestForm -> SimulationRequestForm
setRange position range form =
    case position of
        UTG ->
            { form | utg = form.utg |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos range }

        MP ->
            { form | mp = form.mp |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos range }

        CO ->
            { form | co = form.co |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos range }

        BU ->
            { form | bu = form.bu |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos range }

        SB ->
            { form | sb = form.sb |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos range }

        BB ->
            { form | bb = form.bb |> Form.setValue HandOrCombo.parseAsCononicalHandsOrCombos range }


initialForm : SimulationRequestForm
initialForm =
    { utg = { name = "UTG", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
    , mp = { name = "MP", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
    , co = { name = "CO", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
    , bu = { name = "BU", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
    , sb = { name = "SB", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
    , bb = { name = "BB", value = "", validated = HandOrCombo.parseAsCononicalHandsOrCombos "", edited = False }
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


type Mouse
    = Released
    | Pressed


type alias PopoverStates =
    { rangeSelect : Popover.State
    , openGrid : Popover.State
    , normalize : Popover.State
    , clear : Popover.State
    }


initialPopoverStates : PopoverStates
initialPopoverStates =
    { rangeSelect = Popover.initialState
    , openGrid = Popover.initialState
    , normalize = Popover.initialState
    , clear = Popover.initialState
    }


type alias SharingPopoverStates =
    { shareUrl : Popover.State
    , shareUrlTooltipText : String
    , shareMd : Popover.State
    , shareMdTooltipText : String
    , share2plus2 : Popover.State
    , share2plus2TooltipText : String
    , sharePs : Popover.State
    , sharePsTooltipText : String
    }


initialSharingPopoverStates : SharingPopoverStates
initialSharingPopoverStates =
    { shareUrl = Popover.initialState
    , shareUrlTooltipText = "Copy URL"
    , shareMd = Popover.initialState
    , shareMdTooltipText = "Copy Markdown"
    , share2plus2 = Popover.initialState
    , share2plus2TooltipText = "Copy Forum Format (2+2)"
    , sharePs = Popover.initialState
    , sharePsTooltipText = "Copy Forum Format (PokerStrategy)"
    }


type alias Model =
    { navKey : Navigation.Key
    , simulationRequestForm : SimulationRequestForm
    , currentApiResponse : WebData SimulationResult
    , results : List ( SharingPopoverStates, Url, SimulationResult )
    , boardSelectModalVisibility : Modal.Visibility
    , rangeSelectionModalVisibility : Modal.Visibility
    , boardSelection : List Card
    , rangeSelection : List Combo
    , rangeSelectionPosition : Position
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
    , rangeSelectionDropdown : Dropdown.State
    , location : Url
    , popoverStateUtg : PopoverStates
    , popoverStateMp : PopoverStates
    , popoverStateCo : PopoverStates
    , popoverStateBu : PopoverStates
    , popoverStateSb : PopoverStates
    , popoverStateBb : PopoverStates
    , popoverStateBoard : Popover.State
    , popoverStateClearBoard : Popover.State
    , slider : Slider.DoubleSlider Msg
    , suitSelection : Maybe Suit.Selection
    }


popoverState : Position -> Model -> PopoverStates
popoverState position model =
    case position of
        UTG ->
            model.popoverStateUtg

        MP ->
            model.popoverStateMp

        CO ->
            model.popoverStateCo

        BU ->
            model.popoverStateBu

        SB ->
            model.popoverStateSb

        BB ->
            model.popoverStateBb


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


initialRangeSlider : Slider.DoubleSlider Msg
initialRangeSlider =
    Slider.init
        { min = 0
        , max = 100
        , lowValue = 0
        , highValue = 0
        , step = 0.1
        , onLowChange = DoubleSliderLowChange
        , onHighChange = DoubleSliderHighChange
        }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url key =
    let
        maybeForm =
            UrlParser.parse urlParser url
    in
    { simulationRequestForm = maybeForm |> Maybe.withDefault initialForm
    , currentApiResponse = RemoteData.NotAsked
    , results = []
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
    , navKey = key
    , rangeDropdownStateUtg = Dropdown.initialState
    , rangeDropdownStateMp = Dropdown.initialState
    , rangeDropdownStateCo = Dropdown.initialState
    , rangeDropdownStateBu = Dropdown.initialState
    , rangeDropdownStateSb = Dropdown.initialState
    , rangeDropdownStateBb = Dropdown.initialState
    , rangeSelectionDropdown = Dropdown.initialState
    , location = url
    , popoverStateUtg = initialPopoverStates
    , popoverStateMp = initialPopoverStates
    , popoverStateCo = initialPopoverStates
    , popoverStateBu = initialPopoverStates
    , popoverStateSb = initialPopoverStates
    , popoverStateBb = initialPopoverStates
    , popoverStateBoard = Popover.initialState
    , popoverStateClearBoard = Popover.initialState
    , slider = initialRangeSlider
    , suitSelection = Nothing
    }
        |> sendSimulationRequest


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
    | CopyToClipboard Int SharingType
    | DoubleSliderLowChange Float
    | DoubleSliderHighChange Float
    | HandHover (Maybe Hand)
    | KeyDown RawKey
    | MouseDown
    | MouseUp
    | NotifyCopyToClipboard (Result Error CopiedToClipboardMsg)
    | PopoverStateBoard Popover.State
    | PopoverStateClear Position Popover.State
    | PopoverStateClearBoard Popover.State
    | PopoverStateNormalize Position Popover.State
    | PopoverStateOpenGrid Position Popover.State
    | PopoverStateSelectRange Position Popover.State
    | PopoverStateSharing Int SharingType Popover.State
    | RangeDropdownMsg Position Dropdown.State
    | RangeInput Position String
    | RangeSelectionDropdownMsg Dropdown.State
    | RemoveBoard
    | RemoveRange Position
    | RewriteRange Position
    | SelectOffsuitAces
    | SelectOffsuitBroadways
    | SelectPairs
    | SelectPresetRange Position String
    | SelectRange String
    | SelectSuitedAces
    | SelectSuitedBroadways
    | SendSimulationRequest
    | ShowBoardSelectModal
    | ShowRangeSelectionModal Position
    | ToggleBoardSelection Card
    | ToggleOffsuitSuitsSelection Suit Suit
    | TogglePairsSuitsSelection Suit Suit
    | ToggleSuitedSuitsSelection Suit
    | ToggleSuitSelection
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
    ( { model
        | currentApiResponse = sr
        , results = model.results ++ (sr |> RemoteData.map List.singleton |> RemoteData.withDefault [] |> List.map (\r -> ( initialSharingPopoverStates, model.location, r )))
      }
    , Cmd.none
    )


rangesFromForm : SimulationRequestForm -> List (List HandOrCombo)
rangesFromForm simulationRequestForm =
    [ simulationRequestForm.utg.validated
    , simulationRequestForm.mp.validated
    , simulationRequestForm.co.validated
    , simulationRequestForm.bu.validated
    , simulationRequestForm.sb.validated
    , simulationRequestForm.bb.validated
    ]
        |> List.map Result.toMaybe
        |> Maybe.Extra.values
        |> List.filter (not << List.isEmpty)


sendSimulationRequest : Model -> ( Model, Cmd Msg )
sendSimulationRequest model =
    if validateForm model.simulationRequestForm |> Result.Extra.isErr then
        ( { model | simulationRequestForm = setAllFormFieldsToEdited model.simulationRequestForm }
        , Cmd.none
        )

    else if (rangesFromForm model.simulationRequestForm |> List.length) < 2 then
        ( model, Cmd.none )

    else
        ( { model
            | currentApiResponse = RemoteData.Loading
            , simulationRequestForm = setAllFormFieldsToEdited model.simulationRequestForm
          }
        , sendSimulationRequestHttp (model.simulationRequestForm.board.validated |> Result.withDefault []) (rangesFromForm model.simulationRequestForm)
        )


validateForm : SimulationRequestForm -> Result (List String) SimulationRequestForm
validateForm form =
    Ok (\_ _ _ _ _ _ _ -> form)
        |> Form.apply (form.utg.validated |> Result.Extra.mapBoth (always [ "The UTG range is not a valid range." ]) identity)
        |> Form.apply (form.mp.validated |> Result.Extra.mapBoth (always [ "The MP range is not a valid range." ]) identity)
        |> Form.apply (form.co.validated |> Result.Extra.mapBoth (always [ "The CO range is not a valid range." ]) identity)
        |> Form.apply (form.bu.validated |> Result.Extra.mapBoth (always [ "The BU range is not a valid range." ]) identity)
        |> Form.apply (form.sb.validated |> Result.Extra.mapBoth (always [ "The SB range is not a valid range." ]) identity)
        |> Form.apply (form.bb.validated |> Result.Extra.mapBoth (always [ "The BB range is not a valid range." ]) identity)
        |> Form.apply (form.board.validated |> Result.Extra.mapBoth (always [ "The board is not a valid board." ]) identity)


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
                        , suitSelection = Nothing
                    }

        ApiResponseReceived result ->
            handleApiResponse model result

        SendSimulationRequest ->
            sendSimulationRequest model
                |> updateUrl

        RangeInput position str ->
            ( { model
                | simulationRequestForm = setRange position str model.simulationRequestForm
                , currentApiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )

        BoardInput str ->
            ( { model
                | simulationRequestForm = setBoard str model.simulationRequestForm
                , currentApiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )

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
                , rangeSelection = range |> List.concatMap HandOrCombo.combos
                , rangeSelectionPosition = position
                , slider = initialRangeSlider
              }
            , Cmd.none
            )

        CloseRangeSelectionModal ->
            ( { model | rangeSelectionModalVisibility = Modal.hidden, rangeSelection = [], suitSelection = Nothing }, Cmd.none )

        ConfirmRangeSelection ->
            confirmRangeSelection model.rangeSelectionPosition model

        MouseDown ->
            case model.handUnderMouse of
                Just hand ->
                    ( toggleHandSelection hand { model | mouse = Pressed }, Cmd.none )

                Nothing ->
                    ( { model | mouse = Pressed }, Cmd.none )

        MouseUp ->
            ( { model | mouse = Released }, Cmd.none )

        ClearRange ->
            ( { model | rangeSelection = [], suitSelection = Nothing, slider = initialRangeSlider }, Cmd.none )

        HandHover (Just hand) ->
            if model.mouse == Pressed then
                ( toggleHandSelection hand { model | handUnderMouse = Just hand, ignoreRangeHoverState = False }, Cmd.none )

            else
                ( { model | handUnderMouse = Just hand, ignoreRangeHoverState = False }, Cmd.none )

        HandHover Nothing ->
            ( { model | handUnderMouse = Nothing, ignoreRangeHoverState = False }, Cmd.none )

        SelectPairs ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.pairs |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = initialRangeSlider
              }
            , Cmd.none
            )

        SelectSuitedAces ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.suitedAces |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = initialRangeSlider
              }
            , Cmd.none
            )

        SelectSuitedBroadways ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.suitedBroadways |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = initialRangeSlider
              }
            , Cmd.none
            )

        SelectOffsuitAces ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.offsuitAces |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = initialRangeSlider
              }
            , Cmd.none
            )

        SelectOffsuitBroadways ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.offsuitBroadways |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = initialRangeSlider
              }
            , Cmd.none
            )

        RangeDropdownMsg position state ->
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

        RangeSelectionDropdownMsg state ->
            ( { model | rangeSelectionDropdown = state }, Cmd.none )

        SelectPresetRange position range ->
            ( { model
                | simulationRequestForm = setRange position range model.simulationRequestForm |> rewrite position
                , currentApiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )

        CopyToClipboard index sharingType ->
            let
                maybeText =
                    case sharingType of
                        URL ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map (\( _, url, _ ) -> Url.toString url)

                        Markdown ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map ((\( _, _, r ) -> r) >> Sharing.markdown model.location)

                        TwoPlusTwo ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map ((\( _, _, r ) -> r) >> Sharing.twoPlusTwo model.location)

                        PokerStrategy ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map ((\( _, _, r ) -> r) >> Sharing.pokerStrategy model.location)
            in
            case maybeText of
                Just text ->
                    ( model, Ports.copyToClipboard (Ports.copyToclipboardMsgEncoder { text = text, index = index, sharingType = sharingType }) )

                Nothing ->
                    ( model, Cmd.none )

        RemoveRange position ->
            ( { model | simulationRequestForm = setRange position "" model.simulationRequestForm }, Cmd.none )

        NotifyCopyToClipboard (Err _) ->
            ( model, Cmd.none )

        NotifyCopyToClipboard (Ok copiedMsg) ->
            ( { model
                | results =
                    model.results
                        |> List.reverse
                        |> List.Extra.updateAt copiedMsg.index
                            (\( pos, url, sr ) ->
                                ( case copiedMsg.sharingType of
                                    URL ->
                                        { pos | shareUrlTooltipText = "Copied! " }

                                    Markdown ->
                                        { pos | shareMdTooltipText = "Copied! " }

                                    TwoPlusTwo ->
                                        { pos | share2plus2TooltipText = "Copied! " }

                                    PokerStrategy ->
                                        { pos | sharePsTooltipText = "Copied! " }
                                , url
                                , sr
                                )
                            )
                        |> List.reverse
              }
            , Cmd.none
            )

        SelectRange range ->
            ( { model
                | rangeSelection = range |> HandOrCombo.parseAsCononicalHandsOrCombos |> Result.withDefault [] |> List.concatMap HandOrCombo.combos
              }
            , Cmd.none
            )

        RemoveBoard ->
            ( { model | simulationRequestForm = setBoard "" model.simulationRequestForm }, Cmd.none )

        PopoverStateSelectRange position state ->
            Cmd.none |> Tuple.pair (updatePopoverState (\s -> { s | rangeSelect = state }) position model)

        PopoverStateOpenGrid position state ->
            Cmd.none |> Tuple.pair (updatePopoverState (\s -> { s | openGrid = state }) position model)

        PopoverStateNormalize position state ->
            Cmd.none |> Tuple.pair (updatePopoverState (\s -> { s | normalize = state }) position model)

        PopoverStateClear position state ->
            Cmd.none |> Tuple.pair (updatePopoverState (\s -> { s | clear = state }) position model)

        PopoverStateBoard state ->
            ( { model | popoverStateBoard = state }, Cmd.none )

        PopoverStateClearBoard state ->
            ( { model | popoverStateClearBoard = state }, Cmd.none )

        PopoverStateSharing index sharingType state ->
            let
                resestStateIfNotActive pos (Popover.State s) =
                    if s.isActive then
                        case sharingType of
                            URL ->
                                { pos | shareUrl = state }

                            Markdown ->
                                { pos | shareMd = state }

                            TwoPlusTwo ->
                                { pos | share2plus2 = state }

                            PokerStrategy ->
                                { pos | sharePs = state }

                    else
                        case sharingType of
                            URL ->
                                { initialSharingPopoverStates | shareUrl = state }

                            Markdown ->
                                { initialSharingPopoverStates | shareMd = state }

                            TwoPlusTwo ->
                                { initialSharingPopoverStates | share2plus2 = state }

                            PokerStrategy ->
                                { initialSharingPopoverStates | sharePs = state }
            in
            ( { model
                | results = model.results |> List.Extra.updateAt ((model.results |> List.length) - index - 1) (\( pos, url, rs ) -> ( resestStateIfNotActive pos state, url, rs ))
              }
            , Cmd.none
            )

        DoubleSliderLowChange str ->
            let
                newSlider =
                    Slider.updateLowValue str model.slider
            in
            ( { model
                | slider = newSlider
                , rangeSelection = HandOrCombo.range (Slider.fetchHighValue newSlider / 100) (Slider.fetchLowValue newSlider / 100) |> List.concatMap HandOrCombo.combos
              }
            , Cmd.none
            )

        DoubleSliderHighChange str ->
            let
                newSlider =
                    Slider.updateHighValue str model.slider
            in
            ( { model
                | slider = newSlider
                , rangeSelection = HandOrCombo.range (Slider.fetchHighValue newSlider / 100) (Slider.fetchLowValue newSlider / 100) |> List.concatMap HandOrCombo.combos
              }
            , Cmd.none
            )

        ToggleSuitSelection ->
            ( { model
                | suitSelection =
                    if model.suitSelection |> Maybe.Extra.isJust then
                        Nothing

                    else
                        Just Suit.initialSelection
              }
            , Cmd.none
            )

        ToggleSuitedSuitsSelection suit ->
            ( { model | suitSelection = model.suitSelection |> Maybe.map (Suit.toggleSuitedSelection suit) }, Cmd.none )

        TogglePairsSuitsSelection suit1 suit2 ->
            ( { model | suitSelection = model.suitSelection |> Maybe.map (Suit.togglePairsSelection suit1 suit2) }, Cmd.none )

        ToggleOffsuitSuitsSelection suit1 suit2 ->
            ( { model | suitSelection = model.suitSelection |> Maybe.map (Suit.toggleOffSuitSelection suit1 suit2) }, Cmd.none )


updatePopoverState : (PopoverStates -> PopoverStates) -> Position -> Model -> Model
updatePopoverState f position model =
    case position of
        UTG ->
            { model | popoverStateUtg = f model.popoverStateUtg }

        MP ->
            { model | popoverStateMp = f model.popoverStateMp }

        CO ->
            { model | popoverStateCo = f model.popoverStateCo }

        BU ->
            { model | popoverStateBu = f model.popoverStateBu }

        SB ->
            { model | popoverStateSb = f model.popoverStateSb }

        BB ->
            { model | popoverStateBb = f model.popoverStateBb }


toggleHandSelection : Hand -> Model -> Model
toggleHandSelection hand model =
    let
        comboSelection suitSelection =
            hand
                |> Hand.fold
                    (\rank -> suitSelection.pairs |> List.filterMap (\( suit1, suit2 ) -> Combo.combo (Card rank suit1) (Card rank suit2)))
                    (\rank1 rank2 -> suitSelection.suited |> List.filterMap (\suit -> Combo.combo (Card rank1 suit) (Card rank2 suit)))
                    (\rank1 rank2 -> suitSelection.offsuit |> List.filterMap (\( suit1, suit2 ) -> Combo.combo (Card rank1 suit1) (Card rank2 suit2)))

        rangeSelection =
            case ( model.rangeSelection |> Hand.numCombosOfHand hand, model.suitSelection ) of
                ( Hand.All _, Nothing ) ->
                    model.rangeSelection |> List.filter (\c -> hand |> Hand.combos |> List.member c |> not)

                ( Hand.All n, Just suitSelection ) ->
                    if (comboSelection suitSelection |> List.length) == n then
                        model.rangeSelection |> List.filter (\c -> hand |> Hand.combos |> List.member c |> not)

                    else
                        (model.rangeSelection |> List.filter (\c -> hand |> Hand.combos |> List.member c |> not))
                            ++ comboSelection suitSelection

                ( _, Nothing ) ->
                    model.rangeSelection ++ (hand |> Hand.combos)

                ( Hand.None, Just suitSelection ) ->
                    model.rangeSelection ++ comboSelection suitSelection

                ( Hand.Some _, Just suitSelection ) ->
                    if (comboSelection suitSelection |> List.sortWith Combo.order) == (model.rangeSelection |> Hand.filter hand |> List.sortWith Combo.order) then
                        model.rangeSelection |> List.filter (\c -> hand |> Hand.combos |> List.member c |> not)

                    else
                        (model.rangeSelection |> List.filter (\c -> hand |> Hand.combos |> List.member c |> not))
                            ++ comboSelection suitSelection
    in
    { model
        | rangeSelection = rangeSelection
        , ignoreRangeHoverState = True
        , slider = initialRangeSlider
    }


rewriteBoard : SimulationRequestForm -> SimulationRequestForm
rewriteBoard form =
    { form | board = Form.rewrite form.board (List.map Card.toString >> String.concat) }


rewrite : Position -> SimulationRequestForm -> SimulationRequestForm
rewrite position form =
    case position of
        UTG ->
            { form | utg = Form.rewrite form.utg HandOrCombo.toNormalizedString }

        MP ->
            { form | mp = Form.rewrite form.mp HandOrCombo.toNormalizedString }

        CO ->
            { form | co = Form.rewrite form.co HandOrCombo.toNormalizedString }

        BU ->
            { form | bu = Form.rewrite form.bu HandOrCombo.toNormalizedString }

        SB ->
            { form | sb = Form.rewrite form.sb HandOrCombo.toNormalizedString }

        BB ->
            { form | bb = Form.rewrite form.bb HandOrCombo.toNormalizedString }


confirmRangeSelection : Position -> Model -> ( Model, Cmd Msg )
confirmRangeSelection position model =
    let
        form =
            setRange position (model.rangeSelection |> List.map HandOrCombo.fromCombo |> HandOrCombo.toNormalizedString) model.simulationRequestForm
                |> rewrite position
    in
    ( { model
        | rangeSelectionModalVisibility = Modal.hidden
        , simulationRequestForm = form
        , rangeSelection = []
        , suitSelection = Nothing
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
    let
        set f maybe form =
            case maybe of
                Just v ->
                    f v form

                Nothing ->
                    form
    in
    initialForm
        |> set (setRange UTG) maybeUtg
        |> rewrite UTG
        |> set (setRange MP) maybeMp
        |> rewrite MP
        |> set (setRange CO) maybeCo
        |> rewrite CO
        |> set (setRange BU) maybeBu
        |> rewrite BU
        |> set (setRange SB) maybeSb
        |> rewrite SB
        |> set (setRange BB) maybeBb
        |> rewrite BB
        |> set setBoard maybeBaord
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
    if model.simulationRequestForm |> validateForm |> Result.Extra.isOk then
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


rangeQueryParameter : Position -> Form.Field (List HandOrCombo) -> List Url.Builder.QueryParameter
rangeQueryParameter position field =
    case field.validated of
        Err _ ->
            []

        Ok [] ->
            []

        Ok range ->
            [ Url.Builder.string (position |> Position.toString |> String.toLower) (range |> HandOrCombo.toNormalizedString |> String.toLower) ]


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


sendSimulationRequestHttp : List Card -> List (List HandOrCombo) -> Cmd Msg
sendSimulationRequestHttp board ranges =
    Http.get
        { expect = Http.expectJson (RemoteData.fromResult >> ApiResponseReceived) simulationResponseDecoder
        , url =
            Url.Builder.crossOrigin "https://safe-shore-53897.herokuapp.com"
                [ "simulation" ]
                ([ Url.Builder.string "board" (board |> List.map Card.toString |> String.concat)
                 , Url.Builder.string "stdev_target" "0.001"
                 , Url.Builder.string "num_iterations" "500"
                 ]
                    ++ (ranges
                            |> List.indexedMap
                                (\i range ->
                                    Url.Builder.string ("range" ++ String.fromInt (i + 1)) (range |> List.map HandOrCombo.toString |> String.join ",")
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
                SelectState.Selected ->
                    "1"

                SelectState.NotSelected ->
                    "0.5"

                SelectState.MouseOver ->
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
        [ rangeInputView (popoverState UTG model) UTG model.simulationRequestForm.utg (model.currentApiResponse |> RemoteData.map .utg |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateUtg (Ranges.positionalRanges |> List.filter (.position >> (==) UTG) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (popoverState MP model) MP model.simulationRequestForm.mp (model.currentApiResponse |> RemoteData.map .mp |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateMp (Ranges.positionalRanges |> List.filter (.position >> (==) MP) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (popoverState CO model) CO model.simulationRequestForm.co (model.currentApiResponse |> RemoteData.map .co |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateCo (Ranges.positionalRanges |> List.filter (.position >> (==) CO) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (popoverState BU model) BU model.simulationRequestForm.bu (model.currentApiResponse |> RemoteData.map .bu |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateBu (Ranges.positionalRanges |> List.filter (.position >> (==) BU) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (popoverState SB model) SB model.simulationRequestForm.sb (model.currentApiResponse |> RemoteData.map .sb |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateSb (Ranges.positionalRanges |> List.filter (.position >> (==) SB) |> List.map (\pr -> ( pr.label, pr.range )))
        , rangeInputView (popoverState BB model) BB model.simulationRequestForm.bb (model.currentApiResponse |> RemoteData.map .bb |> RemoteData.toMaybe |> Maybe.andThen identity) model.rangeDropdownStateBb (Ranges.positionalRanges |> List.filter (.position >> (==) BB) |> List.map (\pr -> ( pr.label, pr.range )))
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
                        |> InputGroup.attrs
                            (if model.simulationRequestForm.board.validated |> Result.Extra.isOk then
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
                                        [ Html.img [ Html.Attributes.src "images/cards-icon.svg", Html.Attributes.width 20 ] [] ]
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
                                            , Button.disabled (model.simulationRequestForm.board.value |> String.isEmpty)
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
        , Form.row [ Row.attrs [ Spacing.mt2 ] ] [ Form.col [] [ boardView "6vw" (model.simulationRequestForm.board.validated |> Result.withDefault []) ] ]
        , if (rangesFromForm model.simulationRequestForm |> List.length) < 2 then
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
                        ((if field.validated == Ok [] then
                            []

                          else
                            validationFeedbackOutline field
                         )
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
                                        , Button.disabled (rewritable field |> not)
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


rewritable : Form.Field (List HandOrCombo) -> Bool
rewritable field =
    field.value
        /= (field.validated |> Result.withDefault [] |> HandOrCombo.toNormalizedString)
        && (field.validated |> Result.Extra.isOk)


boardCardView : String -> Card -> Html Msg
boardCardView height =
    cardView Nothing SelectState.Selected "default" height


boardView : String -> List Card -> Html Msg
boardView height cards =
    Html.div [ Flex.block, Flex.row ] (streetsView height cards)


streetView : String -> String -> List Card -> Html Msg
streetView label height cards =
    Html.div [ Html.Attributes.style "margin-right" "10px" ]
        [ Html.h6 [] [ Html.text label ]
        , Html.div [ Flex.block, Flex.row, Html.Attributes.style "gap" "2px" ] (cards |> List.map (boardCardView height))
        ]


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
                    boardView "30px" result.board

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
                SelectState.MouseOver

            else if model.boardSelection |> List.member card then
                SelectState.Selected

            else
                SelectState.NotSelected

        Nothing ->
            if model.boardSelection |> List.member card then
                SelectState.Selected

            else
                SelectState.NotSelected


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
                    ([ Html.div [ Flex.block, Flex.row, Spacing.mb2 ] [ boardView "27px" (model.simulationRequestForm.board.validated |> Result.withDefault []) ]
                     , Html.hr [] []
                     ]
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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Browser.Events.onMouseDown (Decode.succeed MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Dropdown.subscriptions model.rangeDropdownStateUtg (RangeDropdownMsg UTG)
        , Dropdown.subscriptions model.rangeDropdownStateMp (RangeDropdownMsg MP)
        , Dropdown.subscriptions model.rangeDropdownStateCo (RangeDropdownMsg CO)
        , Dropdown.subscriptions model.rangeDropdownStateBu (RangeDropdownMsg BU)
        , Dropdown.subscriptions model.rangeDropdownStateSb (RangeDropdownMsg SB)
        , Dropdown.subscriptions model.rangeDropdownStateBb (RangeDropdownMsg BB)
        , Ports.notifyCopyToClipboard (Decode.decodeValue Ports.copiedToClipboardMsgDecoder >> NotifyCopyToClipboard)
        , Dropdown.subscriptions model.rangeSelectionDropdown RangeSelectionDropdownMsg
        ]
