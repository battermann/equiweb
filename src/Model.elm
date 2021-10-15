module Model exposing
    ( ApiResponse
    , Model
    , Mouse(..)
    , Msg(..)
    , PopoverStates
    , ResultLine
    , SharingPopoverStates
    , SimulationResult
    , allRangesExcept
    , board
    , init
    , initialPopoverStates
    , initialRangeSlider
    , initialSharingPopoverStates
    , popoverState
    , range
    , urlParser
    )

import Bootstrap.Alt.Modal as Modal
import Bootstrap.Alt.Popover as Popover
import Bootstrap.Dropdown as Dropdown
import Bounce exposing (Bounce)
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import DoubleSlider as Slider
import Form exposing (RangesForm)
import Json.Decode as Decode exposing (Error)
import Keyboard exposing (RawKey)
import Poker.Card exposing (Card)
import Poker.Combo exposing (Combo)
import Poker.Hand exposing (Hand)
import Poker.HandOrCombo exposing (HandOrCombo)
import Poker.Position as Position exposing (Position(..))
import Poker.Suit as Suit exposing (Suit(..))
import Ports exposing (CopiedToClipboardMsg, SharingType(..))
import RemoteData exposing (WebData)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias ResultLine =
    { range : List HandOrCombo
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
    , form : RangesForm
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
    , simulationApibaseUrl : Maybe String
    , bounce : Bounce
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


init : (Model -> ( Model, Cmd Msg )) -> Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init send flags url key =
    let
        maybeForm =
            UrlParser.parse urlParser url

        baseUrl =
            Decode.decodeValue Decode.string flags |> Result.toMaybe
    in
    { form = maybeForm |> Maybe.withDefault Form.initialForm
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
    , simulationApibaseUrl = baseUrl
    , bounce = Bounce.init
    }
        |> send


type alias ApiResponse =
    { equityPlayer1 : Float
    , equityPlayer2 : Float
    , equityPlayer3 : Maybe Float
    , equityPlayer4 : Maybe Float
    , equityPlayer5 : Maybe Float
    , equityPlayer6 : Maybe Float
    }


type Msg
    = ApiResponseReceived (WebData ApiResponse)
    | BoardInput String
    | BounceMsg
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


urlParser : Parser (RangesForm -> a) a
urlParser =
    let
        toForm maybeUtg maybeMp maybeCo maybeBu maybeSb maybeBb maybeBaord =
            let
                set f maybe form =
                    case maybe of
                        Just v ->
                            f v form

                        Nothing ->
                            form
            in
            Form.initialForm
                |> set (Form.setRange UTG) maybeUtg
                |> Form.rewrite UTG
                |> set (Form.setRange MP) maybeMp
                |> Form.rewrite MP
                |> set (Form.setRange CO) maybeCo
                |> Form.rewrite CO
                |> set (Form.setRange BU) maybeBu
                |> Form.rewrite BU
                |> set (Form.setRange SB) maybeSb
                |> Form.rewrite SB
                |> set (Form.setRange BB) maybeBb
                |> Form.rewrite BB
                |> set Form.setBoard maybeBaord
                |> Form.rewriteBoard
                |> Form.updateNumberOfCombos
    in
    UrlParser.map toForm
        (UrlParser.top
            <?> Query.string "utg"
            <?> Query.string "mp"
            <?> Query.string "co"
            <?> Query.string "bu"
            <?> Query.string "sb"
            <?> Query.string "bb"
            <?> Query.string "board"
        )


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


board : Model -> List Card
board model =
    model.form |> Form.board


range : Position -> Model -> List HandOrCombo
range position model =
    model.form |> Form.range position


allRangesExcept : Position -> Model -> List (List HandOrCombo)
allRangesExcept position model =
    Position.all
        |> List.filter ((/=) position)
        |> List.map (\p -> range p model)
