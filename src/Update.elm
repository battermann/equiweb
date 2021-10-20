module Update exposing (sendSimulationRequest, update)

import Api
import Bootstrap.Alt.Modal as Modal
import Bootstrap.Alt.Popover as Popover
import Bounce
import Browser
import Browser.Navigation as Navigation
import DoubleSlider as Slider
import Form
import Keyboard
import List.Extra
import Maybe.Extra
import Model exposing (ApiResponse, Model, Msg(..), PopoverStates, ResultLine, SimulationResult)
import Poker.Card as Card exposing (Card)
import Poker.CardRemoval as CardRemoval
import Poker.Combo as Combo
import Poker.Hand as Hand exposing (Hand)
import Poker.HandOrCombo as HandOrCombo
import Poker.Position as Position exposing (Position(..))
import Poker.Suit as Suit exposing (Suit(..))
import Ports exposing (SharingType(..))
import RemoteData exposing (WebData)
import Result.Extra
import ResultFormatter
import Url
import Url.Builder
import Url.Parser as UrlParser


bounceDelay : Float
bounceDelay =
    500


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
                        | form = UrlParser.parse Model.urlParser url |> Maybe.withDefault Form.initialForm
                        , boardSelectModalVisibility = Modal.hidden
                        , rangeSelectionModalVisibility = Modal.hidden
                        , boardSelection = []
                        , rangeSelection = []
                        , blockedCombosForRangeSelection = []
                        , rangeSelectionWithCardRemoval = []
                        , rangeSelectionPosition = UTG
                        , cardUnderMouse = Nothing
                        , ignoreCardHoverState = False
                        , mouse = Model.Released
                        , handUnderMouse = Nothing
                        , ignoreRangeHoverState = False
                        , location = url
                        , suitSelection = Nothing
                        , apiResponse =
                            if model.apiResponse |> RemoteData.isLoading then
                                model.apiResponse

                            else
                                RemoteData.NotAsked
                    }

        ApiResponseReceived result ->
            handleApiResponse model result

        SendSimulationRequest ->
            sendSimulationRequest model
                |> updateUrl

        RangeInput position str ->
            ( { model
                | form = Form.setRange position str model.form
                , apiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )
                |> triggerBounce

        BoardInput str ->
            ( { model
                | form = Form.setBoard str model.form
                , apiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )
                |> triggerBounce

        RewriteRange position ->
            ( { model | form = Form.rewrite position model.form }, Cmd.none )

        CloseBoardSelectModal ->
            ( { model | boardSelectModalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowBoardSelectModal ->
            ( { model | boardSelectModalVisibility = Modal.shown, boardSelection = Form.board model.form }
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
                |> triggerBounce

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
                        confirmBoardSelection model |> triggerBounce

                    else if model.rangeSelectionModalVisibility == Modal.shown then
                        confirmRangeSelection model.rangeSelectionPosition model
                            |> triggerBounce

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
                rangeSelection =
                    Form.range position model.form |> List.concatMap HandOrCombo.combos

                blockedCombosForRangeSelection =
                    CardRemoval.blockedCombosForRangeSelection (Form.board model.form) (Form.allRangesExcept position model.form)

                rangeSelectionWithCardRemoval =
                    rangeSelection
                        |> List.Extra.filterNot (\c -> List.member c blockedCombosForRangeSelection)
            in
            ( { model
                | rangeSelectionModalVisibility = Modal.shown
                , rangeSelection = rangeSelection
                , blockedCombosForRangeSelection = blockedCombosForRangeSelection
                , rangeSelectionWithCardRemoval = rangeSelectionWithCardRemoval
                , rangeSelectionPosition = position
                , slider = Model.initialRangeSlider
                , suitSelection = Nothing
              }
            , Cmd.none
            )

        CloseRangeSelectionModal ->
            ( { model
                | rangeSelectionModalVisibility = Modal.hidden
                , rangeSelection = []
                , blockedCombosForRangeSelection = []
                , rangeSelectionWithCardRemoval = []
                , suitSelection = Nothing
              }
            , Cmd.none
            )

        ConfirmRangeSelection ->
            confirmRangeSelection model.rangeSelectionPosition model
                |> triggerBounce

        MouseDown ->
            case model.handUnderMouse of
                Just hand ->
                    toggleHandSelection hand { model | mouse = Model.Pressed }

                Nothing ->
                    ( { model | mouse = Model.Pressed }, Cmd.none )

        MouseUp ->
            ( { model | mouse = Model.Released }, Cmd.none )

        ClearRange ->
            ( { model
                | rangeSelection = []
                , rangeSelectionWithCardRemoval = []
                , suitSelection = Nothing
                , slider = Model.initialRangeSlider
              }
            , Cmd.none
            )

        HandHover (Just hand) ->
            if model.mouse == Model.Pressed then
                toggleHandSelection hand { model | handUnderMouse = Just hand, ignoreRangeHoverState = False }

            else
                ( { model | handUnderMouse = Just hand, ignoreRangeHoverState = False }, Cmd.none )

        HandHover Nothing ->
            ( { model | handUnderMouse = Nothing, ignoreRangeHoverState = False }, Cmd.none )

        SelectPairs ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.pairs |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = Model.initialRangeSlider
              }
            , Cmd.none
            )
                |> triggerBounce

        SelectSuitedAces ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.suitedAces |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = Model.initialRangeSlider
              }
            , Cmd.none
            )
                |> triggerBounce

        SelectSuitedBroadways ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.suitedBroadways |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = Model.initialRangeSlider
              }
            , Cmd.none
            )
                |> triggerBounce

        SelectOffsuitAces ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.offsuitAces |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = Model.initialRangeSlider
              }
            , Cmd.none
            )
                |> triggerBounce

        SelectOffsuitBroadways ->
            ( { model
                | rangeSelection = model.rangeSelection ++ (HandOrCombo.offsuitBroadways |> List.concatMap HandOrCombo.combos) |> List.Extra.unique
                , slider = Model.initialRangeSlider
              }
            , Cmd.none
            )
                |> triggerBounce

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
                | form = Form.setRange position range model.form |> Form.rewrite position
                , apiResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )
                |> triggerBounce

        CopyToClipboard index sharingType ->
            let
                maybeText =
                    case sharingType of
                        URL ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map (\( _, url, _ ) -> Url.toString url)

                        Markdown ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map ((\( _, _, r ) -> r) >> ResultFormatter.markdown model.location)

                        TwoPlusTwo ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map ((\( _, _, r ) -> r) >> ResultFormatter.twoPlusTwo model.location)

                        PokerStrategy ->
                            model.results |> List.reverse |> List.Extra.getAt index |> Maybe.map ((\( _, _, r ) -> r) >> ResultFormatter.pokerStrategy model.location)
            in
            case maybeText of
                Just text ->
                    ( model, Ports.copyToClipboard (Ports.copyToclipboardMsgEncoder { text = text, index = index, sharingType = sharingType }) )

                Nothing ->
                    ( model, Cmd.none )

        RemoveRange position ->
            ( { model | form = Form.clearRange position model.form }, Cmd.none )
                |> triggerBounce

        NotifyCopyToClipboard (Err _) ->
            ( model, Cmd.none )

        NotifyCopyToClipboard (Ok copiedMsg) ->
            ( { model
                | results =
                    model.results
                        |> List.reverse
                        |> List.Extra.updateAt copiedMsg.index
                            (\( pos, url, simResult ) ->
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
                                , simResult
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
                |> triggerBounce

        RemoveBoard ->
            ( { model | form = Form.clearBoard model.form }, Cmd.none )
                |> triggerBounce

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
                                Model.initialSharingPopoverStates |> (\ipos -> { ipos | shareUrl = state })

                            Markdown ->
                                Model.initialSharingPopoverStates |> (\ipos -> { ipos | shareMd = state })

                            TwoPlusTwo ->
                                Model.initialSharingPopoverStates |> (\ipos -> { ipos | share2plus2 = state })

                            PokerStrategy ->
                                Model.initialSharingPopoverStates |> (\ipos -> { ipos | sharePs = state })
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
                |> triggerBounce

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
                |> triggerBounce

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

        BounceMsg ->
            let
                newBounce =
                    Bounce.pop model.bounce

                form =
                    if Bounce.steady newBounce then
                        if model.rangeSelectionModalVisibility /= Modal.shown then
                            Form.updateNumberOfCombos model.form

                        else
                            model.form

                    else
                        model.form

                rangeSelectionWithCardRemoval =
                    if model.rangeSelectionModalVisibility == Modal.shown && Bounce.steady newBounce then
                        model.rangeSelection
                            |> List.Extra.filterNot (\c -> List.member c model.blockedCombosForRangeSelection)

                    else
                        []
            in
            ( { model | bounce = newBounce, form = form, rangeSelectionWithCardRemoval = rangeSelectionWithCardRemoval }, Cmd.none )

        RemoveResult index ->
            ( { model
                | results =
                    model.results
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( i, result ) ->
                                if i == (model.results |> List.length |> (\length -> length - 1 - index)) then
                                    Nothing

                                else
                                    Just result
                            )
              }
            , Cmd.none
            )


triggerBounce : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
triggerBounce ( model, cmd ) =
    ( { model | bounce = Bounce.push model.bounce }, Cmd.batch [ cmd, Bounce.delay bounceDelay BounceMsg ] )


handleApiResponse : Model -> WebData ApiResponse -> ( Model, Cmd Msg )
handleApiResponse model response =
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

        simResult =
            response
                |> RemoteData.map
                    (\res ->
                        [ ( Form.range UTG model.form, UTG )
                        , ( Form.range MP model.form, MP )
                        , ( Form.range CO model.form, CO )
                        , ( Form.range BU model.form, BU )
                        , ( Form.range SB model.form, SB )
                        , ( Form.range BB model.form, BB )
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
                                    (Form.board model.form)
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
        | apiResponse = simResult
        , results = model.results ++ (simResult |> RemoteData.map List.singleton |> RemoteData.withDefault [] |> List.map (\r -> ( Model.initialSharingPopoverStates, model.location, r )))
      }
    , Cmd.none
    )


sendSimulationRequest : Model -> ( Model, Cmd Msg )
sendSimulationRequest model =
    if Form.validateForm model.form |> Result.Extra.isErr then
        ( model, Cmd.none )

    else if (Form.ranges model.form |> List.length) < 2 then
        ( model, Cmd.none )

    else
        ( { model
            | apiResponse = RemoteData.Loading
          }
        , Api.sendSimulationRequest model.simulationApibaseUrl (Form.board model.form) (Form.ranges model.form)
        )


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


toggleHandSelection : Hand -> Model -> ( Model, Cmd Msg )
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
    ( { model
        | rangeSelection = rangeSelection |> List.Extra.unique
        , ignoreRangeHoverState = True
        , slider = Model.initialRangeSlider
      }
    , Cmd.none
    )
        |> triggerBounce


confirmRangeSelection : Position -> Model -> ( Model, Cmd Msg )
confirmRangeSelection position model =
    let
        form =
            Form.setRange position (model.rangeSelection |> List.map HandOrCombo.fromCombo |> HandOrCombo.toNormalizedString) model.form
                |> Form.rewrite position
    in
    ( { model
        | rangeSelectionModalVisibility = Modal.hidden
        , form = form
        , rangeSelection = []
        , blockedCombosForRangeSelection = []
        , rangeSelectionWithCardRemoval = []
        , suitSelection = Nothing
        , apiResponse = RemoteData.NotAsked
      }
    , Cmd.none
    )


makeUrl : Model -> String
makeUrl model =
    let
        rangeQueryParameter position field =
            case field.validated of
                Err _ ->
                    []

                Ok [] ->
                    []

                Ok range ->
                    [ Url.Builder.string (position |> Position.toString |> String.toLower) (range |> HandOrCombo.toNormalizedString |> String.toLower) ]

        boardQueryParameter board =
            case board.validated of
                Err _ ->
                    []

                Ok [] ->
                    []

                Ok cards ->
                    [ Url.Builder.string "board" (cards |> List.map Card.toString |> String.concat |> String.toLower) ]
    in
    Url.Builder.absolute []
        (rangeQueryParameter UTG (Form.rangeField UTG model.form)
            ++ rangeQueryParameter MP (Form.rangeField MP model.form)
            ++ rangeQueryParameter CO (Form.rangeField CO model.form)
            ++ rangeQueryParameter BU (Form.rangeField BU model.form)
            ++ rangeQueryParameter SB (Form.rangeField SB model.form)
            ++ rangeQueryParameter BB (Form.rangeField BB model.form)
            ++ boardQueryParameter (Form.boardField model.form)
        )


updateUrl : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateUrl ( model, cmd ) =
    if model.form |> Form.validateForm |> Result.Extra.isOk then
        Cmd.batch [ cmd, Navigation.pushUrl model.navKey (makeUrl model) ]
            |> Tuple.pair model

    else
        ( model, cmd )


confirmBoardSelection : Model -> ( Model, Cmd Msg )
confirmBoardSelection model =
    ( { model
        | boardSelectModalVisibility = Modal.hidden
        , form = Form.setBoard (model.boardSelection |> List.map Card.toString |> String.concat) model.form
        , boardSelection = []
        , apiResponse = RemoteData.NotAsked
      }
    , Cmd.none
    )
