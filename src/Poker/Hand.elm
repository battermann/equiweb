module Poker.Hand exposing (CombosOfHand(..), Hand, allOrderedByRank, allWithAccumulatedNumberOfCombosOrderedByRank, combine, combos, combosOfHand, fold, grid, highCard, isOffsuit, isPair, isSuited, lowCard, offsuit, offsuitAces, offsuitBroadways, order, pair, pairs, parser, suited, suitedAces, suitedBroadways, toRangeNotation, toString)

import List
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Poker.Card exposing (Card)
import Poker.Combo as Combo exposing (Combo)
import Poker.RangeNotation exposing (RangeNotation(..))
import Poker.Rank as Rank exposing (Rank(..))
import Poker.Suit as Suit


type Hand
    = Pair Rank
    | Suited Rank Rank
    | Offsuit Rank Rank


combos : Hand -> List Combo
combos hand =
    case hand of
        Pair rank ->
            Suit.suitCombinations
                |> List.map (\( s1, s2 ) -> Combo.combo (Card rank s1) (Card rank s2))
                |> Maybe.Extra.values

        Suited high low ->
            Suit.all
                |> List.map (\suit -> Combo.combo (Card high suit) (Card low suit))
                |> Maybe.Extra.values

        Offsuit high low ->
            Suit.all
                |> List.concatMap (\s1 -> Suit.all |> List.map (Tuple.pair s1))
                |> List.filter (\( s1, s2 ) -> s1 /= s2)
                |> List.map (\( s1, s2 ) -> Combo.combo (Card high s1) (Card low s2))
                |> Maybe.Extra.values


grid : List (List Hand)
grid =
    Rank.all
        |> List.reverse
        |> List.map
            (\r1 ->
                Rank.all
                    |> List.reverse
                    |> List.map
                        (\r2 ->
                            if r1 == r2 then
                                Pair r1

                            else if r1 |> Rank.gt r2 then
                                Suited r1 r2

                            else
                                Offsuit r2 r1
                        )
            )


isOffsuit : Hand -> Bool
isOffsuit hand =
    case hand of
        Offsuit _ _ ->
            True

        _ ->
            False


isPair : Hand -> Bool
isPair hand =
    case hand of
        Pair _ ->
            True

        _ ->
            False


isSuited : Hand -> Bool
isSuited hand =
    case hand of
        Suited _ _ ->
            True

        _ ->
            False


toString : Hand -> String
toString hand =
    case hand of
        Pair rank ->
            Rank.toString rank ++ Rank.toString rank

        Suited high low ->
            Rank.toString high ++ Rank.toString low ++ "s"

        Offsuit high low ->
            Rank.toString high ++ Rank.toString low ++ "o"


highCard : Hand -> Rank
highCard hand =
    case hand of
        Pair rank ->
            rank

        Suited rank _ ->
            rank

        Offsuit rank _ ->
            rank


lowCard : Hand -> Rank
lowCard hand =
    case hand of
        Pair rank ->
            rank

        Suited _ rank ->
            rank

        Offsuit _ rank ->
            rank


pair : Rank -> Hand
pair rank =
    Pair rank


suited : Rank -> Rank -> Maybe Hand
suited r1 r2 =
    if r1 == r2 then
        Nothing

    else if r1 |> Rank.gt r2 then
        Just <| Suited r1 r2

    else
        Just <| Suited r2 r1


offsuit : Rank -> Rank -> Maybe Hand
offsuit r1 r2 =
    if r1 == r2 then
        Nothing

    else if r1 |> Rank.gt r2 then
        Just <| Offsuit r1 r2

    else
        Just <| Offsuit r2 r1


parser : Parser (List Hand)
parser =
    Parser.oneOf
        [ Parser.backtrackable rangeParser |. Parser.end
        , Parser.backtrackable rangePlusParser |. Parser.end
        , singleHandParser |. Parser.end
        ]


singleHandParser : Parser (List Hand)
singleHandParser =
    Parser.oneOf
        [ Parser.backtrackable (pairParser |> Parser.map List.singleton)
        , Parser.backtrackable (suitedParser |> Parser.map List.singleton)
        , Parser.backtrackable (offsuitParser |> Parser.map List.singleton)
        , anySuitParser
        ]


ranksParser : Parser ( Rank, Rank )
ranksParser =
    Parser.succeed Tuple.pair
        |= Rank.parser
        |= Rank.parser


equalRanksParser : Parser.Parser Rank
equalRanksParser =
    ranksParser
        |> Parser.andThen
            (\( r1, r2 ) ->
                if r1 == r2 then
                    Parser.succeed r1

                else
                    Parser.problem "Ranks are not equal"
            )


pairParser : Parser.Parser Hand
pairParser =
    equalRanksParser |> Parser.map Pair


validateUnpaired : ( Rank, Rank ) -> Parser ( Rank, Rank )
validateUnpaired ( r1, r2 ) =
    if r1 |> Rank.gt r2 then
        Parser.succeed ( r1, r2 )

    else if r2 |> Rank.gt r1 then
        Parser.succeed ( r2, r1 )

    else
        Parser.problem "Ranks must not be equal"


unpairedParser : Parser ( Rank, Rank )
unpairedParser =
    ranksParser |> Parser.andThen validateUnpaired


toSuited : ( Rank, Rank ) -> Parser Hand
toSuited ( r1, r2 ) =
    case suited r1 r2 of
        Just hand ->
            Parser.succeed hand

        Nothing ->
            Parser.problem "Suited hand is not valid"


toOffsuit : ( Rank, Rank ) -> Parser Hand
toOffsuit ( r1, r2 ) =
    case offsuit r1 r2 of
        Just hand ->
            Parser.succeed hand

        Nothing ->
            Parser.problem "Offsuit hand is not valid"


suitedParser : Parser Hand
suitedParser =
    (unpairedParser |> Parser.andThen toSuited)
        |. Parser.symbol "s"


offsuitParser : Parser Hand
offsuitParser =
    (unpairedParser |> Parser.andThen toOffsuit)
        |. Parser.symbol "o"


anySuitParser : Parser (List Hand)
anySuitParser =
    unpairedParser
        |> Parser.andThen
            (\( r1, r2 ) ->
                toSuited ( r1, r2 )
                    |> Parser.andThen
                        (\suitedHand ->
                            toOffsuit ( r1, r2 )
                                |> Parser.map (\offsuitHand -> [ suitedHand, offsuitHand ])
                        )
            )


rangeParser : Parser (List Hand)
rangeParser =
    Parser.andThen combineTuplesToRanges <|
        Parser.succeed Tuple.pair
            |= singleHandParser
            |. Parser.symbol "-"
            |= singleHandParser


combineTuplesToRanges : ( List Hand, List Hand ) -> Parser (List Hand)
combineTuplesToRanges ( list1, list2 ) =
    List.map2 validateRange list1 list2
        |> List.foldl (\p1 p2 -> p1 |> Parser.andThen (\r1 -> p2 |> Parser.map (\r2 -> r2 ++ r1))) (Parser.succeed [])
        |> Parser.andThen validateNonEmpty


validateNonEmpty : List a -> Parser (List a)
validateNonEmpty =
    \list ->
        case list of
            [] ->
                Parser.problem "Range must not be empty"

            xs ->
                Parser.succeed xs


validateRange : Hand -> Hand -> Parser (List Hand)
validateRange h1 h2 =
    case ( h1, h2 ) of
        ( Suited high1 low1, Suited high2 low2 ) ->
            if high1 == high2 then
                Parser.succeed (Rank.range low1 low2 |> List.map (Suited high1))

            else
                Parser.problem "This is not a valid hand range"

        ( Offsuit high1 low1, Offsuit high2 low2 ) ->
            if high1 == high2 then
                Parser.succeed (Rank.range low1 low2 |> List.map (Offsuit high1))

            else
                Parser.problem "This is not a valid hand range"

        ( Pair r1, Pair r2 ) ->
            Parser.succeed (Rank.range r1 r2 |> List.map Pair)

        _ ->
            Parser.problem "This is not a valid hand range"


rangePlusParser : Parser (List Hand)
rangePlusParser =
    Parser.andThen combineHandsToPlusRanges <|
        singleHandParser
            |. Parser.symbol "+"


combineHandsToPlusRanges : List Hand -> Parser (List Hand)
combineHandsToPlusRanges =
    List.foldl (\h p -> validateRangePlus h |> Parser.andThen (\r1 -> p |> Parser.map (\r2 -> r2 ++ r1))) (Parser.succeed [])


validateRangePlus : Hand -> Parser (List Hand)
validateRangePlus hand =
    case hand of
        Suited high low ->
            Parser.succeed (Rank.range high low |> List.drop 1 |> List.map (Suited high))

        Offsuit high low ->
            Parser.succeed (Rank.range high low |> List.drop 1 |> List.map (Offsuit high))

        Pair r ->
            Parser.succeed (Rank.from r |> List.map Pair)


order : Hand -> Hand -> Order
order lhs rhs =
    case ( lhs, rhs ) of
        ( Pair r1, Pair r2 ) ->
            if r1 |> Rank.gt r2 then
                GT

            else if r1 |> Rank.lt r2 then
                LT

            else
                EQ

        ( Pair _, _ ) ->
            GT

        ( _, Pair _ ) ->
            LT

        ( Suited _ _, Offsuit _ _ ) ->
            GT

        ( Suited h1 l1, Suited h2 l2 ) ->
            if h1 |> Rank.gt h2 then
                GT

            else if h1 |> Rank.lt h2 then
                LT

            else if l1 |> Rank.gt l2 then
                GT

            else if l1 |> Rank.lt l2 then
                LT

            else
                EQ

        ( _, Suited _ _ ) ->
            LT

        ( Offsuit h1 l1, Offsuit h2 l2 ) ->
            if h1 |> Rank.gt h2 then
                GT

            else if h1 |> Rank.lt h2 then
                LT

            else if l1 |> Rank.gt l2 then
                GT

            else if l1 |> Rank.lt l2 then
                LT

            else
                EQ


toRangeNotation : Hand -> RangeNotation
toRangeNotation hand =
    case hand of
        Pair Rank.Ace ->
            PairPlus Rank.Ace

        Pair belowAce ->
            PairRange belowAce belowAce

        Suited h l ->
            if Rank.isConnected h l then
                SuitedPlus h l

            else
                SuitedRange h l l

        Offsuit h l ->
            if Rank.isConnected h l then
                OffsuitPlus h l

            else
                OffsuitRange h l l


combine : Hand -> RangeNotation -> List RangeNotation
combine hand ranges =
    case ( hand, ranges ) of
        ( Pair low, PairPlus high ) ->
            if Rank.isConnected high low then
                [ PairPlus low ]

            else
                [ toRangeNotation hand, ranges ]

        ( Pair lowest, PairRange high low ) ->
            if Rank.isConnected low lowest then
                [ PairRange high lowest ]

            else
                [ toRangeNotation hand, ranges ]

        ( Pair _, _ ) ->
            [ toRangeNotation hand, ranges ]

        ( Suited high lowest, SuitedPlus otherHigh low ) ->
            if high == otherHigh && Rank.isConnected low lowest then
                [ SuitedPlus high lowest ]

            else
                [ toRangeNotation hand, ranges ]

        ( Suited high lowest, SuitedRange otherHigh lowTo lowFrom ) ->
            if high == otherHigh && Rank.isConnected lowFrom lowest then
                [ SuitedRange high lowTo lowest ]

            else
                [ toRangeNotation hand, ranges ]

        ( Suited _ _, _ ) ->
            [ toRangeNotation hand, ranges ]

        ( Offsuit high lowest, OffsuitPlus otherHigh low ) ->
            if high == otherHigh && Rank.isConnected low lowest then
                [ OffsuitPlus high lowest ]

            else
                [ toRangeNotation hand, ranges ]

        ( Offsuit high lowest, OffsuitRange otherHigh lowTo lowFrom ) ->
            if high == otherHigh && Rank.isConnected lowFrom lowest then
                [ OffsuitRange high lowTo lowest ]

            else
                [ toRangeNotation hand, ranges ]

        ( Offsuit _ _, _ ) ->
            [ toRangeNotation hand, ranges ]


pairs : List Hand
pairs =
    Rank.all |> List.map Pair


suitedAces : List Hand
suitedAces =
    Rank.all |> List.filter ((/=) Rank.Ace) |> List.map (Suited Rank.Ace)


offsuitAces : List Hand
offsuitAces =
    Rank.all |> List.filter ((/=) Rank.Ace) |> List.map (Offsuit Rank.Ace)


suitedBroadways : List Hand
suitedBroadways =
    [ Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Ten ]
        |> List.Extra.uniquePairs
        |> List.map (\( r1, r2 ) -> suited r1 r2)
        |> Maybe.Extra.values


offsuitBroadways : List Hand
offsuitBroadways =
    [ Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Ten ]
        |> List.Extra.uniquePairs
        |> List.map (\( r1, r2 ) -> offsuit r1 r2)
        |> Maybe.Extra.values


allOrderedByRank : List Hand
allOrderedByRank =
    allWithAccumulatedNumberOfCombosOrderedByRank |> List.map (\( h, _, _ ) -> h)


fold : (Rank -> a) -> (Rank -> Rank -> a) -> (Rank -> Rank -> a) -> Hand -> a
fold onPair onSuited onOffsuit hand =
    case hand of
        Pair r ->
            onPair r

        Suited h l ->
            onSuited h l

        Offsuit h l ->
            onOffsuit h l


type CombosOfHand
    = All
    | None
    | Some Int


combosOfHand : Hand -> List Combo -> CombosOfHand
combosOfHand hand cs =
    case hand of
        Pair r ->
            case cs |> Combo.getPairs r |> List.length of
                0 ->
                    None

                6 ->
                    All

                n ->
                    Some n

        Suited h l ->
            case cs |> Combo.getSuited h l |> List.length of
                0 ->
                    None

                4 ->
                    All

                n ->
                    Some n

        Offsuit h l ->
            case cs |> Combo.getOffsuit h l |> List.length of
                0 ->
                    None

                12 ->
                    All

                n ->
                    Some n


allWithAccumulatedNumberOfCombosOrderedByRank : List ( Hand, Int, Float )
allWithAccumulatedNumberOfCombosOrderedByRank =
    [ ( Pair Ace, 6, 0.004524886877828055 )
    , ( Pair King, 12, 0.00904977375565611 )
    , ( Pair Queen, 18, 0.013574660633484163 )
    , ( Pair Jack, 24, 0.01809954751131222 )
    , ( Pair Ten, 30, 0.02262443438914027 )
    , ( Suited Ace King, 34, 0.02564102564102564 )
    , ( Pair Nine, 40, 0.030165912518853696 )
    , ( Suited Ace Queen, 44, 0.033182503770739065 )
    , ( Offsuit Ace King, 56, 0.042232277526395176 )
    , ( Suited Ace Jack, 60, 0.04524886877828054 )
    , ( Suited King Queen, 64, 0.048265460030165915 )
    , ( Pair Eight, 70, 0.05279034690799397 )
    , ( Suited Ace Ten, 74, 0.05580693815987934 )
    , ( Offsuit Ace Queen, 86, 0.06485671191553545 )
    , ( Suited King Jack, 90, 0.06787330316742081 )
    , ( Suited King Ten, 94, 0.07088989441930618 )
    , ( Suited Queen Jack, 98, 0.07390648567119155 )
    , ( Offsuit Ace Jack, 110, 0.08295625942684766 )
    , ( Offsuit King Queen, 122, 0.09200603318250378 )
    , ( Suited Queen Ten, 126, 0.09502262443438914 )
    , ( Suited Ace Nine, 130, 0.09803921568627451 )
    , ( Pair Seven, 136, 0.10256410256410256 )
    , ( Offsuit Ace Ten, 148, 0.11161387631975868 )
    , ( Suited Jack Ten, 152, 0.11463046757164404 )
    , ( Offsuit King Jack, 164, 0.12368024132730016 )
    , ( Suited Ace Eight, 168, 0.12669683257918551 )
    , ( Suited King Nine, 172, 0.1297134238310709 )
    , ( Offsuit Queen Jack, 184, 0.138763197586727 )
    , ( Suited Ace Seven, 188, 0.14177978883861236 )
    , ( Offsuit King Ten, 200, 0.15082956259426847 )
    , ( Suited Queen Nine, 204, 0.15384615384615385 )
    , ( Suited Ace Five, 208, 0.1568627450980392 )
    , ( Pair Six, 214, 0.16138763197586728 )
    , ( Suited Ace Six, 218, 0.16440422322775264 )
    , ( Offsuit Queen Ten, 230, 0.17345399698340874 )
    , ( Suited Jack Nine, 234, 0.17647058823529413 )
    , ( Offsuit Ace Nine, 246, 0.18552036199095023 )
    , ( Suited Ten Nine, 250, 0.1885369532428356 )
    , ( Suited Ace Four, 254, 0.19155354449472098 )
    , ( Suited King Eight, 258, 0.19457013574660634 )
    , ( Offsuit Jack Ten, 270, 0.20361990950226244 )
    , ( Suited King Seven, 274, 0.2066365007541478 )
    , ( Offsuit Ace Eight, 286, 0.21568627450980393 )
    , ( Suited Ace Three, 290, 0.2187028657616893 )
    , ( Suited Queen Eight, 294, 0.22171945701357465 )
    , ( Offsuit King Nine, 306, 0.23076923076923078 )
    , ( Suited Ace Two, 310, 0.23378582202111614 )
    , ( Suited King Six, 314, 0.2368024132730015 )
    , ( Suited Jack Eight, 318, 0.2398190045248869 )
    , ( Suited Ten Eight, 322, 0.24283559577677225 )
    , ( Offsuit Ace Seven, 334, 0.25188536953242835 )
    , ( Pair Five, 340, 0.2564102564102564 )
    , ( Offsuit Queen Nine, 352, 0.2654600301659125 )
    , ( Suited Nine Eight, 356, 0.2684766214177979 )
    , ( Suited King Five, 360, 0.27149321266968324 )
    , ( Suited Queen Seven, 364, 0.27450980392156865 )
    , ( Offsuit Jack Nine, 376, 0.28355957767722473 )
    , ( Offsuit Ace Five, 388, 0.29260935143288086 )
    , ( Offsuit Ten Nine, 400, 0.30165912518853694 )
    , ( Offsuit Ace Six, 412, 0.31070889894419307 )
    , ( Suited King Four, 416, 0.3137254901960784 )
    , ( Offsuit King Eight, 428, 0.32277526395173456 )
    , ( Suited Queen Six, 432, 0.3257918552036199 )
    , ( Suited Jack Seven, 436, 0.3288084464555053 )
    , ( Suited Ten Seven, 440, 0.33182503770739064 )
    , ( Offsuit Ace Four, 452, 0.34087481146304677 )
    , ( Suited Nine Seven, 456, 0.3438914027149321 )
    , ( Suited King Three, 460, 0.3469079939668175 )
    , ( Suited Eight Seven, 464, 0.34992458521870284 )
    , ( Suited Queen Five, 468, 0.35294117647058826 )
    , ( Offsuit King Seven, 480, 0.36199095022624433 )
    , ( Pair Four, 486, 0.3665158371040724 )
    , ( Offsuit Queen Eight, 498, 0.3755656108597285 )
    , ( Offsuit Ace Three, 510, 0.38461538461538464 )
    , ( Suited King Two, 514, 0.38763197586727 )
    , ( Offsuit Jack Eight, 526, 0.39668174962292607 )
    , ( Suited Queen Four, 530, 0.3996983408748115 )
    , ( Offsuit Ten Eight, 542, 0.40874811463046756 )
    , ( Suited Jack Six, 546, 0.4117647058823529 )
    , ( Offsuit King Six, 558, 0.42081447963800905 )
    , ( Offsuit Ace Two, 570, 0.4298642533936652 )
    , ( Suited Ten Six, 574, 0.43288084464555054 )
    , ( Offsuit Nine Eight, 586, 0.4419306184012066 )
    , ( Suited Seven Six, 590, 0.444947209653092 )
    , ( Suited Eight Six, 594, 0.4479638009049774 )
    , ( Suited Nine Six, 598, 0.45098039215686275 )
    , ( Suited Queen Three, 602, 0.4539969834087481 )
    , ( Suited Jack Five, 606, 0.45701357466063347 )
    , ( Offsuit King Five, 618, 0.4660633484162896 )
    , ( Offsuit Queen Seven, 630, 0.4751131221719457 )
    , ( Suited Queen Two, 634, 0.4781297134238311 )
    , ( Suited Jack Four, 638, 0.48114630467571645 )
    , ( Pair Three, 644, 0.4856711915535445 )
    , ( Suited Six Five, 648, 0.48868778280542985 )
    , ( Offsuit Jack Seven, 660, 0.497737556561086 )
    , ( Offsuit Ten Seven, 672, 0.5067873303167421 )
    , ( Offsuit King Four, 684, 0.5158371040723982 )
    , ( Suited Seven Five, 688, 0.5188536953242836 )
    , ( Suited Ten Five, 692, 0.521870286576169 )
    , ( Offsuit Queen Six, 704, 0.530920060331825 )
    , ( Suited Jack Three, 708, 0.5339366515837104 )
    , ( Suited Nine Five, 712, 0.5369532428355958 )
    , ( Offsuit Eight Seven, 724, 0.5460030165912518 )
    , ( Suited Eight Five, 728, 0.5490196078431373 )
    , ( Offsuit Nine Seven, 740, 0.5580693815987934 )
    , ( Suited Ten Four, 744, 0.5610859728506787 )
    , ( Offsuit King Three, 756, 0.5701357466063348 )
    , ( Suited Jack Two, 760, 0.5731523378582202 )
    , ( Suited Five Four, 764, 0.5761689291101055 )
    , ( Offsuit Queen Five, 776, 0.5852187028657617 )
    , ( Suited Six Four, 780, 0.5882352941176471 )
    , ( Suited Ten Three, 784, 0.5912518853695324 )
    , ( Pair Two, 790, 0.5957767722473605 )
    , ( Offsuit King Two, 802, 0.6048265460030166 )
    , ( Suited Seven Four, 806, 0.6078431372549019 )
    , ( Offsuit Seven Six, 818, 0.6168929110105581 )
    , ( Suited Ten Two, 822, 0.6199095022624435 )
    , ( Offsuit Queen Four, 834, 0.6289592760180995 )
    , ( Offsuit Jack Six, 846, 0.6380090497737556 )
    , ( Suited Eight Four, 850, 0.6410256410256411 )
    , ( Suited Nine Four, 854, 0.6440422322775264 )
    , ( Offsuit Eight Six, 866, 0.6530920060331825 )
    , ( Offsuit Ten Six, 878, 0.6621417797888386 )
    , ( Offsuit Nine Six, 890, 0.6711915535444947 )
    , ( Suited Five Three, 894, 0.6742081447963801 )
    , ( Suited Nine Three, 898, 0.6772247360482655 )
    , ( Offsuit Queen Three, 910, 0.6862745098039216 )
    , ( Offsuit Jack Five, 922, 0.6953242835595776 )
    , ( Suited Six Three, 926, 0.698340874811463 )
    , ( Suited Four Three, 930, 0.7013574660633484 )
    , ( Suited Nine Two, 934, 0.7043740573152338 )
    , ( Suited Seven Three, 938, 0.7073906485671192 )
    , ( Offsuit Six Five, 950, 0.7164404223227753 )
    , ( Offsuit Queen Two, 962, 0.7254901960784313 )
    , ( Offsuit Jack Four, 974, 0.7345399698340875 )
    , ( Suited Eight Three, 978, 0.7375565610859729 )
    , ( Offsuit Seven Five, 990, 0.746606334841629 )
    , ( Suited Five Two, 994, 0.7496229260935143 )
    , ( Offsuit Eight Five, 1006, 0.7586726998491704 )
    , ( Suited Eight Two, 1010, 0.7616892911010558 )
    , ( Offsuit Ten Five, 1022, 0.770739064856712 )
    , ( Offsuit Nine Five, 1034, 0.779788838612368 )
    , ( Offsuit Jack Three, 1046, 0.7888386123680241 )
    , ( Suited Six Two, 1050, 0.7918552036199095 )
    , ( Offsuit Five Four, 1062, 0.8009049773755657 )
    , ( Suited Four Two, 1066, 0.803921568627451 )
    , ( Offsuit Ten Four, 1078, 0.8129713423831071 )
    , ( Offsuit Jack Two, 1090, 0.8220211161387632 )
    , ( Suited Seven Two, 1094, 0.8250377073906485 )
    , ( Offsuit Six Four, 1106, 0.8340874811463047 )
    , ( Offsuit Ten Three, 1118, 0.8431372549019608 )
    , ( Suited Three Two, 1122, 0.8461538461538461 )
    , ( Offsuit Seven Four, 1134, 0.8552036199095022 )
    , ( Offsuit Eight Four, 1146, 0.8642533936651584 )
    , ( Offsuit Ten Two, 1158, 0.8733031674208145 )
    , ( Offsuit Nine Four, 1170, 0.8823529411764706 )
    , ( Offsuit Five Three, 1182, 0.8914027149321267 )
    , ( Offsuit Nine Three, 1194, 0.9004524886877828 )
    , ( Offsuit Six Three, 1206, 0.9095022624434389 )
    , ( Offsuit Four Three, 1218, 0.918552036199095 )
    , ( Offsuit Nine Two, 1230, 0.9276018099547512 )
    , ( Offsuit Seven Three, 1242, 0.9366515837104072 )
    , ( Offsuit Eight Three, 1254, 0.9457013574660633 )
    , ( Offsuit Five Two, 1266, 0.9547511312217195 )
    , ( Offsuit Eight Two, 1278, 0.9638009049773756 )
    , ( Offsuit Four Two, 1290, 0.9728506787330317 )
    , ( Offsuit Six Two, 1302, 0.9819004524886877 )
    , ( Offsuit Seven Two, 1314, 0.9909502262443439 )
    , ( Offsuit Three Two, 1326, 1 )
    ]
