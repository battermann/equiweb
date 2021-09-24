module Poker.Data exposing (positionalRanges)

import Poker.Position exposing (Position(..))


type alias PositionalRange =
    { position : Position
    , label : String
    , range : String
    }


positionalRanges : List PositionalRange
positionalRanges =
    [ PositionalRange UTG "UTG RFI" utgRfi
    , PositionalRange MP "MP RFI" mpRfi
    , PositionalRange MP "MP 3bet vs UTG" mp3betVsUtg
    , PositionalRange CO "CO RFI" coRfi
    , PositionalRange CO "CO 3bet vs UTG" co3betVsUtg
    , PositionalRange CO "CO 3bet vs MP" co3betVsMp
    , PositionalRange BU "BU RFI" buRfi
    , PositionalRange BU "BU Call vs UTG" buCallVsUtg
    , PositionalRange BU "BU Call vs MP" buCallVsMp
    , PositionalRange BU "BU 3bet vs UTG" bu3betVsUtg
    , PositionalRange BU "BU 3bet vs MP" bu3betVsMp
    , PositionalRange BU "BU 3bet vs CO" bu3betvsCo
    , PositionalRange SB "SB RFI" sbRfi
    , PositionalRange SB "SB 3bet vs UTG" sb3betVsUtg
    , PositionalRange SB "SB 3bet vs MP" sbt3betVsMp
    , PositionalRange SB "SB 3bet vs CO" sb3betVsCo
    , PositionalRange SB "SB 3bet vs BU" sb3betVsBu
    , PositionalRange BB "BB Call vs UTG" bbCallVsUtg
    , PositionalRange BB "BB Call vs MP" bbCallVsMp
    , PositionalRange BB "BB Call vs CO" bbCallVsCo
    , PositionalRange BB "BB Call vs BU" bbCallVsBu
    , PositionalRange BB "BB Call vs SB" bbCallVsSb
    , PositionalRange BB "BB 3bet vs UTG" bb3betVsUtg
    , PositionalRange BB "BB 3bet vs MP" bb3betVsMp
    , PositionalRange BB "BB 3bet vs CO" bb3betVsCo
    , PositionalRange BB "BB 3bet vs BU" bb3betVsBu
    , PositionalRange BB "BB 3bet vs SB" bb3betVsSb
    ]


utgRfi : String
utgRfi =
    "AA-22, AKo-ATo, KQo-KJo, AKs-A2s, KQs-K8s, QJs-Q9s, JTs-J9s, T9s, 98s, 87s, 76s, 65s, 54s"


mpRfi : String
mpRfi =
    "A9o, AA-22, AKo-ATo, KQo-KTo, QJo-QTo, AKs-A2s, KQs-K5s, QJs-Q8s, JTs-J8s, T9s-T8s, 98s-97s, 87s-86s, 76s, 65s, 54s"


mp3betVsUtg : String
mp3betVsUtg =
    "99, QJs, JTs, T9s, 98s, AA-TT, AKo-AQo, AKs-ATs, A5s, KQs-KJs"


coRfi : String
coRfi =
    "A7o, AA-22, AKo-A8o, A5o, KQo-KTo, QJo-QTo, JTo, AKs-A2s, KQs-K3s, QJs-Q5s, JTs-J7s, T9s-T7s, 98s-97s, 87s-86s, 76s, 65s, 54s, 43s"


co3betVsUtg : String
co3betVsUtg =
    "99, QJs, JTs, T9s, 98s, AA-TT, AKo-AQo, AKs-ATs, A5s, KQs-KJs"


co3betVsMp : String
co3betVsMp =
    "99-77, JTs, T9s, 98s, 87s, 76s, 65s, AA-TT, AKo-AQo, AKs-ATs, A5s-A4s, KQs-KJs, QJs"


buRfi : String
buRfi =
    "K7o, 87o, J3s, 73s, AA-22, AKo-A2o, KQo-K8o, QJo-Q8o, JTo-J8o, T9o-T8o, 98o, AKs-A2s, KQs-K2s, QJs-Q2s, JTs-J4s, T9s-T5s, 98s-95s, 87s-84s, 76s-74s, 65s-63s, 54s-53s, 43s"


bu3betVsUtg : String
bu3betVsUtg =
    "88, AJo, KQo, A4s, T9s, 98s, 87s, 76s, AA-99, AKo-AQo, AKs-ATs, A5s, KQs-KJs, QJs, JTs"


bu3betVsMp : String
bu3betVsMp =
    "77, KQo, K9s, 87s, 76s, AA-88, AKo-AJo, AKs-A9s, A5s-A4s, KQs-KTs, QJs-QTs, JTs, T9s, 98s"


bu3betvsCo : String
bu3betvsCo =
    "KJo, A7s, K9s, Q9s, J9s, T8s, 65s, 54s, ATo, AA-66, AKo-AJo, KQo, AKs-A8s, A5s-A3s, KQs-KTs, QJs-QTs, JTs, T9s, 98s, 87s, 76s"


buCallVsUtg : String
buCallVsUtg =
    "55, JJ-66, AQo, KQo, AQs-ATs, KQs-KJs, QJs, JTs, T9s, 98s, 87s"


buCallVsMp : String
buCallVsMp =
    buCallVsUtg


sbRfi : String
sbRfi =
    "62s, Q7o, J7o, T7o, 97o, 86o, 52s, 42s, 32s, AA-22, AKo-A2o, KQo-K7o, QJo-Q8o, JTo-J8o, T9o-T8o, 98o, 87o, 76o, AKs-A2s, KQs-K2s, QJs-Q2s, JTs-J2s, T9s-T3s, 98s-93s, 87s-83s, 76s-73s, 65s-63s, 54s-53s, 43s"


sb3betVsUtg : String
sb3betVsUtg =
    "AQo, A4s, KTs, QTs, T9s, 98s, 87s, 76s, AA-99, AKo, AKs-ATs, A5s, KQs-KJs, QJs, JTs"


sbt3betVsMp : String
sbt3betVsMp =
    sb3betVsUtg


sb3betVsCo : String
sb3betVsCo =
    "77, AJo, KQo, A4s, J9s, 98s, 65s, AA-88, AKo-AQo, AKs-ATs, A5s, KQs-KTs, QJs-QTs, JTs, T9s, 87s, 76s"


sb3betVsBu : String
sb3betVsBu =
    "55, ATo, KJo, A7s, A3s, T8s, 54s, AA-66, AKo-AJo, KQo, AKs-A8s, A5s-A4s, KQs-K9s, QJs-Q9s, JTs-J9s, T9s, 98s, 87s, 76s, 65s"


bb3betVsUtg : String
bb3betVsUtg =
    "AA-JJ, AKo, AKs-ATs, A5s-A3s, KQs-KTs, QJs-QTs, JTs"


bb3betVsMp : String
bb3betVsMp =
    bb3betVsUtg


bb3betVsCo : String
bb3betVsCo =
    "AA-99, AKo, AKs-A9s, A5s-A3s, KQs-KTs, K6s-K4s, QJs-Q9s, JTs-J9s, T9s-T8s, 98s-97s, 87s, 76s, 65s, 54s"


bb3betVsBu : String
bb3betVsBu =
    "AA-88, AKo-AJo, KQo-KJo, AKs-A8s, A5s-A3s, KQs-K9s, K6s-K4s, QJs-Q9s, JTs-J8s, T9s-T8s, 98s, 87s, 76s, 65s, 54s"


bb3betVsSb : String
bb3betVsSb =
    "K5o, AA-77, AKo-AJo, A5o-A2o, KQo, K8o-K6o, Q8o, J8o, T8o, AKs-A9s, A5s-A3s, KQs-K9s, K7s-K6s, QJs-QTs, JTs-J9s, J4s-J3s, T9s-T8s, T5s-T2s, 98s-97s, 87s, 76s, 65s, 54s"


bbCallVsUtg : String
bbCallVsUtg =
    "JJ-22, AQo-ATo, KQo-KJo, QJo, ATs-A2s, KTs-K5s, QTs-Q7s, JTs-J7s, T9s-T7s, 98s-96s, 87s-86s, 76s-75s, 65s-64s, 54s-53s, 43s"


bbCallVsMp : String
bbCallVsMp =
    bbCallVsUtg


bbCallVsCo : String
bbCallVsCo =
    "A8o, A5o, J9o, T9o, Q3s-Q2s, 52s, 42s, 32s, 99-22, AQo-A9o, KQo-KTo, QJo-QTo, JTo, ATs-A2s, KTs-K2s, Q9s-Q4s, J9s-J6s, T9s-T6s, 98s-95s, 87s-84s, 76s-74s, 65s-63s, 54s-53s, 43s"


bbCallVsBu : String
bbCallVsBu =
    "88-22, AJo-A4o, KQo-K6o, QJo-Q8o, JTo-J8o, T9o-T8o, 98o-97o, 87o, 76o, A9s-A6s, A3s-A2s, K9s-K2s, Q8s-Q2s, J8s-J2s, T7s-T5s, 97s-95s, 86s-84s, 75s-73s, 64s-63s, 54s-52s, 43s-42s, 32s"


bbCallVsSb : String
bbCallVsSb =
    "86o, 65o, 54o, 62s, 88-22, AJo-A2o, KQo-K6o, QJo-Q8o, JTo-J8o, T9o-T8o, 98o-97o, 87o, 76o, A9s-A6s, A3s-A2s, K9s-K2s, QTs-Q2s, J9s-J2s, T8s-T2s, 97s-93s, 86s-84s, 75s-73s, 64s-63s, 53s-52s, 43s-42s, 32s"
