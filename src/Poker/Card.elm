module Poker.Card exposing
    ( Card
    , all
    , parser
    , toChar
    , toString
    )

import List.Extra
import Parser exposing ((|=))
import Poker.Rank as Rank exposing (Rank(..))
import Poker.Suit as Suit exposing (Suit(..))


type alias Card =
    { rank : Rank
    , suit : Suit
    }


all : List Card
all =
    Suit.all |> List.Extra.andThen (\suit -> Rank.all |> List.reverse |> List.map (\rank -> Card rank suit))


toString : Card -> String
toString { rank, suit } =
    Rank.toString rank ++ Suit.toString suit


toChar : Card -> Char
toChar { suit, rank } =
    case ( suit, rank ) of
        ( Club, Two ) ->
            '🃒'

        ( Spades, Two ) ->
            '🂢'

        ( Heart, Two ) ->
            '🂲'

        ( Diamond, Two ) ->
            '🃂'

        ( Club, Three ) ->
            '🃓'

        ( Spades, Three ) ->
            '🂣'

        ( Heart, Three ) ->
            '🂳'

        ( Diamond, Three ) ->
            '🃃'

        ( Club, Four ) ->
            '🃔'

        ( Spades, Four ) ->
            '🂤'

        ( Heart, Four ) ->
            '🂴'

        ( Diamond, Four ) ->
            '🃄'

        ( Club, Five ) ->
            '🃕'

        ( Spades, Five ) ->
            '🂥'

        ( Heart, Five ) ->
            '🂵'

        ( Diamond, Five ) ->
            '🃅'

        ( Club, Six ) ->
            '🃖'

        ( Spades, Six ) ->
            '🂦'

        ( Heart, Six ) ->
            '🂶'

        ( Diamond, Six ) ->
            '🃆'

        ( Club, Seven ) ->
            '🃗'

        ( Spades, Seven ) ->
            '🂧'

        ( Heart, Seven ) ->
            '🂷'

        ( Diamond, Seven ) ->
            '🃇'

        ( Club, Eight ) ->
            '🃘'

        ( Spades, Eight ) ->
            '🂨'

        ( Heart, Eight ) ->
            '🂸'

        ( Diamond, Eight ) ->
            '🃈'

        ( Club, Nine ) ->
            '🃙'

        ( Spades, Nine ) ->
            '🂩'

        ( Heart, Nine ) ->
            '🂹'

        ( Diamond, Nine ) ->
            '🃉'

        ( Club, Ten ) ->
            '🃚'

        ( Spades, Ten ) ->
            '🂪'

        ( Heart, Ten ) ->
            '🂺'

        ( Diamond, Ten ) ->
            '🃊'

        ( Club, Jack ) ->
            '🃛'

        ( Spades, Jack ) ->
            '🂫'

        ( Heart, Jack ) ->
            '🂻'

        ( Diamond, Jack ) ->
            '🃋'

        ( Club, Queen ) ->
            '🃝'

        ( Spades, Queen ) ->
            '🂭'

        ( Heart, Queen ) ->
            '🂽'

        ( Diamond, Queen ) ->
            '🃍'

        ( Club, King ) ->
            '🃞'

        ( Spades, King ) ->
            '🂮'

        ( Heart, King ) ->
            '🂾'

        ( Diamond, King ) ->
            '🃎'

        ( Club, Ace ) ->
            '🃑'

        ( Spades, Ace ) ->
            '🂡'

        ( Heart, Ace ) ->
            '🂱'

        ( Diamond, Ace ) ->
            '🃁'


parser : Parser.Parser Card
parser =
    Parser.succeed Card
        |= Rank.parser
        |= Suit.parser
