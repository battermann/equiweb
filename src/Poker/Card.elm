module Poker.Card exposing
    ( Card
    , all
    , boardToString
    , order
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
    Suit.all |> List.Extra.andThen (\suit -> Rank.all |> List.map (\rank -> Card rank suit))


toString : Card -> String
toString { rank, suit } =
    Rank.toString rank ++ Suit.toString suit


toChar : Card -> Char
toChar { suit, rank } =
    case ( suit, rank ) of
        ( Clubs, Two ) ->
            '🃒'

        ( Spades, Two ) ->
            '🂢'

        ( Hearts, Two ) ->
            '🂲'

        ( Diamonds, Two ) ->
            '🃂'

        ( Clubs, Three ) ->
            '🃓'

        ( Spades, Three ) ->
            '🂣'

        ( Hearts, Three ) ->
            '🂳'

        ( Diamonds, Three ) ->
            '🃃'

        ( Clubs, Four ) ->
            '🃔'

        ( Spades, Four ) ->
            '🂤'

        ( Hearts, Four ) ->
            '🂴'

        ( Diamonds, Four ) ->
            '🃄'

        ( Clubs, Five ) ->
            '🃕'

        ( Spades, Five ) ->
            '🂥'

        ( Hearts, Five ) ->
            '🂵'

        ( Diamonds, Five ) ->
            '🃅'

        ( Clubs, Six ) ->
            '🃖'

        ( Spades, Six ) ->
            '🂦'

        ( Hearts, Six ) ->
            '🂶'

        ( Diamonds, Six ) ->
            '🃆'

        ( Clubs, Seven ) ->
            '🃗'

        ( Spades, Seven ) ->
            '🂧'

        ( Hearts, Seven ) ->
            '🂷'

        ( Diamonds, Seven ) ->
            '🃇'

        ( Clubs, Eight ) ->
            '🃘'

        ( Spades, Eight ) ->
            '🂨'

        ( Hearts, Eight ) ->
            '🂸'

        ( Diamonds, Eight ) ->
            '🃈'

        ( Clubs, Nine ) ->
            '🃙'

        ( Spades, Nine ) ->
            '🂩'

        ( Hearts, Nine ) ->
            '🂹'

        ( Diamonds, Nine ) ->
            '🃉'

        ( Clubs, Ten ) ->
            '🃚'

        ( Spades, Ten ) ->
            '🂪'

        ( Hearts, Ten ) ->
            '🂺'

        ( Diamonds, Ten ) ->
            '🃊'

        ( Clubs, Jack ) ->
            '🃛'

        ( Spades, Jack ) ->
            '🂫'

        ( Hearts, Jack ) ->
            '🂻'

        ( Diamonds, Jack ) ->
            '🃋'

        ( Clubs, Queen ) ->
            '🃝'

        ( Spades, Queen ) ->
            '🂭'

        ( Hearts, Queen ) ->
            '🂽'

        ( Diamonds, Queen ) ->
            '🃍'

        ( Clubs, King ) ->
            '🃞'

        ( Spades, King ) ->
            '🂮'

        ( Hearts, King ) ->
            '🂾'

        ( Diamonds, King ) ->
            '🃎'

        ( Clubs, Ace ) ->
            '🃑'

        ( Spades, Ace ) ->
            '🂡'

        ( Hearts, Ace ) ->
            '🂱'

        ( Diamonds, Ace ) ->
            '🃁'


parser : Parser.Parser Card
parser =
    Parser.succeed Card
        |= Rank.parser
        |= Suit.parser


order : Card -> Card -> Order
order card1 card2 =
    case Rank.order card1.rank card2.rank of
        EQ ->
            Suit.order card1.suit card2.suit

        orElse ->
            orElse


boardToString : List Card -> String
boardToString cards =
    case cards of
        [] ->
            "Preflop"

        _ :: _ :: _ :: [] ->
            cards |> List.map toString |> String.concat

        flop1 :: flop2 :: flop3 :: turn :: [] ->
            [ toString flop1, toString flop2, toString flop3, " ", toString turn ] |> String.concat

        flop1 :: flop2 :: flop3 :: turn :: river :: [] ->
            [ toString flop1, toString flop2, toString flop3, " ", toString turn, " ", toString river ] |> String.concat

        _ ->
            ""
