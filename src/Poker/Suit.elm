module Poker.Suit exposing
    ( Selection
    , Suit(..)
    , all
    , gt
    , gte
    , initialSelection
    , isSelectionEmpty
    , lt
    , lte
    , order
    , parser
    , suitCombinations
    , suitToChar
    , toString
    , toggleOffSuitSelection
    , togglePairsSelection
    , toggleSuitedSelection
    )

import List.Extra
import Parser


type Suit
    = Clubs
    | Spades
    | Hearts
    | Diamonds


all : List Suit
all =
    [ Clubs
    , Spades
    , Hearts
    , Diamonds
    ]


gt : Suit -> Suit -> Bool
gt =
    compare (>)


gte : Suit -> Suit -> Bool
gte =
    compare (>=)


lt : Suit -> Suit -> Bool
lt =
    compare (<)


lte : Suit -> Suit -> Bool
lte =
    compare (<=)


compare : (Int -> Int -> Bool) -> Suit -> Suit -> Bool
compare f lhs rhs =
    f (suitToInt rhs) (suitToInt lhs)


suitToInt : Suit -> Int
suitToInt suit =
    case suit of
        Clubs ->
            4

        Spades ->
            3

        Hearts ->
            2

        Diamonds ->
            1


suitCombinations : List ( Suit, Suit )
suitCombinations =
    List.Extra.uniquePairs all


suitToChar : Suit -> Char
suitToChar suit =
    case suit of
        Spades ->
            '♠'

        Clubs ->
            '♣'

        Hearts ->
            '♥'

        Diamonds ->
            '♦'


toString : Suit -> String
toString suit =
    case suit of
        Spades ->
            "s"

        Clubs ->
            "c"

        Hearts ->
            "h"

        Diamonds ->
            "d"


parser : Parser.Parser Suit
parser =
    Parser.oneOf
        [ Parser.symbol "c" |> Parser.map (always Clubs)
        , Parser.symbol "C" |> Parser.map (always Clubs)
        , Parser.symbol "s" |> Parser.map (always Spades)
        , Parser.symbol "S" |> Parser.map (always Spades)
        , Parser.symbol "h" |> Parser.map (always Hearts)
        , Parser.symbol "H" |> Parser.map (always Hearts)
        , Parser.symbol "d" |> Parser.map (always Diamonds)
        , Parser.symbol "D" |> Parser.map (always Diamonds)
        ]


order : Suit -> Suit -> Order
order lhs rhs =
    if lhs |> gt rhs then
        GT

    else if lhs |> lt rhs then
        LT

    else
        EQ


type alias Selection =
    { pairs : List ( Suit, Suit )
    , suited : List Suit
    , offsuit : List ( Suit, Suit )
    }


toggleSuitedSelection : Suit -> Selection -> Selection
toggleSuitedSelection suit selection =
    { selection
        | suited =
            if selection.suited |> List.member suit then
                selection.suited |> List.filter ((/=) suit)

            else
                suit :: selection.suited
    }


toggleOffSuitSelection : Suit -> Suit -> Selection -> Selection
toggleOffSuitSelection suit1 suit2 selection =
    { selection
        | offsuit =
            if selection.offsuit |> List.member ( suit1, suit2 ) then
                selection.offsuit |> List.filter ((/=) ( suit1, suit2 ))

            else
                ( suit1, suit2 ) :: selection.offsuit
    }


togglePairsSelection : Suit -> Suit -> Selection -> Selection
togglePairsSelection suit1 suit2 selection =
    { selection
        | pairs =
            if selection.pairs |> List.member ( suit1, suit2 ) then
                selection.pairs |> List.filter ((/=) ( suit1, suit2 ))

            else
                ( suit1, suit2 ) :: selection.pairs
    }


initialSelection : Selection
initialSelection =
    { pairs = []
    , suited = []
    , offsuit = []
    }


isSelectionEmpty : Selection -> Bool
isSelectionEmpty selection =
    List.isEmpty selection.pairs && List.isEmpty selection.suited && List.isEmpty selection.offsuit
