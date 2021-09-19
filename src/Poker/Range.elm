module Poker.Range exposing (rewrite)

import Parser
import Poker.Combo as Combo
import Poker.Hand as Hand
import Result.Extra


rewrite : String -> Result (List String) String
rewrite rangeString =
    rangeString
        |> String.split ","
        |> List.map (String.replace " " "")
        |> List.map
            (Parser.run
                (Parser.oneOf
                    [ Parser.backtrackable (Hand.parser |> Parser.map (List.map Hand.toString))
                    , Parser.backtrackable Combo.parser |> Parser.map (Combo.toString >> List.singleton)
                    , Parser.end |> Parser.map (always [])
                    ]
                )
            )
        |> Result.Extra.combine
        |> Result.map (List.concat >> List.intersperse "," >> String.concat)
        |> Result.Extra.mapBoth (always [ "Range is not valid" ]) identity
