module ResultFormatter exposing (markdown, pokerStrategy, twoPlusTwo)

import Maybe.Extra
import Model exposing (ResultLine, SimulationResult)
import Poker.Board as Board
import Poker.Card exposing (Card)
import Poker.HandOrCombo as Range
import Poker.Position as Position exposing (Position(..))
import Round
import Url exposing (Url)


lines : SimulationResult -> List ( Position, ResultLine )
lines simulationResult =
    [ simulationResult.utg |> Maybe.map (Tuple.pair UTG)
    , simulationResult.mp |> Maybe.map (Tuple.pair MP)
    , simulationResult.co |> Maybe.map (Tuple.pair CO)
    , simulationResult.bu |> Maybe.map (Tuple.pair BU)
    , simulationResult.sb |> Maybe.map (Tuple.pair SB)
    , simulationResult.bb |> Maybe.map (Tuple.pair BB)
    ]
        |> Maybe.Extra.values


markdownResultLine : ( Position, ResultLine ) -> String
markdownResultLine ( position, resultLine ) =
    """| {{POSITION}} | {{RANGE}} | {{EQUITY}}% |"""
        |> String.replace "{{POSITION}}" (position |> Position.toString)
        |> String.replace "{{RANGE}}" (resultLine.range |> Range.toNormalizedString)
        |> String.replace "{{EQUITY}}" ((resultLine.equity * 100) |> Round.round 2)


markdown : Url -> SimulationResult -> String
markdown url result =
    """**Board**: {{BOARD}}

| Position | Hand Range | Equity |
| --- | --- | --- |
{{RESULT_LINES}}

Powered by [Equiweb - 6 Max Hold'em Equity Simulations]({{URL}})"""
        |> String.replace "{{BOARD}}" (result.board |> Board.toString)
        |> String.replace "{{RESULT_LINES}}" (result |> lines |> List.map markdownResultLine |> String.join "\n")
        |> String.replace "{{URL}}" (url |> Url.toString)


altBoard : List Card -> String
altBoard board =
    Board.toString board
        |> String.toUpper
        |> String.replace "S" ":spade:"
        |> String.replace "C" ":club:"
        |> String.replace "H" ":heart:"
        |> String.replace "D" ":diamond:"
        |> String.replace "PREFLOP" "(preflop)"


twoPlusTwoLine : ( Position, ResultLine ) -> String
twoPlusTwoLine ( position, resultLine ) =
    """{{POSITION}} |{{RANGE}} |{{EQUITY}}%"""
        |> String.replace "{{POSITION}}" (position |> Position.toString)
        |> String.replace "{{RANGE}}" (resultLine.range |> Range.toNormalizedString)
        |> String.replace "{{EQUITY}}" ((resultLine.equity * 100) |> Round.round 2)


twoPlusTwo : Url -> SimulationResult -> String
twoPlusTwo url result =
    """[table=head]Board
{{BOARD}}
[/table]
[table=head]Position |Hand Range |Equity
{{RESULT_LINES}}
[/table]
Powered by [URL="{{URL}}"]Equiweb - 6 Max Hold'em Equity Simulations[/URL]
"""
        |> String.replace "{{BOARD}}" (result.board |> altBoard)
        |> String.replace "{{RESULT_LINES}}" (result |> lines |> List.map twoPlusTwoLine |> String.join "\n")
        |> String.replace "{{URL}}" (url |> Url.toString)


pokerStrategyLine : ( Position, ResultLine ) -> String
pokerStrategyLine ( position, resultLine ) =
    """[b]{{POSITION}}[/b]{{EQUITY}}%\u{00A0}{ {{RANGE}} }"""
        |> String.replace "{{POSITION}}" (position |> Position.toString |> String.padRight 3 '\u{00A0}')
        |> String.replace "{{RANGE}}" (resultLine.range |> Range.toNormalizedString)
        |> String.replace "{{EQUITY}}" ((resultLine.equity * 100) |> Round.round 2 |> String.padLeft 7 '\u{00A0}')


pokerStrategy : Url -> SimulationResult -> String
pokerStrategy url result =
    """[quote][FONT=courier new][SIZE=14]
[b]Board: [/b]{{BOARD}}
[b]Pos\u{00A0}\u{00A0}Equity\u{00A0}\u{00A0}Range[/b]
{{RESULT_LINES}}
Powered by [url="{{URL}}"]Equiweb - 6 Max Hold'em Equity Simulations[/url]
[/SIZE][/FONT][/quote]"""
        |> String.replace "{{BOARD}}" (altBoard result.board |> String.replace " " "\u{00A0}")
        |> String.replace "{{RESULT_LINES}}" (result |> lines |> List.map pokerStrategyLine |> String.join "\n")
        |> String.replace "{{URL}}" (url |> Url.toString)
