module Sharing exposing (markdown)

import Maybe.Extra
import Poker.Card as Card
import Poker.Position as Position exposing (Position(..))
import Poker.Range as Range
import Round
import SimulationResult exposing (ResultLine, SimulationResult)
import Url exposing (Url)



-- 2+2
-- [table=head]Board
-- K:heart:A:spade:T:club:
-- [/table]
-- [table=head]Position |Hand Range |Equity
-- UTG |22+,A2s+,K8s+,Q9s+,J9s+,T9s,98s,87s,76s,65s,54s,ATo+,KJo+ |44.49%
-- SB |99+,ATs+,A5s,A4s,KTs+,QTs+,JTs,T9s,98s,87s,76s,AQo+ |55.51%
-- [/table]
-- Powered by [URL="https://equiweb.surge.sh/?utg=22%2B%2Ca2s%2B%2Ck8s%2B%2Cq9s%2B%2Cj9s%2B%2Ct9s%2C98s%2C87s%2C76s%2C65s%2C54s%2Cato%2B%2Ckjo%2B&sb=99%2B%2Cats%2B%2Ca5s%2Ca4s%2Ckts%2B%2Cqts%2B%2Cjts%2Ct9s%2C98s%2C87s%2C76s%2Caqo%2B"]Equiweb - 6 Max Hold'em Equity Simulations[/URL]
--
-- MD
-- **Board**: KhQdTd
--
-- | Position | Hand Range | Equity |
-- | --- | --- | --- |
-- | UTG | 22+,A2s+,K8s+,Q9s+,J9s+,T9s,98s,87s,76s,65s,54s,ATo+,KJo+ | 44.49% |
-- | SB | 99+,ATs+,A5s,A4s,KTs+,QTs+,JTs,T9s,98s,87s,76s,AQo+ | 55.51% |
--
-- Powered by [Equiweb - 6 Max Hold'em Equity Simulations](https://equiweb.surge.sh/?utg=22%2B%2Ca2s%2B%2Ck8s%2B%2Cq9s%2B%2Cj9s%2B%2Ct9s%2C98s%2C87s%2C76s%2C65s%2C54s%2Cato%2B%2Ckjo%2B&sb=99%2B%2Cats%2B%2Ca5s%2Ca4s%2Ckts%2B%2Cqts%2B%2Cjts%2Ct9s%2C98s%2C87s%2C76s%2Caqo%2B)
--
-- ASCII
-- Board: KhQdTd
-- +----------+-----------------------------------------------------------+--------+
-- | Position | Hand Range                                                | Equity |
-- |----------|-----------------------------------------------------------|--------|
-- | UTG      | 22+,A2s+,K8s+,Q9s+,J9s+,T9s,98s,87s,76s,65s,54s,ATo+,KJo+ | 44.49% |
-- +----------+-----------------------------------------------------------+--------+
-- | SB       | 99+,ATs+,A5s,A4s,KTs+,QTs+,JTs,T9s,98s,87s,76s,AQo+       | 55.51% |
-- +----------+-----------------------------------------------------------+--------+
-- Powered by Equiweb - 6 Max Hold'em Equity Simulations
-- https://equiweb.surge.sh/?utg=22%2B%2Ca2s%2B%2Ck8s%2B%2Cq9s%2B%2Cj9s%2B%2Ct9s%2C98s%2C87s%2C76s%2C65s%2C54s%2Cato%2B%2Ckjo%2B&sb=99%2B%2Cats%2B%2Ca5s%2Ca4s%2Ckts%2B%2Cqts%2B%2Cjts%2Ct9s%2C98s%2C87s%2C76s%2Caqo%2B
--
-- PokerStrategy
-- [FONT=courier new][SIZE=12]
-- [b]Board: [/b]K:heart:Q:heart:2:diamond: A:diamond:
-- [b]Pos Equity Hand Range[/b]
-- [b]UTG[/b] 44.49% { 22+,A2s+,K2s+,Q2s+,J2s+,T2s+,92s+,82s+,72s+,62s+,52s+,42s+,32s,A2o+,K2o+,Q2o+,J2o+,T2o+,92o+,82o+,72o+,62o+,52o+,42o+,32o }
-- [b]SB [/b] 55.51% { 99+,ATs+,A5s,A4s,KTs+,QTs+,JTs,T9s,98s,87s,76s,AQo+ }
-- Powered by [url="https://equiweb.surge.sh/?utg=22%2B%2Ca2s%2B%2Ck8s%2B%2Cq9s%2B%2Cj9s%2B%2Ct9s%2C98s%2C87s%2C76s%2C65s%2C54s%2Cato%2B%2Ckjo%2B&sb=99%2B%2Cats%2B%2Ca5s%2Ca4s%2Ckts%2B%2Cqts%2B%2Cjts%2Ct9s%2C98s%2C87s%2C76s%2Caqo%2B"]Equiweb - 6 Max Hold'em Equity Simulations[/url]
-- [/SIZE][/FONT]


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
        |> String.replace "{{BOARD}}" (result.board |> Card.boardToString)
        |> String.replace "{{RESULT_LINES}}" (result |> lines |> List.map markdownResultLine |> String.join "\n")
        |> String.replace "{{URL}}" (url |> Url.toString)
