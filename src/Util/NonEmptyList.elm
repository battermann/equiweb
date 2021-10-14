module Util.NonEmptyList exposing (toList)


toList : ( a, List a ) -> List a
toList ( x, xs ) =
    x :: xs
