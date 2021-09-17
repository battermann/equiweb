module Form exposing (..)


type alias Error =
    String


type alias Validated a =
    Result (List Error) a


type alias Field a =
    { name : String
    , value : String
    , validated : Validated a
    }


setValue : (String -> Validated a) -> String -> Field a -> Field a
setValue f value field =
    { field | value = value, validated = f value }


apply : Validated a -> Validated (a -> b) -> Validated b
apply result fResult =
    case ( fResult, result ) of
        ( Ok f, Ok a ) ->
            Ok <| f a

        ( Err errs, Ok _ ) ->
            Err errs

        ( Ok _, Err errs ) ->
            Err errs

        ( Err errs1, Err errs2 ) ->
            Err (errs1 ++ errs2)
