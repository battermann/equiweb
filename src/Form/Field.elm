module Form.Field exposing (Field, Validated, apply, clear, rewrite, setEdited, setValue)


type alias Error =
    String


type alias Validated a =
    Result (List Error) a


type alias Field a =
    { name : String
    , value : String
    , validated : Validated a
    , edited : Bool
    }


rewrite : Field a -> (a -> String) -> Field a
rewrite field toString =
    case field.validated of
        Ok v ->
            { field | value = toString v }

        _ ->
            field


setValue : (String -> Validated a) -> String -> Field a -> Field a
setValue f value field =
    { field | value = value, validated = f value, edited = String.isEmpty value |> not }


setEdited : Field a -> Field a
setEdited field =
    { field | edited = True }


clear : a -> Field a -> Field a
clear default field =
    { field | edited = False, value = "", validated = Ok default }


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
