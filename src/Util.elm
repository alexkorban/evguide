module Util exposing (..)

import FormatNumber exposing (format)
import Http
import String.Extra as String


floatAsStr : Int -> Float -> String
floatAsStr decimals float =
    format
        { decimals = decimals
        , thousandSeparator = ","
        , decimalSeparator = "."
        , negativePrefix = "−"
        , negativeSuffix = ""
        , positivePrefix = ""
        , positiveSuffix = ""
        }
        float


intAsMoneyStr : Int -> String
intAsMoneyStr n =
    "$" ++ (floatAsStr 0 <| toFloat n)


intListAsString : List Int -> String
intListAsString list =
    String.join ", " <| List.map String.fromInt list


dasherise : String -> String
dasherise =
    String.dasherize << String.toLower


equalBy : (a -> b) -> a -> a -> Bool
equalBy f a b =
    f a == f b


flip : (b -> a -> c) -> a -> b -> c
flip f a b =
    f b a


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"


undasherise : String -> String
undasherise =
    String.toTitleCase << String.replace "-" " "
