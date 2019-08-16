module Util exposing (..)

import FormatNumber exposing (format)
import String.Extra as String


intAsMoneyStr : Int -> String
intAsMoneyStr n =
    "$"
        ++ (format
                { decimals = 0
                , thousandSeparator = ","
                , decimalSeparator = "."
                , negativePrefix = "−"
                , negativeSuffix = ""
                , positivePrefix = ""
                , positiveSuffix = ""
                }
            <|
                toFloat n
           )


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


undasherise : String -> String
undasherise =
    String.toTitleCase << String.replace "-" " "
