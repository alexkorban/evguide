module Util exposing (asMoneyStr, dasherise, equalBy, flip, undasherise)

import FormatNumber exposing (format)
import String.Extra as String


asMoneyStr : Int -> String
asMoneyStr n =
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
