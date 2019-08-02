module Ui exposing (heading2, headingTypeface, logo, mainTypeface, smallFont)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region


headingTypeface : Attribute msg
headingTypeface =
    Font.family [ Font.typeface "BioRhyme", Font.typeface "Helvetica", Font.sansSerif ]


logoTypeface : Attribute msg
logoTypeface =
    Font.family [ Font.typeface "Bungee Shade", Font.typeface "Georgia", Font.monospace ]


mainTypeface : Attribute msg
mainTypeface =
    Font.family [ Font.typeface "Nunito", Font.typeface "Helvetica", Font.sansSerif ]


fontScale =
    modular 18 1.25


smallFont : Attribute msg
smallFont =
    Font.size <| round <| fontScale -1


heading2 : Element msg -> Element msg
heading2 =
    el [ Font.size <| round <| fontScale 3, Font.bold, Font.color darkBlue, headingTypeface, Region.heading 2 ]


logo : List (Attribute msg) -> Element msg -> Element msg
logo attrs =
    el ([ Font.size 24, Font.color lightCharcoal, logoTypeface ] ++ attrs)
