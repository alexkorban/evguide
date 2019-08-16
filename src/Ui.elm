module Ui exposing (..)

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


heading1 : List (Attribute msg) -> List (Element msg) -> Element msg
heading1 attrs =
    paragraph ([ Font.size <| round <| fontScale 4, Font.bold, Font.color darkBlue, headingTypeface, Region.heading 1 ] ++ attrs)


heading2 : List (Attribute msg) -> List (Element msg) -> Element msg
heading2 attrs =
    paragraph ([ Font.size <| round <| fontScale 3, Font.bold, Font.color darkBlue, headingTypeface, Region.heading 2 ] ++ attrs)


heading3 : List (Attribute msg) -> List (Element msg) -> Element msg
heading3 attrs =
    paragraph ([ Font.size <| round <| fontScale 2, Font.bold, Font.color darkBlue, headingTypeface, Region.heading 3 ] ++ attrs)


heading4 : List (Attribute msg) -> List (Element msg) -> Element msg
heading4 attrs =
    paragraph ([ Font.size <| round <| fontScale 1, Font.bold, Font.color darkBlue, headingTypeface, Region.heading 4 ] ++ attrs)


logo : List (Attribute msg) -> Element msg -> Element msg
logo attrs =
    el ([ Font.size 24, Font.color lightCharcoal, logoTypeface ] ++ attrs)


menuIcon : List (Attribute msg) -> Element msg
menuIcon attrs =
    textColumn ([ width <| px 30, alignRight, spacing -17, logoTypeface, Font.size 20, Font.color green ] ++ attrs)
        [ paragraph [] [ text "—" ]
        , paragraph [] [ text "—" ]
        , paragraph [] [ text "—" ]
        ]


listIcon : List (Attribute msg) -> Element msg
listIcon attrs =
    el ([ Font.size 16, logoTypeface ] ++ attrs) <| text "•"
