module Ui exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html.Attributes as Attr


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


footerLink : List (Attribute msg) -> { url : String, label : Element msg } -> Element msg
footerLink attrs params =
    link ([ Font.color paleBlue ] ++ attrs) params


headingAttrs =
    [ Font.bold, Font.color darkBlue, headingTypeface, htmlAttribute <| Attr.style "clear" "both" ]


heading1 : List (Attribute msg) -> List (Element msg) -> Element msg
heading1 attrs =
    paragraph ([ Font.size <| round <| fontScale 4, Region.heading 1 ] ++ headingAttrs ++ attrs)


heading2 : List (Attribute msg) -> List (Element msg) -> Element msg
heading2 attrs =
    paragraph ([ Font.size <| round <| fontScale 3, Region.heading 2 ] ++ headingAttrs ++ attrs)


heading3 : List (Attribute msg) -> List (Element msg) -> Element msg
heading3 attrs =
    paragraph ([ Font.size <| round <| fontScale 2, Region.heading 3 ] ++ headingAttrs ++ attrs)


heading4 : List (Attribute msg) -> List (Element msg) -> Element msg
heading4 attrs =
    paragraph ([ Font.size <| round <| fontScale 1, Region.heading 4 ] ++ headingAttrs ++ attrs)


link : List (Attribute msg) -> { url : String, label : Element msg } -> Element msg
link attrs params =
    Element.link ([ Font.color green ] ++ attrs) params


logo : Color -> List (Attribute msg) -> Element msg
logo color attrs =
    row ([ Font.size 24, logoTypeface ] ++ attrs)
        [ el [ Font.color lightBlue ] <| text "EV"
        , el [ Font.color color ] <| text "Guide"
        ]


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
