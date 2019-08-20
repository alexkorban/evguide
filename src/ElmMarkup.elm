module ElmMarkup exposing (..)

import Color exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as Attr
import Mark exposing (Block, Document)
import Mark.Error
import Types exposing (..)
import Ui


type Align
    = Left
    | Right
    | Center


para =
    paragraph [ spacing 15 ]


markupToEls : String -> Result (List String) (PageDict msg)
markupToEls markup =
    case Mark.compile document markup of
        Mark.Success dict ->
            Ok dict

        Mark.Almost { result, errors } ->
            -- This is the case where there has been an error,
            -- but it has been caught by `Mark.onError` and is still rendereable.
            Err <| List.map Mark.Error.toString errors

        Mark.Failure errors ->
            Err <| List.map Mark.Error.toString errors


document : Document (PageDict msg)
document =
    Mark.document Dict.fromList <| Mark.manyOf [ pageBlock ]



{- Handle Text -}


inlineMarkup =
    Mark.textWith
        { view =
            \styles string ->
                styledText styles string
        , replacements = Mark.commonReplacements
        , inlines =
            [ Mark.annotation "link"
                (\texts url ->
                    link [ Font.color green ] { url = url, label = text <| String.join " " <| List.map (\( styles, str ) -> str) texts }
                )
                |> Mark.field "url" Mark.string
            , Mark.verbatim "name" text
            ]
        }


styledText styles string =
    if styles.bold || styles.italic || styles.strike then
        el
            [ if styles.bold then
                Font.bold

              else
                alpha 1.0
            , if styles.italic then
                Font.italic

              else
                alpha 1.0
            , if styles.strike then
                Font.strike

              else
                alpha 1.0
            ]
        <|
            text string

    else
        text string


pageBlock : Block ( String, Size -> Element msg )
pageBlock =
    Mark.record "Page"
        (\id textElFuncs ->
            ( id
            , \windowSize ->
                textColumn [ spacing 20, width <| minimum 300 fill ] <|
                    List.map ((|>) windowSize) textElFuncs
            )
        )
        |> Mark.field "id" Mark.string
        |> Mark.field "text"
            (Mark.manyOf
                [ heading1
                , heading2
                , heading3
                , heading4
                , iframe
                , image
                , list
                , Mark.map (\els -> always <| para els) inlineMarkup
                ]
            )
        |> Mark.toBlock



{- Handle Blocks -}


heading1 : Block (Size -> Element msg)
heading1 =
    Mark.block "H1" (\children -> \_ -> Ui.heading1 [] children) inlineMarkup


heading2 : Block (Size -> Element msg)
heading2 =
    Mark.block "H2" (\children -> \_ -> Ui.heading2 [] children) inlineMarkup


heading3 : Block (Size -> Element msg)
heading3 =
    Mark.block "H3" (\children -> \_ -> Ui.heading3 [] children) inlineMarkup


heading4 : Block (Size -> Element msg)
heading4 =
    Mark.block "H4" (\children -> \_ -> Ui.heading4 [] children) inlineMarkup


iframe : Block (Size -> Element msg)
iframe =
    Mark.record "Iframe"
        (\url heightPx ->
            \windowSize ->
                Element.html <|
                    Html.iframe
                        [ Attr.src url
                        , Attr.height <| min heightPx windowSize.height
                        , Attr.style "margin" "20px 0 20px 0"
                        , Attr.style "width" "100%"
                        , Attr.style "box-sizing" "border-box"
                        ]
                        []
        )
        |> Mark.field "url" Mark.string
        |> Mark.field "height" Mark.int
        |> Mark.toBlock


strToAlign : String -> Result Mark.Error.Custom Align
strToAlign s =
    case s of
        "Left" ->
            Ok Left

        "Right" ->
            Ok Right

        "Center" ->
            Ok Center

        "Centre" ->
            Ok Center

        _ ->
            Err
                { title = "Bad align value in |> Image"
                , message = [ "The align attribute has invalid value " ++ s ]
                }


image : Block (Size -> Element msg)
image =
    Mark.record "Image"
        (\url description widthPx align border ->
            \windowSize ->
                let
                    imageIsWide =
                        windowSize.width - widthPx < 300

                    alignment =
                        case align of
                            Left ->
                                if imageIsWide then
                                    centerX

                                else
                                    alignLeft

                            Right ->
                                if imageIsWide then
                                    centerX

                                else
                                    alignRight

                            Center ->
                                centerX

                    imageParams =
                        { src = url, description = description }

                    imageEl alignAttr =
                        case border of
                            False ->
                                Element.image [ width <| maximum widthPx fill, alignAttr ] imageParams

                            True ->
                                el [ width <| maximum (widthPx + 2) fill, alignAttr ] <|
                                    el
                                        [ width <| maximum (widthPx + 2) fill
                                        , alignTop
                                        , padding 1
                                        , Border.rounded 2
                                        , Border.width 1
                                        , Border.color Color.grey
                                        ]
                                    <|
                                        Element.image [ width fill ] imageParams
                in
                if alignment == centerX then
                    el [ width fill ] <| imageEl centerX

                else
                    imageEl alignment
        )
        |> Mark.field "url" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.field "width" Mark.int
        |> Mark.field "align" (Mark.verify strToAlign Mark.string)
        |> Mark.field "border" Mark.bool
        |> Mark.toBlock


{-| Handling bulleted and numbered lists - taken from elm-markup examples
-}
list : Block (Size -> Element msg)
list =
    Mark.tree "List" renderList <| Mark.map para inlineMarkup


{-| Note: we have to define this as a separate function because
-- `Items` and `Node` are a pair of mutually recursive data structures.
-- It's easiest to render them using two separate functions:
-- renderList and renderItem
-}
renderList : Mark.Enumerated (Element msg) -> Size -> Element msg
renderList (Mark.Enumerated enum) windowSize =
    let
        group =
            case enum.icon of
                Mark.Bullet ->
                    column

                Mark.Number ->
                    column
    in
    group [ spacing 15, paddingXY 20 0 ]
        (List.map (renderItem windowSize) enum.items)


renderItem : Size -> Mark.Item (Element msg) -> Element msg
renderItem windowSize (Mark.Item item) =
    row [ width fill, spacingXY 10 0 ]
        [ Ui.listIcon [ alignTop ]
        , column [ width fill ]
            [ para item.content
            , renderList item.children windowSize
            ]
        ]
