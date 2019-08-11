module ElmMarkup exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Mark exposing (Block, Document)
import Mark.Error
import Ui


para =
    paragraph []


markupToEls : String -> Result String (Dict String (Element msg))
markupToEls markup =
    case Mark.compile document markup of
        Mark.Success dict ->
            Ok dict

        Mark.Almost { result, errors } ->
            -- This is the case where there has been an error,
            -- but it has been caught by `Mark.onError` and is still rendereable.
            Err <| String.join "\n" <| List.map Mark.Error.toString errors

        Mark.Failure errors ->
            Err <| String.join "\n" <| List.map Mark.Error.toString errors


document : Document (Dict String (Element msg))
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
                    link [] { url = url, label = para <| List.map (\( styles, str ) -> styledText styles str) texts }
                )
                |> Mark.field "url" Mark.string
            , Mark.verbatim "name"
                (\str ->
                    el [] <| text str
                )
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


pageBlock : Block ( String, Element msg )
pageBlock =
    Mark.record "Page"
        (\id textEls -> ( id, textColumn [ spacing 10, width <| minimum 300 fill ] textEls ))
        |> Mark.field "id" Mark.string
        |> Mark.field "text"
            (Mark.manyOf
                [ heading1
                , heading2
                , list
                , Mark.map para inlineMarkup
                ]
            )
        |> Mark.toBlock



{- Handle Blocks -}


heading1 =
    Mark.block "H1" (\children -> Ui.heading1 [] children) inlineMarkup


heading2 =
    Mark.block "H2" (\children -> Ui.heading2 [] children) inlineMarkup



-- image =
--     Mark.record "Image"
--         (\src description ->
--             Html.img
--                 [ Attr.src src
--                 , Attr.alt description
--                 ]
--                 []
--         )
--         |> Mark.field "src" Mark.string
--         |> Mark.field "description" Mark.string
--         |> Mark.toBlock
-- code =
--     Mark.record "Code"
--         (\lang str ->
--             Html.pre [] [ Html.code [ Attr.class lang ] [ Html.text str ] ]
--         )
--         |> Mark.field "lang" Mark.string
--         |> Mark.field "code" Mark.string
--         |> Mark.toBlock
{- Handling bulleted and numbered lists - taken from elm-markup examples -}


list : Mark.Block (Element msg)
list =
    Mark.tree "List" renderList (Mark.map para inlineMarkup)



{- Note: we have to define this as a separate function because
   -- `Items` and `Node` are a pair of mutually recursive data structures.
   -- It's easiest to render them using two separate functions:
   -- renderList and renderItem
-}


renderList : Mark.Enumerated (Element msg) -> Element msg
renderList (Mark.Enumerated enum) =
    let
        group =
            case enum.icon of
                Mark.Bullet ->
                    column

                Mark.Number ->
                    column
    in
    group []
        (List.map renderItem enum.items)


renderItem : Mark.Item (Element msg) -> Element msg
renderItem (Mark.Item item) =
    column []
        [ para item.content
        , renderList item.children
        ]
