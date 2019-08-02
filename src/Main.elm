module Main exposing (update, vehicleCard, view)

import Browser
import Browser.Events exposing (onResize)
import Color exposing (..)
import Cons exposing (Cons)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FormatNumber exposing (format)
import Html exposing (Html)
import List.Extra as List
import Ui
import Vehicle exposing (..)



-- Message names follow this format: SomeoneDidSomethingSomewhereAndSomehow


type Msg
    = UserResizedWindow Int Int


type alias Model =
    { windowHeight : Int
    , windowWidth : Int
    }


type alias Flags =
    { windowHeight : Int, windowWidth : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { windowHeight = flags.windowHeight
      , windowWidth = flags.windowWidth
      }
    , Cmd.none
    )


{-| Vehicle cards are of fixed width because we can fit at least one on any screen,
so there's no point mucking around with making them responsive
-}
vehicleCardWidth : Int
vehicleCardWidth =
    324


mainContentMargin : Int
mainContentMargin =
    20


vehicleCard : Vehicle -> Element msg
vehicleCard vehicle =
    let
        makeModel =
            vehicle.make ++ " " ++ vehicle.model

        asMoney n =
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

        rangeAsString range =
            case range of
                Single value ->
                    String.fromInt value ++ " km"

                Range from to ->
                    String.fromInt from ++ "-" ++ String.fromInt to ++ " km"

        pricesAsString prices =
            case prices of
                Single price ->
                    asMoney price

                Range from to ->
                    asMoney from ++ "-" ++ asMoney to

        yearsAsString years =
            case years of
                Single year ->
                    String.fromInt year ++ "-now"

                Range from to ->
                    String.fromInt from ++ "-" ++ String.fromInt to

        specSummary =
            [ ( "Range", rangeAsString vehicle.range )
            , ( "Price", pricesAsString vehicle.price )
            , ( "Years", yearsAsString vehicle.years )
            , ( "", Maybe.withDefault "" vehicle.comment )
            ]
                |> List.map (\( label, value ) -> row [ width fill, Ui.smallFont ] [ el [] <| text label, el [ alignRight ] <| text value ])
                |> List.intersperse (el [ width fill, height <| px 1, Background.color lightGrey ] none)
                |> column [ width fill, spacing 5 ]
    in
    column
        [ width <| px vehicleCardWidth
        , padding 10
        , spacingXY 0 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color green
        , Border.shadow { offset = ( 2, 2 ), blur = 0.1, color = black, size = 2 }
        ]
        [ Ui.heading2 <| text makeModel
        , el [ padding 1, Border.rounded 2, Border.width 1, Border.color Color.grey ] <| image [ width <| px 300 ] { src = vehicle.imageUrl, description = makeModel }
        , specSummary
        ]


view : Model -> Html msg
view model =
    let
        navigation =
            row [ padding 10, Region.navigation ]
                [ paragraph []
                    [ Ui.logo [ Font.color lightBlue ] <| text "EV"
                    , Ui.logo [ Font.color blue ] <| text "Guide"
                    ]
                ]

        footer =
            column
                [ Region.footer
                , width fill
                , paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                ]
                [ el
                    [ width fill
                    , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                    , Border.color lightBlue
                    , height <| px 3
                    , Background.color offWhite
                    ]
                    none
                , el
                    [ width fill
                    , height <| px 3
                    , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                    , Border.color blue
                    , Background.color offWhite
                    , Border.dashed
                    ]
                    none
                , row
                    [ width fill
                    , padding 10
                    , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                    , Border.color lightBlue
                    , Background.color blue
                    ]
                    [ paragraph []
                        [ Ui.logo [ Font.color lightBlue ] <| text "EV"
                        , Ui.logo [ Font.color offWhite ] <| text "Guide"
                        ]
                    ]
                ]
    in
    layout [ Ui.mainTypeface ] <|
        column [ width fill ]
            [ navigation
            , el [ width fill, height <| px 1, Background.color lightBlue ] none
            , column
                [ Region.mainContent
                , width <| minimum (vehicleCardWidth + mainContentMargin * 2) shrink
                , centerX
                , spacingXY 0 10
                , paddingEach { left = 20, right = 20, top = 0, bottom = 0 }
                , Font.color offBlack
                ]
              <|
                (Vehicle.data
                    |> Cons.toList
                    |> List.map vehicleCard
                    |> List.greedyGroupsOf (max ((model.windowWidth - mainContentMargin * 2) // vehicleCardWidth) 1)
                    |> List.map (row [ spacing 5 ])
                )
            , footer
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserResizedWindow width height ->
            ( { model | windowHeight = height, windowWidth = width }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize UserResizedWindow


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "EV Guide New Zealand", body = [ view model ] }
        , subscriptions = subscriptions
        }
