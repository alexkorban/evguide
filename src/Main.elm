module Main exposing (update, vehicleCard, view)

import Browser exposing (UrlRequest(..))
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Color exposing (..)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import ElmMarkup
import Html exposing (Html)
import List.Extra as List
import Types exposing (..)
import Ui
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))
import Util exposing (..)
import Vehicle exposing (..)


{-| / or /vehicles ==> vehicle list
-- /vehicles/nissan/leaf ==> particular model
-- /why-ev or /ev-running-costs or /about ==> information pages
-}
type GuideRoute
    = IndexPageRoute
    | PageRoute String
    | VehiclePageRoute VehicleMake VehicleModel Year


{-| Message names follow this format: SomeoneDidSomethingSomewhereAndSomehow
-}
type Msg
    = RuntimeChangedUrl Url
    | UserClickedLink UrlRequest
    | UserClickedMenuIcon
    | UserClickedOutsideMenuPanel
    | UserResizedWindow Int Int


type alias Model =
    { isMenuPanelOpen : Bool
    , navKey : Nav.Key
    , pageText : Dict String (Element Msg)
    , route : GuideRoute
    , vehicleText : Dict String (Element Msg)
    , windowHeight : Int
    , windowWidth : Int
    }


type alias Flags =
    { windowHeight : Int, windowWidth : Int }


routeParser : UrlParser.Parser (GuideRoute -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map VehiclePageRoute <|
            UrlParser.s "vehicles"
                </> UrlParser.string
                </> UrlParser.string
                </> UrlParser.int
        , UrlParser.map PageRoute <| UrlParser.string
        , UrlParser.map IndexPageRoute <| UrlParser.oneOf [ UrlParser.s "vehicles", UrlParser.top ]
        ]


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { isMenuPanelOpen = False
      , navKey = navKey
      , pageText = Result.withDefault Dict.empty <| ElmMarkup.markupToEls pageText
      , route = Maybe.withDefault IndexPageRoute <| UrlParser.parse routeParser url
      , vehicleText = Result.withDefault Dict.empty <| ElmMarkup.markupToEls vehicleText
      , windowHeight = flags.windowHeight
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

        imageUrl =
            "/img/" ++ String.fromInt (rangeFrom vehicle.years) ++ "-" ++ dasherise makeModel ++ "-small.jpg"

        specSummary =
            [ ( "Range", rangeAsString vehicle.range )
            , ( "Price", pricesAsString vehicle.price )
            , ( "Years", yearsAsString vehicle.years )
            , ( "", Maybe.withDefault " " vehicle.comment )
            ]
                |> List.map (\( label, value ) -> row [ width fill, Ui.smallFont ] [ el [] <| text label, el [ alignRight ] <| text value ])
                |> List.intersperse (el [ width fill, height <| px 1, Background.color lightGrey ] none)
                |> column [ width fill, spacing 5 ]
    in
    link []
        { url = "/vehicles/" ++ id vehicle
        , label =
            column
                [ width <| px vehicleCardWidth
                , padding 10
                , spacingXY 0 10
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color green
                , Border.shadow { offset = ( 1, 1 ), blur = 3, color = grey, size = 0.3 }
                ]
                [ Ui.heading2 [] [ text makeModel ]
                , el [ padding 1, Border.rounded 2, Border.width 1, Border.color Color.grey ] <|
                    image [ width <| px 300 ] { src = imageUrl, description = makeModel }
                , specSummary
                ]
        }


vehicleDetails : Model -> Vehicle -> List (Element Msg)
vehicleDetails model vehicle =
    let
        makeModel =
            vehicle.make ++ " " ++ vehicle.model

        imageUrl =
            "/img/" ++ String.fromInt (rangeFrom vehicle.years) ++ "-" ++ dasherise makeModel ++ ".jpg"

        vehicleImage =
            column [ width fill, paddingEach { top = 10, bottom = 10, left = 0, right = 0 } ] <|
                [ el [ centerX, padding 1, Border.rounded 2, Border.width 1, Border.color Color.grey ] <|
                    image [ width <| maximum 1000 fill ] { src = imageUrl, description = makeModel }
                ]

        heading =
            Ui.heading1 [ paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ] [ text (vehicle.make ++ " " ++ vehicle.model) ]

        description =
            paragraph [ width <| fillPortion 3 ]
                [ Maybe.withDefault (text "") <| Dict.get (id vehicle) model.vehicleText ]

        spec =
            [ ( "Range", rangeAsString vehicle.range )
            , ( "Price", pricesAsString vehicle.price )
            , ( "Years", yearsAsString vehicle.years )
            , ( "Batteries, kWh", String.join ", " <| List.map String.fromInt vehicle.batteries )
            ]
                |> List.map (\( label, value ) -> row [ width fill, Ui.smallFont ] [ el [] <| text label, el [ alignRight ] <| text value ])
                |> List.intersperse (el [ width fill, height <| px 1, Background.color lightGrey ] none)
                |> flip column
    in
    if model.windowWidth > 700 then
        [ vehicleImage
        , row [ width <| maximum 800 fill, centerX, spacing 10 ]
            [ column
                [ width fill
                , alignTop
                , paddingEach { left = 5, right = 5, top = 0, bottom = 0 }
                ]
                [ heading, description ]
            , spec
                [ width <| px 280
                , spacing 10
                , paddingEach { top = 20, bottom = 20, right = 15, left = 15 }
                , alignTop
                , Border.widthEach
                    { left = 4
                    , right =
                        if model.windowWidth > 820 then
                            4

                        else
                            0
                    , top = 0
                    , bottom = 0
                    }
                , Border.color paleBlue
                , Background.color offWhite
                ]
            ]
        ]

    else
        -- Narrow viewport
        [ vehicleImage
        , heading
        , spec
            [ width <| maximum 300 fill
            , centerX
            , spacing 5
            , paddingEach { bottom = 30, top = 0, left = 0, right = 0 }
            ]
        , description
        ]


navigationLinks : List { url : String, label : Element Msg }
navigationLinks =
    [ { url = "/why-ev", label = text "Why EV" }
    , { url = "/running-costs", label = text "Running costs" }
    ]


navigationMenuLinks : List { url : String, label : Element Msg }
navigationMenuLinks =
    { url = "/", label = text "Vehicles" } :: navigationLinks


navigation : Model -> Element Msg
navigation model =
    let
        menu =
            if model.windowWidth < 2 * vehicleCardWidth + 2 * mainContentMargin then
                [ Ui.menuIcon [ onClick UserClickedMenuIcon ] ]

            else
                List.map (\navLink -> link [ alignRight, Font.size 14, Font.color green ] navLink) navigationLinks
    in
    column [ width fill ]
        [ row [ width fill, height <| px 60, padding 10, Region.navigation, spacing 20 ] <|
            link []
                { url = "/"
                , label =
                    paragraph []
                        [ Ui.logo [ Font.color lightBlue ] <| text "EV"
                        , Ui.logo [ Font.color blue ] <| text "Guide"
                        ]
                }
                :: menu
        , if allCommentsShort then
            none

          else
            Ui.heading2 [ Font.color orange ] [ text "Vehicle comment overflow" ]
        , el [ width fill, height <| px 1, Background.color lightBlue ] none
        , el [ width fill, height <| px 10 ] none
        ]


navigationMenu : Element Msg
navigationMenu =
    row [ width fill, height fill ]
        [ el
            [ width fill
            , height fill
            , onClick UserClickedOutsideMenuPanel
            , Background.color <| rgba255 11 79 108 0.4
            ]
            none
        , column
            [ width <| px 200
            , height fill
            , padding 15
            , spacing 15
            , Background.color offWhite
            , Border.widthEach { left = 4, right = 0, top = 0, bottom = 0 }
            , Border.color paleBlue
            , Border.shadow { offset = ( 0, 0 ), blur = 10, color = blue, size = 0.5 }
            ]
          <|
            List.map (\navLink -> link [ alignRight, Font.size 18, Font.bold, Font.color green ] navLink) navigationMenuLinks
        ]


footer : Model -> Element Msg
footer model =
    let
        footerLinks =
            navigationMenuLinks ++ [ { url = "/contact", label = text "Contact" } ]

        isNarrow =
            model.windowWidth < vehicleCardWidth * 2 + mainContentMargin * 2
    in
    column
        [ Region.footer
        , width fill
        , paddingEach { top = 30, bottom = 0, left = 0, right = 0 }
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
        , (if isNarrow then
            column

           else
            row
          )
            [ width fill
            , paddingEach { top = 20, bottom = 20, left = 10, right = 10 }
            , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            , Border.color lightBlue
            , Background.color blue
            ]
            [ paragraph [ alignTop ]
                [ Ui.logo [ Font.color lightBlue ] <| text "EV"
                , Ui.logo [ Font.color offWhite ] <| text "Guide"
                ]
            , column
                [ width <| minimum 200 <| fillPortion 1
                , alignTop
                , spacing 15
                , if isNarrow then
                    paddingXY 0 30

                  else
                    padding 0
                , if isNarrow then
                    Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }

                  else
                    Border.width 0
                , Border.color offWhite
                ]
              <|
                List.map
                    (\navLink -> link [ Font.color paleBlue, Font.size 13 ] navLink)
                    footerLinks
            , column
                [ if model.windowWidth < vehicleCardWidth * 3 + mainContentMargin * 2 then
                    width <| px 0

                  else
                    width <| fillPortion 1
                , if isNarrow then
                    paddingXY 0 30

                  else
                    padding 0
                ]
                []
            , textColumn [ width <| fillPortion 2, spacing 15, Font.color white, Font.size 13 ]
                [ paragraph []
                    [ text "This site is based on the "
                    , link [ Font.color paleBlue ] { url = "https://www.electricheaven.nz", label = text "Electric car guide" }
                    , text " by Sigurd Magnusson."
                    ]
                , paragraph []
                    [ text "Vehicle images are indicative only. Left-hand drive vehicles may be shown." ]
                , paragraph []
                    [ text "Prices, years and specifications can be approximate due to shifting market and manufacturer changes."
                    ]
                , paragraph []
                    [ text "A project of "
                    , link [ Font.color paleBlue ] { url = "https://korban.net", label = text "korban.net" }
                    , text "."
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        content =
            case model.route of
                IndexPageRoute ->
                    [ column
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
                            |> List.map (row [ spacing 8 ])
                        )
                    ]

                VehiclePageRoute vehicleMake vehicleModel vehicleYear ->
                    let
                        maybeVehicle =
                            Vehicle.find (undasherise vehicleMake) (undasherise vehicleModel) vehicleYear
                    in
                    case maybeVehicle of
                        Just vehicle ->
                            vehicleDetails model vehicle

                        Nothing ->
                            [ Ui.heading1 [] [ text "Unknown vehicle" ] ]

                PageRoute pageId ->
                    [ el [ width <| maximum 800 fill, centerX, paddingXY 0 20 ] <|
                        Maybe.withDefault (Ui.heading1 [] [ text ("Unknown page " ++ pageId) ]) <|
                            Dict.get pageId model.pageText
                    ]
    in
    layout [ Ui.mainTypeface, Font.size 16 ] <|
        column
            [ width fill
            , if model.isMenuPanelOpen then
                inFront navigationMenu

              else
                padding 0
            ]
        <|
            [ navigation model, column [ width fill, paddingEach { left = 5, right = 5, top = 0, bottom = 0 } ] content, footer model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuntimeChangedUrl url ->
            ( { model | route = Maybe.withDefault IndexPageRoute <| UrlParser.parse routeParser url }, Cmd.none )

        UserClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( { model | isMenuPanelOpen = False }, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        UserClickedMenuIcon ->
            ( { model | isMenuPanelOpen = True }, Cmd.none )

        UserClickedOutsideMenuPanel ->
            ( { model | isMenuPanelOpen = False }, Cmd.none )

        UserResizedWindow width height ->
            ( { model | windowHeight = height, windowWidth = width, isMenuPanelOpen = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize UserResizedWindow


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = \model -> { title = "EV Guide New Zealand", body = [ view model ] }
        , subscriptions = subscriptions
        , onUrlRequest = UserClickedLink
        , onUrlChange = RuntimeChangedUrl
        }


pageText : String
pageText =
    """
|> Page 
    id = why-ev
    text = 
        |> H1 
            Why EV?

        The benefits of an EV are...


|> Page
    id = running-costs
    text =
        |> H1
            Running costs 
        
        The running costs are...
"""
