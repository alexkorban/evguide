module Main exposing (update, vehicleCard, view)

import Browser exposing (UrlRequest(..))
import Browser.Dom
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
import Task
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
    | InfoPageRoute String
    | VehiclePageRoute String


{-| Message names follow this format: SomeoneDidSomethingSomewhereAndSomehow
-}
type Msg
    = RuntimeChangedUrl Url
    | RuntimeDidSomethingIrrelevant
    | UserClickedLink UrlRequest
    | UserClickedMenuIcon
    | UserClickedOutsideMenuPanel
    | UserChoseVehicleAvailabilityOption VehicleAvailability
    | UserChoseVehicleSortOrder VehicleSortOrder
    | UserResizedWindow Int Int


type VehicleAvailability
    = AvailableAny
    | AvailableNew
    | AvailableUsed


type VehicleSortOrder
    = NameSort
    | PriceSort
    | RangeSort


type alias Model =
    { isMenuPanelOpen : Bool
    , markupErrors : List String
    , navKey : Nav.Key
    , pageText : PageDict Msg
    , route : GuideRoute
    , vehicleAvailability : VehicleAvailability
    , vehicleSortOrder : VehicleSortOrder
    , vehicleText : PageDict Msg
    , windowSize : Size
    }


type alias Flags =
    { windowHeight : Int, windowWidth : Int }


routeParser : UrlParser.Parser (GuideRoute -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map (\make model year -> VehiclePageRoute <| make ++ "/" ++ model ++ "/" ++ String.fromInt year) <|
            UrlParser.s "vehicles"
                </> UrlParser.string
                </> UrlParser.string
                </> UrlParser.int
        , UrlParser.map IndexPageRoute <| UrlParser.oneOf [ UrlParser.s "vehicles", UrlParser.top ]
        , UrlParser.map InfoPageRoute <| UrlParser.string
        ]


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        pageTextRes =
            ElmMarkup.markupToEls pageTextMarkup

        vehicleTextRes =
            ElmMarkup.markupToEls vehicleTextMarkup

        markupErrors =
            case ( pageTextRes, vehicleTextRes ) of
                ( Ok _, Ok _ ) ->
                    []

                ( Ok _, Err messages ) ->
                    messages

                ( Err messages, Ok _ ) ->
                    messages

                ( Err messages1, Err messages2 ) ->
                    messages1 ++ messages2
    in
    ( { isMenuPanelOpen = False
      , markupErrors = markupErrors
      , navKey = navKey
      , pageText = Result.withDefault Dict.empty pageTextRes
      , route = Maybe.withDefault IndexPageRoute <| UrlParser.parse routeParser url
      , vehicleAvailability = AvailableAny
      , vehicleSortOrder = NameSort
      , vehicleText = Result.withDefault Dict.empty vehicleTextRes
      , windowSize = { height = flags.windowHeight, width = flags.windowWidth }
      }
    , Cmd.none
    )


{-| Vehicle cards are of fixed width because we can fit at least one on any reasonable screen,
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

        vehicleTextEl =
            Maybe.withDefault (always <| text "") <| Dict.get (id vehicle) model.vehicleText

        description =
            paragraph [ width <| fillPortion 3 ]
                [ vehicleTextEl model.windowSize ]

        spec =
            [ ( "Range", rangeAsString vehicle.range )
            , ( "Price", pricesAsString vehicle.price )
            , ( "Years", yearsAsString vehicle.years )
            , ( "Batteries", intListAsString vehicle.batteries ++ " kWh" )
            , ( "Seats", intListAsString vehicle.seats )
            , ( "No. in NZ", String.fromInt vehicle.count )
            ]
                |> List.map (\( label, value ) -> row [ width fill, Ui.smallFont ] [ el [] <| text label, el [ alignRight ] <| text value ])
                |> List.intersperse (el [ width fill, height <| px 1, Background.color lightGrey ] none)
                |> flip column
    in
    if model.windowSize.width > 700 then
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
                        if model.windowSize.width > 820 then
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


{-| These are shown in a row on larger screens
-}
navigationLinks : List { url : String, label : Element Msg }
navigationLinks =
    [ { url = "/why-ev", label = text "Why EV" }
    , { url = "/running-costs", label = text "Running costs" }
    , { url = "/charging", label = text "Charging" }
    , { url = "/batteries", label = text "Batteries" }
    , { url = "/buying", label = text "Buying" }
    , { url = "/nz-policies", label = text "NZ policies" }
    , { url = "/resources", label = text "Resources" }
    ]


{-| These are shown in a slideout panel on smaller screens
-}
navigationMenuLinks : List { url : String, label : Element Msg }
navigationMenuLinks =
    { url = "/", label = text "Vehicles" } :: navigationLinks


navigation : Model -> Element Msg
navigation model =
    let
        pageLinks =
            if model.windowSize.width < 2 * vehicleCardWidth + 8 * mainContentMargin then
                [ Ui.menuIcon [ onClick UserClickedMenuIcon ] ]

            else
                List.map (\navLink -> link [ alignRight, Font.size 15, Font.color green ] navLink) navigationLinks
    in
    column [ width fill ]
        [ row [ width fill, height <| px 60, padding 10, Region.navigation, spacing 20 ] <|
            link []
                { url = "/"
                , label = Ui.logo blue []
                }
                :: pageLinks
        , el [ width fill, height <| px 1, Background.color lightBlue ] none
        , el [ width fill, height <| px 10 ] none
        ]


navigationMenuPanel : Element Msg
navigationMenuPanel =
    let
        linkEls =
            navigationMenuLinks
                |> List.map (\navLink -> link [ alignRight, Font.size 18, Font.color green ] navLink)
                |> List.intersperse (el [ width fill, height <| px 1, Background.color lightGrey ] none)
    in
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
            (linkEls ++ [ Ui.logo paleBlue [ paddingXY 0 20, alignRight, Font.size 18 ] ])
        ]


footer : Model -> Element Msg
footer model =
    let
        footerLinks =
            navigationMenuLinks
                ++ [ { url = "/contact", label = text "Contact" }
                   , { url = "/licences", label = text "Licenses" }
                   ]

        isNarrow =
            model.windowSize.width < vehicleCardWidth * 2 + mainContentMargin * 2
    in
    column
        [ Region.footer
        , width fill
        , paddingEach { top = 30, bottom = 0, left = 0, right = 0 }
        ]
        [ el
            [ width fill
            , height <| px 3
            , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            , Border.color lightBlue
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
            [ Ui.logo offWhite [ alignTop, width <| px 200 ]
            , column
                [ width <| minimum 200 <| fillPortion 1
                , alignTop
                , spacing 15
                , paddingXY 0
                    (if isNarrow then
                        30

                     else
                        0
                    )
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
                [ width
                    (if model.windowSize.width < vehicleCardWidth * 3 + mainContentMargin * 2 then
                        px 0

                     else
                        fillPortion 1
                    )
                , paddingXY 0
                    (if isNarrow then
                        30

                     else
                        0
                    )
                ]
                []
            , textColumn [ width <| fillPortion 2, spacing 15, alignTop, Font.color white, Font.size 13 ]
                [ paragraph []
                    [ text "Some of the content is from the "
                    , link [ Font.color paleBlue ] { url = "https://www.electricheaven.nz", label = text "Electric car guide" }
                    , text " by Sigurd Magnusson, released under the Creative Commons Attributions licence."
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


indexPageContent : Model -> List (Element Msg)
indexPageContent model =
    let
        sortOptionButton label state =
            case state of
                Input.Idle ->
                    el [ Font.color blue ] <| text label

                Input.Focused ->
                    el [ Font.color lightBlue ] <| text label

                Input.Selected ->
                    el [ Font.color lightBlue, Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }, Border.color lightBlue ] <| text label

        sortOptions =
            el
                [ centerX
                , Background.color veryPaleBlue
                , paddingXY 20 7
                , Border.shadow { offset = ( -2, 2 ), blur = 2, color = paleBlue, size = 0.1 }
                ]
            <|
                Input.radioRow
                    [ padding 0
                    , spacing 10
                    ]
                    { onChange = UserChoseVehicleSortOrder
                    , selected = Just model.vehicleSortOrder
                    , label = Input.labelLeft [ centerY, paddingEach { right = 10, left = 0, top = 0, bottom = 0 } ] <| text "Sort by:"
                    , options =
                        [ Input.optionWith NameSort <| sortOptionButton "Name"
                        , Input.optionWith PriceSort <| sortOptionButton "Price"
                        , Input.optionWith RangeSort <| sortOptionButton "Range"
                        ]
                    }

        availabilityOptions =
            el
                [ centerX
                , Background.color veryPaleBlue
                , paddingXY 20 7
                , Border.shadow { offset = ( -2, 2 ), blur = 2, color = paleBlue, size = 0.1 }
                ]
            <|
                Input.radioRow
                    [ padding 0
                    , spacing 10
                    ]
                    { onChange = UserChoseVehicleAvailabilityOption
                    , selected = Just model.vehicleAvailability
                    , label = Input.labelLeft [ centerY, paddingEach { right = 10, left = 0, top = 0, bottom = 0 } ] <| text "Availability:"
                    , options =
                        [ Input.optionWith AvailableAny <| sortOptionButton "New or used"
                        , Input.optionWith AvailableUsed <| sortOptionButton "Used"
                        , Input.optionWith AvailableNew <| sortOptionButton "New"
                        ]
                    }

        controls =
            (if model.windowSize.width < 2 * vehicleCardWidth + 2 * mainContentMargin then
                column

             else
                row
            )
                [ width fill
                , paddingXY 20 10
                , spacing 20
                ]
                [ sortOptions, availabilityOptions ]

        vehicleCards =
            column
                [ Region.mainContent
                , width <| minimum (vehicleCardWidth + mainContentMargin * 2) shrink
                , centerX
                , spacingXY 0 10
                , paddingEach { left = 20, right = 20, top = 20, bottom = 0 }
                , Font.color offBlack
                ]
            <|
                (Vehicle.data
                    |> Cons.toList
                    |> List.map vehicleCard
                    |> List.greedyGroupsOf (max ((model.windowSize.width - mainContentMargin * 2) // vehicleCardWidth) 1)
                    |> List.map (row [ spacing 8 ])
                )
    in
    [ controls
    , vehicleCards
    ]


view : Model -> Html Msg
view model =
    let
        content =
            case model.route of
                IndexPageRoute ->
                    indexPageContent model

                InfoPageRoute pageId ->
                    case Dict.get pageId model.pageText of
                        Just pageText ->
                            [ el [ width <| maximum 800 fill, centerX, paddingXY 0 20 ] <| pageText model.windowSize ]

                        Nothing ->
                            indexPageContent model

                VehiclePageRoute vehicleId ->
                    case Vehicle.find vehicleId of
                        Just vehicle ->
                            vehicleDetails model vehicle

                        Nothing ->
                            [ Ui.heading1 [] [ text "Unknown vehicle" ] ]
    in
    layout [ Ui.mainTypeface, Font.size 16 ] <|
        column
            ([ [ width fill ]
             , if model.isMenuPanelOpen then
                [ inFront navigationMenuPanel ]

               else
                []
             ]
                |> List.concat
            )
        <|
            ([ [ navigation model ]
             , if allCommentsShort then
                []

               else
                [ Ui.heading2 [ Font.color orange ] [ text "Vehicle comment overflow" ] ]
             , if not <| List.isEmpty model.markupErrors then
                [ textColumn [ Font.color orange ] <| List.map text model.markupErrors ]

               else
                []
             , [ column [ width fill, paddingEach { left = 5, right = 5, top = 0, bottom = 0 } ] content ]
             , [ footer model ]
             ]
                |> List.concat
            )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> RuntimeDidSomethingIrrelevant) (Browser.Dom.setViewport 0 0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuntimeChangedUrl url ->
            ( { model | route = Maybe.withDefault IndexPageRoute <| UrlParser.parse routeParser url }, resetViewport )

        RuntimeDidSomethingIrrelevant ->
            ( model, Cmd.none )

        UserChoseVehicleAvailabilityOption option ->
            ( { model | vehicleAvailability = option }, Cmd.none )

        UserChoseVehicleSortOrder order ->
            ( { model | vehicleSortOrder = order }, Cmd.none )

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
            ( { model | windowSize = { height = height, width = width }, isMenuPanelOpen = False }, Cmd.none )


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


pageTextMarkup : String
pageTextMarkup =
    """
|> Page 
    id = why-ev
    text = 
        |> H1 
            Why EV?

        Electric vehicles (EVs) are cars that move using a large electric battery
        powering an electric motor. They do not use any petrol or diesel fuel. 

        Also called Battery Electric Vehicles (BEVs), they produce no exhaust, which is
        far kinder to the environment – petrol and diesel transport produce 18% of New
        Zealand’s greenhouse gases. 80% of New Zealand electricity is generated by rain
        (hydro dams), geothermal, and wind, so the source of the car’s fuel is
        environmentally friendly, and inexpensive, and produced locally. We import over
        a billion dollars of petrol and three billion dollars of crude oil from overseas
        each year and local electricity generation is cheaper. 

        A 2015 government study shows electric cars also have environmental benefits
        versus petrol cars when the full lifecycle of manufacture, use, and disposal are
        assessed, and that the ingredients like lithium in batteries, aren’t scarce. 

        Each year, an estimated 256 New Zealanders prematurely die from harmful diesel
        and other vehicle emissions (similar to the number who die in crashes) and this
        would reduce by driving electric vehicles. 

        Electric cars have no clutch or gears, and accelerate more quickly and smoothly,
        in a "sporty" way, and climb hills easier than petrol cars. A fully electric
        motor has fewer moving parts, no spark plugs or engine oil, and requires less
        maintenance than a petrol equivalent. 

        Such cars are extremely quiet and reduce noise pollution. Travelling down hills
        or braking recharges the batteries, and is known as regenerative braking. The
        motor uses no energy when the car is still. 

        Electric cars are safe, reliable, manufactured by large brands, and are
        beginning to be sold in high volume globally. Norway, with a similar population
        and size to New Zealand, is a global leader, with electric cars now outselling
        fuel-driven vehicles. Norway thus expects to end fuel car sales in 2025. 

        The dashboard displays how far you can drive with remaining battery charge.
        Entry-level electric cars have a shorter range (100km+) than petrol cars.
        High-end cars with large batteries (500km+ range) cost more. Battery prices are
        dropping significantly (80% drop from 2010 to 2016), making electric cars
        steadily cheaper. 

        On average New Zealand drivers travel 28km per day, and 95% of days within
        125km. Electric cars can be charged at home overnight and be "full" in the
        morning, so affordable electric cars are practical for most daily journeys. The
        census shows over half of New Zealand households have two or more cars,
        suggesting many could own a cheap electric car and keep a long distance fuel
        car. 

        Since 2016, electric car prices and models in NZ have improved. Electric cars
        here are mostly cheap, used, imported short-range 
        [Nissan Leaf]{ link | url = /vehicles/nissan/leaf/2011 } hatchbacks. 
        Increasing numbers of other makes and
        models are arriving, including large, long-range, high performance cars by
        Tesla, the global pioneer in electric cars. Most automakers are indicating
        timeframes by which all cars they manufacture will be partially or fully
        electric, eg Volvo in 2019, Jaguar 2020, Mercedes 2022, Toyota`/`Lexus 2025,
        and Porsche & VW 2030.

        |> H2 
            What about hybrids?

        There is a type of vehicles called /plug-in hybrid electric vehicles/ (PHEV).
        These have both an electric and petrol motor, but with the added feature that
        they can be plugged in at home or wherever there is an electrical socket. This
        lets you drive short distances electrically, at low cost and without pollution,
        and long distances using fossil fuel, avoiding the need to frequently recharge.

        These vehicles also have regenerative braking, which captures some energy that
        would be wasted as braking heat. 

        They cost somewhere in the middle between affordable (short range) and expensive
        (long range) fully electric cars. 

        The drawback of plug-in hybrids is a complicated engine requiring maintenance,
        petrol refueling costs, air pollution, and engine noise. 

        The fossil fuel engine will either help the electric motor turn the wheels
        (“parallel PHEV”) or only recharge the battery (“series PHEV”) but some can do
        both. Most have very small batteries that don’t drive far electrically. As
        battery prices drop, plug-in hybrids will be replaced by full battery electrics.

        |> H4 
            What we used to call hybrids no longer count

        Cars such as the /non-plug-in/ Toyota Prius Hybrid found in this country over
        the past decade are different–they can not be plugged into an electric socket to
        recharge. They can only fill up on petrol, and use the petrol engine and
        regenerative braking to recharge a small battery that gives a short (1-2 km)
        electric range. A plug-in vehicle has many more benefits.

        |> Image 
            url = /img/vehicle-types.png
            description = Vehicle types
            width = 800
            border = False 
            align = Center


        |> H2
            What about hydrogen?

        There has been an ongoing debate about whether the long-term future of car energy
        would be hydrogen fuel cells or stored electricity (i.e. batteries). While
        hydrogen vehicles can recharge quickly and drive long distances, the challenge
        is that hydrogen is made by splitting it out of natural gas (which releases
        greenhouse gases) or water (which requires vast amounts of electricity) and the
        hydrogen then needs to be pressurised, stored, and transported, even though the
        vehicle still has an electric motor. 

        Battery electric cars by contrast are safer (no explosive gas), simpler, use
        less energy, and it is a quarter of the cost to generate electricity, send it
        through the electrical grid, and recharge batteries. Hydrogen cars are not sold
        in New Zealand, and are very limited globally. A demonstration refueling project is to
        be trialed at Port of Auckland, and Hyundai have displayed a Nexo hydrogen SUV
        at a Fielddays expo in New Zealand.

|> Page
    id = running-costs
    text =
        |> H1
            Running costs 

        In one sentence: EVs are expensive upfront but cheaper overall.

        Electric cars are currently more expensive to buy new than fuel vehicles,
        largely due to high battery prices and low production volume. This is expected
        to change within 10 years, as battery prices drop, at which point it will be
        cheaper for car manufacturers to build electric cars than fuel cars.

        Travelling by electricity is cheaper than petrol: EECA calculates it is
        equivalent to 30 cents a litre, about 7 times cheaper than petrol. 

        Driven regularly, an electric car can save you a few thousand dollars a year,
        quickly paying off the higher car purchase price. Fewer moving parts means
        electric cars have less maintenance cost 
        ([see total cost of ownership calculator]{ link | url = https://eecabusiness.govt.nz/tools/vehicle-total-cost-of-ownership-tool/ }).

        The cost of electricity varies more than petrol. Recharging with electricity can
        be free (if your employer or a friendly business or council is paying instead of
        you!), low cost (overnight off-peak electricity rates are cheaper than daytime,
        if you select a good plan or provider), or higher cost (if you recharge during
        the day, or are paying to use a fast-charging station). 

        Assuming you commute 40km a day, you would probably need about 8 units of
        electricity (kWh) to recharge. At a low overnight rate of 11c per kWh this is
        $0.88 a day. 

        Overnight charging is good for the national electricity grid
        because it is at its lowest demand, meaning the power is likely generated with
        renewables, not coal and gas. If your car has a smart timer, set the "End charge
        time" to just before 7am, so your battery (and optionally cabin) isn’t cold to
        drive away in, your battery isn’t full for long, and so it randomises the charge
        start time (makes managing electricity demand easier for the power companies).

|> Page 
    id = charging 
    text = 
        |> H1 
            Charging

        We use kilowatt-hours (kWh) rather than litres to measure electricity, so you’re unlikely to talk 
        to EV drivers about dollars per litre, and instead hear them discuss:

        |> List 
            - cents per kWh, the cost of electricity; determines the cost of travelling and charging
            - km per kWh, similar to ‘miles per gallon’, or how far you’re driving for a unit of electricity
            - kWh as a size of battery, which gives you an idea of how far you can drive (range)
            - kW as a speed of charging, and, also, speed of draining your battery
                
                (A 30kWh battery should take around 10 hours to recharge with a 3kW charger. 
                Driving at 15kW will drain a 30kWh battery in two hours.)            

        Depending on driving style and car, you can usually expect to travel around 5 to 7 km per kWh.
        A detailed Norwegian study showed almost all cars are charged daily or weekly at home, 
        and that most cars use a public fast charger once a month or less, which is likely consistent 
        with New Zealand.

        The regular 230 volt AC electricity in our homes, and the regular socket we
        use for all household appliances is all you need to recharge your car, though
        dedicated equipment is faster and safer. The electrical safety regulator,
        WorkSafe, has guidelines on its website about what is required and
        recommended for domestic and public electric vehicle charging equipment,
        sockets and wiring.

        |> H2 
            Socket types 

        |> H3
            Domestic 3-pin socket (S3112)

        |> Image 
            url = /img/domestic-socket.jpg
            description = Domestic 3-pin (S3112) socket 
            width = 200
            border = False
            align = Left


        |> List 
            - 8-10 amps, single phase AC 230V
            - 1.8-2.3kW
            - 10km+ per hour recharging
            - 100km takes approx. 10 hours

        This is what you find throughout New Zealand homes. For most people, it is
        sufficient to charge their cars overnight during low-cost off peak hours
        (11pm-7am). It is too slow to be very useful for daytime recharging, and won’t
        give you much more than a 100km top-up overnight. 

        This socket is probably what you already have inside your garage at home. If
        your car doesn’t come with a cable fitting this socket, you can purchase a
        portable 8 amp unit from [various sources]{ link | url = https://leadingthecharge.org.nz/where_to_buy_charging_equipment }.

        /Note: Read WorkSafe guidelines for restrictions about this socket outside of a
        domestic environment, and restrictions from using the 15 amp variant of this
        socket (which can get too hot)./

        |> H3 
            Blue Commando socket (IEC 60309)

        |> Image 
            url = /img/blue-commando-socket.jpg
            description = Blue Commando socket (IEC 60309)
            width = 200
            border = False
            align = Left 

        |> List 
            - 16 amps, single phase AC 230V
            - 3.7kW
            - 18km+ per hour recharging
            - 100km takes 5 hours

        These are the plugs found in campgrounds all over the country, used by
        campervans. Having a connector for this socket lets you recharge in many
        locations around the country, and allows a higher current, faster charge. 

        You can get an electrician to fit this socket at home. The thick metal pins are
        well suited to repeated, prolonged use and rugged outdoor conditions, and won’t
        heat up as easily, reducing fire risk. Unless a car is parked for many hours,
        this is rather slow for daytime recharging, but it is a very low cost solution. 

        /Note: Read WorkSafe guidelines for restrictions about installing this socket
        outside of a domestic environment./

        |> H3
            Dedicated "slow" (AC) charging station

        |> Image 
            url = /img/ac-charging-station.jpg
            description = Dedicated "slow" (AC) charging station
            width = 200
            border = False
            align = Left 

        |> List 
            - 15-40 amps single phase AC 230V
            - 3-9kW
            - 18-45km per hour recharging 100km takes 2-5 hours
            - OR: 
                -- 32 amps, three phase AC 415V
                -- 22kW
                -- 110km per hour recharging 100km takes just under an hour

        For around $800 or more, you can buy a dedicated wall-mounted charging
        station. They are safer, more robust, and charge faster compared to regular
        wall sockets, so are the ideal option for homes, businesses, and public
        locations. 

        WorkSafe guidelines indicate standards you should look for in a
        product. Some take payment, have timers, are smartphone controllable, or work
        well with solar. The unit will either come with an attached cable, or just a
        socket. A unit with just a socket is compatible with all car types and thus is
        the approach recommended by NZTA for public stations. Units with attached cables
        are limited to specific cars (okay for home or fleets). Either way the
        connectors are specific to electric cars, deterring others from using them. 

        Cars limit the maximum pace of AC charging, eg older Nissan Leafs only charge
        up to 3.6 kW, and the newest BMW i3 charge up to 11 kW. So while a high power 22
        kW charger will connect, it will charge only as fast as the car supports. On the
        other hand, a Renault Zoe and Tesla cars can charge at high power levels, and
        drivers could feel impatient using a lower power (eg 7kW) charger. 

        These units (especially 3-phase 22kW) provide fast enough speeds to suit users
        parked at day-time destinations (eg workplaces, malls), without the high cost
        of fast DC chargers (below).


        |> H3
            Fast DC charger

        |> Image 
            url = /img/dc-charger.jpg
            description = Fast DC charger
            width = 200
            border = False
            align = Left 

        |> List 
            - 16-800 amps, 415-480V, 3 phase, inverted and supplied to car as DC
            - Medium: 25 kW (Common in NZ). Adding 100km takes up to 1 hour
            - Fast: 50 kW (Common in NZ). Adding 100km takes 25 minutes
            - Faster: 120 kW (Rare in NZ37). Adding 100km takes 10 minutes
            - Ultra Fast: 400 kW (no car yet supports charging this quickly; used in NZ today by electric buses). Adding 100km takes 3 minutes


        Fast chargers take much less time to recharge the battery compared to other
        options, and make long distance road trips practical. They work by providing a
        much greater amount of electricity and by changing it into direct current
        meaning it can be fed straight into the battery. Like petrol, you can choose
        just to "top up" your car and put in a few minutes’ worth of power. 

        This type of charging equipment comes in a large range of speeds and therefore
        costs (around $15,000 to over $100,000; a 50kW device is in the middle of this
        range). They are purchased by organisations and put in key locations where a
        high volume of car owners can drive to, such as town centers, supermarkets or
        petrol stations, or workplace fleet carparks. 

        They are overkill in locations where people intend to park for hours; a slower
        charger would be more appropriate there. 25 minutes typically adds 100km.
        However, this depends on how quickly the car can fast charge and whether the
        charger is delivering the full power that the car can manage. For example, an
        older Nissan Leaf charges much slower than what a typical fast charger offers
        (50kW), whereas Tesla and many new electric cars can charge much quicker than
        what a 50kW fast charger offers. 

        Cars usually can only be fast-charged to between 85-95% full,
        and the charging slows down significantly as the car completes charging.


        |> H2  
            Car connectors and inlets 

        Your car will normally come with a portable cable for a 3 pin socket, and
        might come with a cable to plug into a "Type 2" wall socket. Do
        *not* allow a car dealer to provide you with a cable for a
        Japanese shaped wall socket or 100V electricity; this is unsafe and not
        permitted.

        The connector`/`inlet on the car is designed specifically to be durable for
        continuous use and to be safe. There are multiple standards based on
        manufacturer, country, and charging speed. The following is based on typical
        configuration for cars in New Zealand.

        |> H3
            Slow charging connectors (AC)

        |> H4 
            Type 1 (“J1772”) (Japan & US)

        |> Image 
            url = /img/type1-j1772-plug.svg
            description = Type1 J1772
            width = 200
            border = False
            align = Left 

        |> List 
            - Audi A3 e-tron, Q7 (historically) 
            - BMW - bought in NZ (historically) 
            - Holden Volt
            - Kia Soul
            - Nissan Leaf and eNV200 
            - Mitsubishi iMiev and Outlander 
            - Toyota Prius Plugin (not Prime)

        |> H4 
            Type 2 ("Mennekes") (Europe)

        |> Image 
            url = /img/type2-mennekes-plug.svg
            description = Type2 "Mennekes"
            width = 200
            border = False
            align = Left 

        |> List 
            - Audi (from 2017)
            - BMW (from mid 2017)
            - Hyundai Ioniq, Kona
            - Jaguar i-Pace
            - Kia Niro (and Kia Soul 2019+)
            - Renault Zoe, Kangoo
            - Tesla (slow AC. Fast DC at Supercharger) Toyota Prius Prime
            - VW eGolf

        |> H3 
            Fast charging connectors (DC)

        |> H4 
            CHAdeMO (Japan and US)

        |> Image 
            url = /img/chademo-type4-plug.svg
            description = CHAdeMO
            width = 200
            border = False
            align = Left 

        |> List
            - BMW i3 imported from Japan
            - Kia Soul
            - Nissan Leaf and eNV200
            - Mitsubishi iMiev and some Outlander 
            - Tesla Model S and Model X (with adapter from Tesla)

        |> H4 
            Tesla Supercharger (Japan/US)    

        |> Image 
            url = /img/tesla-hpwc-plug.svg
            description = Tesla Supercharger
            width = 200
            border = False
            align = Left 

        |> List 
            - This socket isn’t found on Tesla cars in NZ 
            - In NZ, Tesla Model S and Model X use CHAdeMO (with special cable supplied by Tesla) and modified Type 2 (not CCS) for both AC and DC
            - Model 3 uses Type2 CCS
            - NOT supported by NZ charging stations

        |> H3 
            Combo (slow AC and fast DC)

        |> H4 
            Type 1 CCS (Japan and US)

        |> Image 
            url = /img/type1-ccs-plug.svg
            description = Type 1 CCS
            width = 200
            border = False
            align = Left 

        |> List 
            - BMW i3 bought in NZ (historically)
            - NOT supported by NZ charging stations

        |> H4 
            Type 2 CCS (Europe)

        |> Image 
            url = /img/combo-ccs-eu-plug.svg
            description = Type 2 CCS 
            width = 200
            border = False
            align = Left 

        |> List
            - Audi & BMW (mid 2017 onwards) 
            - Jaguar I-PACE
            - Kia Soul (from 2019), Kia Niro BEV 
            - Hyundai Ioniq and Kona
            - Tesla Model 3 
            - Volkswagen eGolf


        |> H2 
            Where to charge 

        Home is where the majority of charging takes place. Some New Zealand employers
        are providing workplace charging to staff. (This is popular in the USA where
        workplace charging is available to over 1 million workers; a charging station
        makes employees six times more likely to own an electric car.)

        A national network with over 100 public fast chargers and growing is being
        installed by [ChargeNet]{ link | url = https://charge.net.nz } in cities and
        every 50-100 km along major state highways. 

        The first stations were installed in 2015, assisted by BMW, Foodstuffs, EECA,
        Councils, and lines companies. An access fob, phone app, and website offered by
        ChargeNet allows drivers to pay for charging across both their network and
        many (but not all) stations installed by others.

        Here is the current map of ChargeNet chargers: 

        |> Iframe 
            url = https://www.google.com/maps/d/embed?mid=1iTUqNusVOcAfYb0Y4NWeKq0pnCg
            height = 500

        In addition to the above options: 

        |> List 
            - Tesla is also installing Superchargers for road trips, and slower chargers at destinations.
            - Some electricity companies are also installing charging stations (e.g. Vector in Auckland).
            - Hotels, motels and campgrounds offer charging. Many require a Blue Commando plug.
            - A number of tourism destinations and retailers are adding slow chargers for customers.
            - Wellington City Council is trialing street pole chargers for residents who only have on-street parks.


        You can see an extensive map of different charging options on 
        [plugshare.com]{ link | url = https://plugshare.com } (also available as a
        phone app):

        |> Iframe
            url = https://www.plugshare.com/widget2.html?latitude=-41.2784624&longitude=174.7790788&spanLat=7.22836&spanLng=7.22836&plugs=1,2,3,4,5,6,42,13,7,8,9,10,11,12,14,15,16,17
            height = 500 

        NZTA collates and publicly shares official charging locations through a programme named 
        EVRoam, visible on NZTA and the AA website.

        |> H2
            How far can you drive before recharging?

        Automakers and dealers advertise the distance cars can drive, however these can
        be exaggerated. A good information source is the [EPA Range]{ link | url = https://fueleconomy.gov}. 
        The US government test-drives cars in a consistent manner
        to determine how far the battery lasts on a typical journey mixing highway and
        suburban driving. A similar European “NEDC” and Worldwide “WLTP” electric car
        range test is less useful because they state long distances that can never be
        achieved with normal driving. 

        Several situations will result in a car using up
        its battery before reaching the EPA range: e.g. frequent acceleration, big hill
        climbs, high speeds, constant aircon or heating, headwinds, towing a trailer,
        and an old battery. Conversely, travelling slowly or staying on flat terrain can
        often let you drive further than the EPA figure. 

        When planning road trips, talk
        to other owners of your car model about how mountains, headwinds, and other
        factors drain your battery along your specific route, and how much battery you
        need to confidently reach destinations. The  
        [PowerTrip app]{ link | url = https://thepowertrip.co.nz } can give you a rough idea. 
        If you run out of charge, the car will slow down to crawl and eventually stop. 
        The AA can flat-bed tow you to a public charger so you can get back on your journey.

|> Page
    id = batteries
    text = 
        |> H1 
            Batteries

        Electric car batteries weigh several hundred kilograms and usually sit in the floor of the car. 
        This gives the cars a low centre of gravity, adding stability when cornering and accelerating.

        Battery size is measured in kilowatt-hours, or kWh. Lower-priced electric cars have approx. 24 kWh or 
        smaller batteries. High-end cars have up to 100 kWh; buses and trucks much more still. 
        This affects range and cost.

        |> Image
            url = /img/battery-stress.png 
            description = Battery stress illustration
            width = 400
            align = Left
            border = False

        The life of a battery is reduced when at extreme high or low levels of charge.
        
        To avoid battery charge reaching either end, not all of the battery capacity 
        is made available for use by the vehicle.

        You can lengthen the life of your battery by fully charging it only on occasion (hence the 
        "80% charge" option on most cars) and by avoiding the car being left too long at a high or low 
        level of charge (e.g. finishing your charge at 7am is ideal, but if it gets totally flat, 
        recharge a bit straight away). The battery will last longer if it is generally around a third 
        to half charged. 
        
        Hot temperatures (particularly over 30°C) reduce battery life; some cars 
        actively cool the battery to extend their lifetime. Excessive 
        fast charging (more than daily, for years) will reduce battery life slightly.

        |> H2 
            Capacity reduction over time

        Nissan expect battery capacity to reduce to 80% after 5 years and 70% at 10 years, 
        assuming 20,000km of annual driving in a Los Angeles climate (`~`20°C). 
        
        A survey of Tesla owners show longer battery lifespans: averaging 90% health after driving 300,000km, 
        likely owing to different battery chemistry, active battery cooling, and fewer charge cycles given 
        it has a much larger capacity battery. You can assess battery capacity on the dashboard or smartphone 
        app when you test drive a car. 
        
        While minor loss of capacity is typical in a used vehicle (eg 10-15%), you might be saving half 
        or a third of the cost of a new car, and the range will be still be higher than a typical daily drive. 

        Car batteries have warranties, but conditions vary. Only some dealers provide warranties with used 
        imports, although the [Consumer Guarantees Act]{ link | url = https://www.consumerprotection.govt.nz } 
        standard of "fit for purpose" applies to all sales to private individuals.            

        |> H2 
            End of life

            Eventually the battery will need replacement. It can then be recycled or
            reused, something that cannot be said for petrol or diesel after it has been
            used. Used batteries could be used by homeowners who want to store
            electricity from solar panels or overnight off-peak power. You may be able to
            buy a battery with more capacity than the car initially came with (in case of
            eg BMW, Renault). 
        
            You may need to replace only individual dead cells, at a lower price than a
            full replacement. [BlueCars.nz]{ link | url = https://bluecars.nz } can test
            batteries, fix weak cells, or replace Nissan Leaf car batteries
            (reconditioned $750-$5000), including swapping for larger battery sizes
            (>$15k).

|> Page 
    id = buying
    text = 
        |> H1
            Buying 

        Used and new car dealers throughout NZ sell and service electric cars. You will
        find hundreds of listings by choosing "Electric Cars" on
        [Trade Me Motors]{ link | url = https://trademe.nz/motors }
        or on [evsales.nz]{ link | url = https://evsales.nz }. 

        Used cars from Japan usually have console displays stuck in Japanese; this isn’t
        an issue with UK imports or cars sold new in NZ. The driver’s dashboard can be
        configured to display English by dealers. To change the central entertainment
        headunit to English dealers can sell a new (English) Nissan or third party
        system. This is sometimes included in the purchase price. 

        |> H2
            Go for a test drive! 

        The experience of test-driving an electric car gives people
        confidence to buy. You can test drive an electric car by asking a dealer, asking
        existing owners if they’re prepared to let you drive theirs, or rent from:

        |> List 
            - [Bluecars]{ link | url = https://bluecars.nz }
            - [Europcar]{ link | url = https://europcar.co.nz/electric-vehicles }
            - [Mevo]{ link | url = https://mevo.co.nz }
            - [Snap Rentals]{ link | url = https://snaprentals.co.nz }
            - [Yoogo Share]{ link | url = https://yoogoshare.co.nz }

|> Page 
    id = nz-policies
    text = 
        |> H1
            NZ policies and growth

        For New Zealand to reach the goal of being net zero carbon by 2050, 100% of cars
        entering NZ from 2030 would need to be electric (otherwise large numbers of fuel
        cars will need to be scrapped in the year 2050, because 20% of our cars are over
        20 years old). 

        This will require electric vehicle sales to jump astronomically; in 2019 less
        than 3% of cars entering NZ are electric. 

        |> Image
            url = /img/nz-ev-fleet-size.png 
            description = NZ EV fleet size 
            width = 400
            align = Left
            border = False

        Electric vehicle numbers are rising steadily, but so far only account for 15,500
        out of our 3.8 million light vehicles. 3.8 million electric vehicles would
        demand 17% more electricity, which can be met with renewable power stations that
        have consent to be built.

        In 2016 the government released an electric vehicle ‘package’ with a stated
        target (a doubling of electric vehicles every year to 64,000 by 2021, almost 2%
        of all vehicles, or 12% of car sales being electric), a $1M`/`year (for 5 years)
        nationwide education campaign, offering cash to co-fund projects that aid
        electric car adoption ($3M fund pool, open every 6 months), briefly trialled
        electric cars driving in special vehicle lanes, and efforts to support bulk car
        purchases and charging stations. 

        The annual "rego" fee for electric cars is `~`$75 per year 
        (see [electricvehicles.govt.nz]{link|url=https://electricvehicles.govt.nz}). 

        The current approach to reducing the cost of electric vehicles is set to change
        in 2021, assuming no change in government. Since 2009, electric vehicle owners
        have not paid road user charges (RUCs), saving an owner $720 per 10,000km
        compared to a small diesel car. From 2021 this is proposed to be replaced by a
        [Clean Car Rule and a Clean Car Discount]{ link | url = https://transport.govt.nz/clean-cars/}. 

        The Rule requires car importers to progressively sell cars with lower CO2
        emissions, at levels that trail Japanese rules by 10 years. The Discount will
        award buyers $8000 off new electric and $2600 off used electric imports. Hybrids
        and very efficient fuel cars will get small discounts. High emitting vehicles
        will face up to a $3000 purchase penalty. 

        These policies follow logic outlined in a 2015 report by Barry Barton at
        University of Waikato and a detailed follow up by the Productivity Commission in
        2018. These reports explained NZ was one of the last countries to introduce such
        policies, and how they are successful overseas.

        Heavy electric vehicles over 3 tons (eg buses and trucks) will instead continue
        to retain the RUC exemption through to 2025. Rising petrol prices and fuel taxes
        (up to 9-12c`/`litre within 3 years across New Zealand, plus 10c`/`litre in
        Auckland since 2018) will increase the savings available to electric car
        drivers. 

        Japan and UK are a cheap source for used electric imports as their governments
        also subsidise their purchase, and we don’t charge high import fees. Electric
        vehicle adoption is supported by an
        [industry]{link|url=https://DriveElectric.org.nz} and [owner association]{link|url=https://BetterNZ.org}. 
        Some large NZ firms have said they
        will make a third of their cars electric by 201929. Councils have few electric
        cars however EECA has released a local government guide for councils. Auckland’s
        mayor has pledged the streets for a part of the city will be "fossil fuel free"
        by 2030.

        |> H2
            Global leaders & government policy

        Many governments are forcing automakers to sell electric cars to hit climate
        change and air quality goals, and in response to diesel emissions cheating. All
        new cars sold are expected to be electric from 2025 in Norway, 2030 in
        Germany, Sweden, Netherlands and India, 2032 in Scotland, and 2040 in France and
        Britain. 

        Others have interim goals: 12% of sales in China by 2020; 22% of
        sales in California and New York by 2025; 20-30% of sales in Japan by 2030. Over
        200 European cities have low emission zones where fuel vehicles are barred entry
        or pay fees (e.g. Paris, London). The US has forced VW to spend $2B on charging
        stations across USA. China is working towards 5 million charging locations by
        2020.

        Norway has the most incentives globally, and has a similar population, land
        size, and vehicle count as NZ, but higher proportion of clean electricity.
        Norway charges a "pollution" tax on fuel vehicles (up to $40,000, based on
        emissions and weight) and a discount on electric vehicles (-$10,000). Electric vehicles don’t
        incur the 25% sales tax, enjoy halved fringe benefit tax, and free use of bus
        lanes, toll roads, urban street parking, and charging stations. Monthly electric
        car sales now outnumber fuel car sales; the country has over 300,000 electric
        vehicles (the highest per capita globally), and 10,000 charging points.

        EV fleet growth in Norway: 

        |> Image
            url = /img/norway-ev-fleet-size.png 
            description = Norway EV fleet size 
            width = 400
            align = Center 
            border = False

|> Page 
    id = resources 
    text = 
        |> H1 
            Resources 
                
        |> List 

            - [EVTalk]{ link | url = https://evtalk.co.nz },\u{200B} a NZ electric vehicle news website, email newsletter, and monthly print magazine. \u{200B} 
            - [NZ EV Podcast\u{200B}]{ link | url = https://podcasts.nz/nz-ev-podcast/ }, produced weekly.
            - [EVolocity]{ link | url = https://\u{200B}evolocity.co.nz }\u{200B}, nationwide annual high school competition to build and race electric vehicles.  
            - [EVWorld]{ link | url = \u{200B}https://www.evworld.nz }, public`/`industry conferences. 
            - [International Drive Electric Week]{ link | url = https://driveelectricweek.org }\u{200B}: multiple test drive events\u{200B} (September).
            - [Flip The Fleet]{ link | url = https://\u{200B}flipthefleet.org }: enter driving statistics and be a part of a national EV research project. 
            - [Leading the Charge]{ link | url = https://leadingthecharge.org.nz\u{200B} }\u{200B}, an annual 2500km electric car roadtrip the length of New Zealand, stopping in multiple towns for public display and rides\u{200B} (next: 2020).


        |> H2
            Facebook EV owner groups

        |> List 
            - [NZ EV Owners]{ link | url = https://facebook.com/groups/NZEVOwners/ \u{200B}} \u{200B}(lots of discussion)
            - Northland: [one group]{ link | url = https://facebook.com/groups/1472323112818001/ } and [another group]{ link | url = https://\u{200B}facebook.com/revupnz/ }
            - [Auckland]{ link | url = https://facebook.com/groups/291373964545996/ }
            - [Waikato]{ link | url = https://facebook.com/groups/WaikatoEV/ }
            - [Nelson]{ link | url = https://facebook.com/groups/365895557107117/ }
            - [Wellington]{ link | url = https://facebook.com/groups/WellyEV/ }
            - [Christchurch]{ link | url = https://facebook.com/groups/ChristchurchEVGroup/ }
            - [Dunedin]{ link | url = https://facebook.com/groups/403816650002889/ }


        |> H2 
            What about other types of vehicles?

        |> List 
            - Bicycles\u{200B}: commonly sold in local bicycle shops, with 40-100km "pedal assisted" range.
            - Motorbikes\u{200B}: [Ubco]{ link | url = \u{200B}https://ubcobikes.com\u{200B} } (Kiwi made); [Zero Motorcycles]{ link | url = https://\u{200B}zeromotorcycles.com\u{200B} }, \u{200B}Harley Davidson Livewire (2019).
            - Formula \u{200B}racing\u{200B} cars compete in [Formula E]{ link | url = https://\u{200B}fiaFormulaE.com }
            - An [electric supercar]{ link | url = https://www.nio.io/ep9\u{200B} } is the fastest around the gruelling \u{200B}Nurburgring circuit\u{200B}.
            - Over 400 one-seat "Paxster" ully electric delivery buggies are used by NZ Post.
            - 4WD Utes: \u{200B}Coming soon from \u{200B}[Great Wall]{ link | url = https://greatwall.co.nz\u{200B} }, [Rivian]{ link | url = https://\u{200B}rivian.com\u{200B} }, [Bollinger Motors]{ link | url = https://\u{200B}bollingermotors.com\u{200B} }, and [Tesla]{ link | url = https://tesla.com }.
            - Trucks\u{200B} are made by [Zero Emission Vehicles]{ link | url = https://\u{200B}zevnz.com }\u{200B} and Waste Management locally. 
            - Electric truck importers include [Etrucks]{ link | url = http://etrucks.co.nz }
                and \u{200B}[SEA Electric]{ link | url = https://sea-electric.com}\u{200B}. Tesla is releasing a [truck]{ link | url = https://tesla.com/semi \u{200B}} in 2019 that can carry 36 tons a
                distance of 800km and still recharge to 80% in 30 minutes.
            - Fully electric \u{200B}buses\u{200B} are mass produced, particularly in China, which has 350,000 on their roads.
                Wellington and Auckland have electric buses, and plan to go all electric in the years to come.
            - Fully electric \u{200B}motorhomes\u{200B} are now available for hire (eg \u{200B}[BritzEV]{ link | url = https://www.britzev.com\u{200B}}).
            - The world’s first electric \u{200B}ferry\u{200B} launched in 2015 in Norway (carries 300 people, 120 cars).
            - Electric \u{200B}airplanes\u{200B} are in commercial development. The \u{200B}/Solar Impulse 2/\u{200B} flew the globe in 2016;
                Norway aims to have all domestic air travel go electric before 2040.

|> Page 
    id = licences 
    text = 
        |> H1
            Content licences

        Some of the content and data on this site is taken or adapted from the
        [Electric car guide]{ link | url = https://www.electricheaven.nz } by Sigurd Magnusson, 
        licensed under [CC BY 3.0 NZ]{ link | url = https://creativecommons.org/licenses/by/3.0/nz/ }.

        LDV EV80 image is adapted from [Wikipedia image]{ link | url = https://en.wikipedia.org/wiki/LDV_Maxus#/media/File:Maxus_V80_China_2014-04-16.jpg }, 
        licensed under [CC BY-SA 4.0]{ link | url = https://creativecommons.org/licenses/by-sa/4.0/ }.

        Charging plug images are from [Wikipedia]{ link | url = https://commons.wikimedia.org/wiki/EV_Charger_Gallery },
        licensed under [CC BY-SA 4.0]{ link | url = https://creativecommons.org/licenses/by-sa/4.0/ }.
"""
