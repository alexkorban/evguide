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
    | InfoPageRoute String
    | VehiclePageRoute String


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
    , markupErrors : List String
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
      , vehicleText = Result.withDefault Dict.empty vehicleTextRes
      , windowHeight = flags.windowHeight
      , windowWidth = flags.windowWidth
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

        description =
            paragraph [ width <| fillPortion 3 ]
                [ Maybe.withDefault (text "") <| Dict.get (id vehicle) model.vehicleText ]

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


{-| These are shown in a row on larger screens
-}
navigationLinks : List { url : String, label : Element Msg }
navigationLinks =
    [ { url = "/why-ev", label = text "Why EV" }
    , { url = "/running-costs", label = text "Running costs" }
    , { url = "/charging", label = text "Charging" }
    , { url = "/batteries", label = text "Batteries" }
    , { url = "/nz-policies", label = text "NZ policies" }
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
            if model.windowWidth < 2 * vehicleCardWidth + 2 * mainContentMargin then
                [ Ui.menuIcon [ onClick UserClickedMenuIcon ] ]

            else
                List.map (\navLink -> link [ alignRight, Font.size 15, Font.color green ] navLink) navigationLinks
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
                :: pageLinks
        , el [ width fill, height <| px 1, Background.color lightBlue ] none
        , el [ width fill, height <| px 10 ] none
        ]


navigationMenuPanel : Element Msg
navigationMenuPanel =
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
            navigationMenuLinks
                ++ [ { url = "/contact", label = text "Contact" }
                   , { url = "/licences", label = text "Licenses" }
                   ]

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
            [ paragraph [ alignTop ]
                [ Ui.logo [ Font.color lightBlue ] <| text "EV"
                , Ui.logo [ Font.color offWhite ] <| text "Guide"
                ]
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
                    (if model.windowWidth < vehicleCardWidth * 3 + mainContentMargin * 2 then
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


view : Model -> Html Msg
view model =
    let
        indexPageContent =
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

        content =
            case model.route of
                IndexPageRoute ->
                    indexPageContent

                InfoPageRoute pageId ->
                    case Dict.get pageId model.pageText of
                        Just pageText ->
                            [ el [ width <| maximum 800 fill, centerX, paddingXY 0 20 ] pageText ]

                        Nothing ->
                            indexPageContent

                VehiclePageRoute vehicleId ->
                    case Vehicle.find vehicleId of
                        Just vehicle ->
                            vehicleDetails model vehicle

                        Nothing ->
                            [ Ui.heading1 [] [ text "Unknown vehicle" ] ]
    in
    layout [ Ui.mainTypeface, Font.size 16 ] <|
        column
            [ width fill
            , if model.isMenuPanelOpen then
                inFront navigationMenuPanel

              else
                -- dummy attr
                padding 0
            ]
        <|
            [ navigation model
            , if allCommentsShort then
                none

              else
                Ui.heading2 [ Font.color orange ] [ text "Vehicle comment overflow" ]
            , if not <| List.isEmpty model.markupErrors then
                textColumn [ Font.color orange ] <| List.map text model.markupErrors

              else
                none
            , column [ width fill, paddingEach { left = 5, right = 5, top = 0, bottom = 0 } ] content
            , footer model
            ]


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


pageTextMarkup : String
pageTextMarkup =
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
            Slow (AC)

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
            Fast (DC)

        |> H4 
            CHAdeMO (Japan / US)

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
            - Tesla Model S/X (with adapter from Tesla)

        |> H4 
            Tesla Supercharger (Japan/US)    

        |> Image 
            url = /img/tesla-hpwc-plug.svg
            description = Tesla Supercharger
            width = 300
            border = False
            align = Left 

        |> List 
            - This socket isn’t found on Tesla cars in NZ 
            - In NZ, Tesla Model S/X use CHAdeMO (with special cable supplied by Tesla) and modified Type 2 (not CCS) for both AC and DC
            - Model 3 uses Type2 CCS
            - NOT supported by NZ charging stations

        |> H3 
            Combo (slow AC and fast DC)

        |> H4 
            Type 1 CCS (Japan / US)

        |> Image 
            url = /img/type1-ccs-plug.svg
            description = Type 1 CCS
            width = 300
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
            width = 300
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
