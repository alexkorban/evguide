module Vehicle exposing (..)

import Cons exposing (Cons, cons)
import List.Extra as List
import Types exposing (..)
import Util exposing (..)


type Availability
    = AvailableAny
    | AvailableNew


type SafetyOrg
    = ANCAP
    | EuroNCAP
    | NHTSA


type SortOrder
    = NameSort
    | PriceSort
    | RangeSort


type alias SafetyRating =
    { comment : Maybe String
    , org : SafetyOrg
    , stars : Int
    , url : String
    , year : Year
    }


type alias Vehicle =
    { id : VehicleId
    , batteries : List Int
    , comment : Maybe String
    , count : Int
    , make : String
    , model : String
    , price : SingleOrPair Int
    , range : SingleOrPair Int
    , safetyRating : SafetyRating
    , seats : List Int
    , years : SingleOrPair Int
    }


allCommentsShort : Bool
allCommentsShort =
    Cons.all (\v -> (String.length <| Maybe.withDefault "" v.comment) < 46) data


availableNew : Vehicle -> Bool
availableNew vehicle =
    case vehicle.years of
        Single _ ->
            False

        Range _ _ ->
            False

        RangeFrom _ ->
            True


filter : Availability -> Cons Vehicle -> List Vehicle
filter availability vehicles =
    let
        filterFunc =
            case availability of
                AvailableAny ->
                    always True

                AvailableNew ->
                    availableNew
    in
    Cons.filter filterFunc vehicles


find : VehicleId -> Maybe Vehicle
find vehicleId =
    data
        |> Cons.toList
        |> List.find (\v -> v.id == vehicleId)


id : Vehicle -> String
id vehicle =
    dasherise vehicle.make ++ "/" ++ dasherise vehicle.model ++ "/" ++ (String.fromInt <| rangeFrom vehicle.years)


rangeAsString : SingleOrPair Int -> String
rangeAsString range =
    case range of
        Single value ->
            String.fromInt value ++ " km"

        Range from to ->
            String.fromInt from ++ "-" ++ String.fromInt to ++ " km"

        RangeFrom value ->
            String.fromInt value ++ "+ km"


pricesAsString : SingleOrPair Int -> String
pricesAsString prices =
    case prices of
        Single price ->
            intAsMoneyStr price

        Range from to ->
            intAsMoneyStr from ++ "-" ++ intAsMoneyStr to

        RangeFrom from ->
            intAsMoneyStr from ++ "+"


safetyOrgAsString : SafetyOrg -> String
safetyOrgAsString org =
    case org of
        ANCAP ->
            "ANCAP"

        EuroNCAP ->
            "EuroNCAP"

        NHTSA ->
            "NHTSA"


safetyRatingAsString : SafetyRating -> String
safetyRatingAsString rating =
    String.fromInt rating.year ++ " " ++ String.repeat rating.stars "⭑"


sort : SortOrder -> List Vehicle -> List Vehicle
sort order vehicles =
    case order of
        NameSort ->
            List.sortBy .id vehicles

        PriceSort ->
            List.sortBy (.price >> rangeMidpoint) vehicles

        RangeSort ->
            List.sortBy (.range >> rangeMidpoint) vehicles


yearsAsString : SingleOrPair Year -> String
yearsAsString years =
    case years of
        Single year ->
            String.fromInt year

        Range from to ->
            String.fromInt from ++ "-" ++ String.fromInt to

        RangeFrom year ->
            String.fromInt year ++ "-now"


data : Cons Vehicle
data =
    cons
        { id = "audi/e-tron/2019"
        , batteries = [ 95 ]
        , comment = Nothing
        , count = 62
        , make = "Audi"
        , model = "e-tron"
        , price = Range 149000 180000
        , range = Single 328
        , safetyRating =
            { comment = Nothing
            , org = EuroNCAP
            , stars = 5
            , url = "https://www.euroncap.com/en/results/audi/e-tron/35869"
            , year = 2019
            }
        , seats = [ 5 ]
        , years = RangeFrom 2019
        }
        [ { id = "bmw/i3/2013"
          , batteries = [ 22, 33, 42 ]
          , comment = Just "246km range model coming in the near future"
          , count = 213
          , make = "BMW"
          , model = "i3"
          , price = Range 35000 85000
          , range = Range 130 246
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/bmw/i3/0ddbe4"
                , year = 2014
                }
          , seats = [ 4 ]
          , years = RangeFrom 2013
          }
        , { id = "hyundai/ioniq/2017"
          , batteries = [ 28, 38 ]
          , comment = Just "300km range model coming in the near future"
          , count = 546
          , make = "Hyundai"
          , model = "Ioniq"
          , price = Range 45000 60000
          , range = Range 200 300
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/hyundai/ioniq/9043d8"
                , year = 2016
                }
          , seats = [ 5 ]
          , years = RangeFrom 2017
          }
        , { id = "hyundai/kona/2019"
          , batteries = [ 64 ]
          , comment = Nothing
          , count = 373
          , make = "Hyundai"
          , model = "Kona"
          , price = Range 74000 78000
          , range = Single 415
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/hyundai/kona/61f948"
                , year = 2017
                }
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "jaguar/i-pace/2019"
          , batteries = [ 90 ]
          , comment = Nothing
          , count = 63
          , make = "Jaguar"
          , model = "I-PACE"
          , price = Range 160000 195000
          , range = Single 377
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/jaguar/i-pace/360c05"
                , year = 2018
                }
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "kia/niro-ev/2019"
          , batteries = [ 39, 64 ]
          , comment = Nothing
          , count = 58
          , make = "Kia"
          , model = "Niro EV"
          , price = Range 68000 77000
          , range = Range 244 384
          , safetyRating =
                { comment = Just "Only hybrid versions tested"
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/kia/niro/c84f30"
                , year = 2016
                }
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "kia/soul-ev/2015"
          , batteries = [ 27, 30 ]
          , comment = Nothing
          , count = 23
          , make = "Kia"
          , model = "Soul EV"
          , price = RangeFrom 35000
          , range = Range 150 182
          , safetyRating =
                { comment = Nothing
                , org = EuroNCAP
                , stars = 4
                , url = "https://www.euroncap.com/en/results/kia/soul-ev/7872"
                , year = 2014
                }
          , seats = [ 5 ]
          , years = RangeFrom 2015
          }
        , { id = "ldv/ev80/2019"
          , batteries = [ 56 ]
          , comment = Just "Range is manufacturer claim, not EPA"
          , count = 39
          , make = "LDV"
          , model = "EV80"
          , price = Single 50000
          , range = Single 180
          , safetyRating =
                { comment = Just "Only petrol versions tested"
                , org = ANCAP
                , stars = 3
                , url = "https://www.ancap.com.au/safety-ratings/ldv/v80/2b54a9"
                , year = 2013
                }
          , seats = [ 3 ]
          , years = RangeFrom 2019
          }
        , { id = "mitsubishi/i-miev/2009"
          , batteries = [ 11, 15, 16 ]
          , comment = Just "Also sold as Peugeot iOn & Citroën C-Zero"
          , count = 109
          , make = "Mitsubishi"
          , model = "i-MiEV"
          , price = RangeFrom 11000
          , range = Range 75 100
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 4
                , url = "https://www.ancap.com.au/safety-ratings/mitsubishi/i-miev/667569"
                , year = 2011
                }
          , seats = [ 4 ]
          , years = RangeFrom 2009
          }
        , { id = "nissan/e-nv200/2014"
          , batteries = [ 24, 40 ]
          , comment = Nothing
          , count = 369
          , make = "Nissan"
          , model = "e-NV200"
          , price = Range 28000 65000
          , range = Range 121 194
          , safetyRating =
                { comment = Nothing
                , org = EuroNCAP
                , stars = 3
                , url = "https://www.euroncap.com/en/results/nissan/e-nv200-evalia/7880"
                , year = 2014
                }
          , seats = [ 2, 5, 7 ]
          , years = RangeFrom 2014
          }
        , { id = "nissan/leaf/2011"
          , batteries = [ 24, 30 ]
          , comment = Nothing
          , count = 8453
          , make = "Nissan"
          , model = "Leaf"
          , price = Range 10000 30000
          , range = Range 117 172
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/nissan/leaf/278e6d"
                , year = 2011
                }
          , seats = [ 5 ]
          , years = Range 2011 2017
          }
        , { id = "nissan/leaf/2017"
          , batteries = [ 40, 62 ]
          , comment = Just "Longer range model coming in 2019"
          , count = 8160
          , make = "Nissan"
          , model = "Leaf"
          , price = Range 50000 59000
          , range = Range 243 363
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/nissan/leaf/c28310"
                , year = 2018
                }
          , seats = [ 5 ]
          , years = RangeFrom 2017
          }
        , { id = "renault/kangoo/2011"
          , batteries = [ 22, 33 ]
          , comment = Nothing
          , count = 56
          , make = "Renault"
          , model = "Kangoo"
          , price = Range 46000 75000
          , range = Range 120 190
          , safetyRating =
                { comment = Just "Only petrol versions tested"
                , org = ANCAP
                , stars = 4
                , url = "https://www.ancap.com.au/safety-ratings/renault/kangoo/5654ac"
                , year = 2011
                }
          , seats = [ 2, 5 ]
          , years = RangeFrom 2011
          }
        , { id = "renault/zoe/2013"
          , batteries = [ 22, 41 ]
          , comment = Nothing
          , count = 118
          , make = "Renault"
          , model = "Zoe"
          , price = Range 30000 69000
          , range = Range 140 280
          , safetyRating =
                { comment = Nothing
                , org = EuroNCAP
                , stars = 5
                , url = "https://www.euroncap.com/en/results/renault/zoe/8889"
                , year = 2013
                }
          , seats = [ 5 ]
          , years = RangeFrom 2013
          }
        , { id = "tesla/model-3/2019"
          , batteries = [ 62, 75 ]
          , comment = Nothing
          , count = 60
          , make = "Tesla"
          , model = "Model 3"
          , price = Range 74000 113000
          , range = Range 354 500
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/tesla/model-3/70118a"
                , year = 2019
                }
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "tesla/model-s/2014"
          , batteries = [ 75, 85, 90, 100 ]
          , comment = Nothing
          , count = 342
          , make = "Tesla"
          , model = "Model S"
          , price = Range 71000 189000
          , range = Range 401 595
          , safetyRating =
                { comment = Nothing
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/tesla/model-s/82b32c"
                , year = 2015
                }
          , seats = [ 5, 7 ]
          , years = RangeFrom 2014
          }
        , { id = "tesla/model-x/2017"
          , batteries = [ 75, 85, 90, 100 ]
          , comment = Nothing
          , count = 318
          , make = "Tesla"
          , model = "Model X"
          , price = Range 110000 207000
          , range = Range 383 523
          , safetyRating =
                { comment = Just "Only US results available"
                , org = NHTSA
                , stars = 5
                , url = "https://www.nhtsa.gov/vehicle/2019/TESLA/MODEL%252520X%252520P100D/SUV/AWD"
                , year = 2019
                }
          , seats = [ 5, 6, 7 ]
          , years = RangeFrom 2017
          }
        , { id = "volkswagen/e-golf/2015"
          , batteries = [ 24, 36 ]
          , comment = Nothing
          , count = 257
          , make = "Volkswagen"
          , model = "e-Golf"
          , price = Range 40000 69000
          , range = Range 133 201
          , safetyRating =
                { comment = Just "Only fossil fuel versions tested"
                , org = ANCAP
                , stars = 5
                , url = "https://www.ancap.com.au/safety-ratings/volkswagen/golf/7cda61"
                , year = 2013
                }
          , seats = [ 5 ]
          , years = RangeFrom 2015
          }
        ]


vehicleTextMarkup : String
vehicleTextMarkup =
    """
|> Page 
    id = audi/e-tron/2019
    text = 
        e-tron is Audi's first mass production electric vehicle. 

        It's equipped with dual electric motors. 

        The so-called "virtual side mirrors", with cameras on the outside and screens 
        on the inside of the front doors, are optional.

        Audi announced a recall of some e-tron models in June 2019. 

        Can be purchased new in New Zealand.

|> Page 
    id = bmw/i3/2013
    text = 
        The i3 was BMW's first mass-produced EV, and over 130,000 cars have been sold 
        around the world.

        Most of the car body is made of carbon-fibre reinforced plastic. 

        The i3 has rear-hinged rear doors to ease entry into the back seats. 

        This model has had three revisions. It started out with a 22 kWh battery and
        a range of 130 km. In 2017, the battery was upgraded to 33 kWh, extending the 
        range to 183 km. In 2019, the battery capacity was increased again to 42 kWh, 
        providing a range of 246 km. Cars with this battery should become available 
        soon.
        
        There is a REx variant of the car with a small petrol "range extender" engine. 

        Can be purchased new in New Zealand.

|> Page 
    id = hyundai/ioniq/2017
    text = 
        Over 25,000 fully electric Ioniqs have been sold around the world. 

        For new cars purchased in New Zealand, the battery is covered by a 10 year`/`unlimited km warranty.

        A 300km range model is coming in the near future.

        The Ioniq supports fast charging up to 50 kW, allowing it to reach 80% charge in 
        approximately 30 minutes.

        The car is also available as a plug-in hybrid with a 9 kWh battery and a conventional 
        hybrid with a 1.6 kWh battery.

        Can be purchased new in New Zealand.

|> Page 
    id = hyundai/kona/2019
    text =
        Hyundai Kona is the first fully electric subcompact crossover. 

        For new cars purchased in New Zealand, the battery is covered by a 10 year`/`160,000 km warranty.

        A smaller range battery option exists but is not available in NZ.

        Can be purchased new in New Zealand.

|> Page 
    id = jaguar/i-pace/2019
    text = 
        I-PACE represents Jaguar's first foray into fully electric vehicles. 

        The car is equipped with dual electric motors. 

        In addition to the boot, it has a small front storage compartment. 

        For new cars purchased in New Zealand, the battery is covered by an 8 year`/`160,000 km warranty.

        I-PACE supports fast charging up to 50 kW, allowing it to gain up to 270 km
        of range per hour of charging. The on-board 7kW AC charger delivers up to 35km of range per hour
        of charging.

        Can be purchased new in New Zealand.

|> Page 
    id = kia/niro-ev/2019
    text = 
        For new cars purchased in New Zealand, the battery is covered by a 7 year`/`160,000 km warranty.

        Niro EV supports fast charging up to 50 kW, allowing the 64 kWh model to recharge to 80% in 
        75 minutes (and the 39 kWh model in an hour). The on-board 7kW AC charger delivers up to 40km 
        of range per hour of charging.

        The car is also available as a plug-in hybrid with a 9 kWh battery and a conventional 
        hybrid with a 1.6 kWh battery (these are called Niro rather than Nero EV).

        Can be purchased new in New Zealand.

|> Page 
    id = kia/soul-ev/2015
    text = 
        The battery was upgraded from 27 kWh to 30 kWh in 2018.

        Soul EV cannot be purchased new in New Zealand, all cars are imported. 

        It will be replaced with a new vehicle with significantly larger range in 2020.

|> Page 
    id = ldv/ev80/2019
    text = 
        The van can carry a maximum payload of 1,000 kg, or 1,100 kg in the larger version.
        It is also available as a cab chassis.  

        The range listed is claimed by the manufacturer (EPA range is not available).

        The manufacturer suggests that the battery supports fast charging, allowing it 
        to be fully charged in as little as two hours. 

        Can be purchased new in New Zealand.

|> Page 
    id = mitsubishi/i-miev/2009
    text = 
        Over 45,000 cars have been sold worldwide.

        This car was also sold rebadged as Peugeot iOn & Citroën C-Zero. The Peugeot variant is higher spec.

        The 10.5 kWh and 14.5 kWh batteries available on some of the Japanese imports 
        use the lithium titanate oxide SCiB battery technology, allowing them to withstand
        2.5 times more charge`/`discharge cycles, and provide 1.7 times more driving range 
        than a regular Li-ion battery. SCiB batteries can also be charged rapidly, reaching
        80% capacity in 15 minutes. The 16 kWh battery is a regular Li-ion battery.

        Can no longer be purchased new in New Zealand (used to be sold new between 2011-2014). 

|> Page 
    id = nissan/e-nv200/2014
    text = 
        This van was initially sold with the 24 kWh battery from Nissan Leaf. In 2018, 
        the battery was upgraded to 40 kWh, increasing the range to almost 200 km. 

        Available in both cargo and passenger configurations.

        e-NV200 cannot be purchased new in New Zealand, all vehicles are imported. 

|> Page 
    id = nissan/leaf/2011
    text = 
        This is currently by far the most popular EV in New Zealand. (But note that 
        the count of registered vehicles includes the completely different
        next generation of this model.) Over 400,000 vehicles have been sold around
        the world.

        Please note that it is known to have brake malfunctions in some circumstances. 

        A small number of vehicles were sold new in New Zealand between 2012-2014, 
        however Nissan stopped sales of new cars here, so the majority of vehicles 
        are Japanese imports (with some coming from the UK as well). 

        There were several revisions of this model. The first revision, produced in 
        2011-2012, has a slightly smaller boot. 

        The battery capacity is known to degrade with time at a rate of roughly 3% per year. 

        From 2015 onwards, the battery was improved to be more heat tolerant, and 
        appears to degrade at a slightly lower rate. 

        In 2016, a higher capacity 30 kWh battery option was added. 

        Sam Holford has written a great 
        [detailed guide]{ link | url = https://samholford.github.io/leafguide/ } about this model.

|> Page 
    id = nissan/leaf/2017
    text = 
        This is the second, completely new generation of New Zealand's most popular EV. 
        
        (Note that the count of registered vehicles includes the completely different
        previous generation of this model, currently more widespread by far.)

        The battery is air cooled, without an active cooling system, same as the 
        previous generation of the Leaf. 

        A 363km range variant with a 62kWh battery should become available in 2019.

        Can be purchased new in New Zealand.

|> Page 
    id = renault/kangoo/2011
    text = 
        Over 38,000 vans have been sold around the world. 

        Available in a cargo or passenger configuration.

        Cannot be purchased new in New Zealand.

|> Page 
    id = renault/zoe/2013
    text = 
        Over 133,000 vehicles have been sold around the world. 

        Zoe only supports AC charging, from 2kW to 22kW. At 22kW, a full charge
        takes 2 hours 40 minutes. Vehicles from 2017 onwards also have an option for 
        43kW charging, which provides 80% charge in about an hour. 
        
        Can be purchased new in New Zealand.

|> Page 
    id = tesla/model-3/2019
    text = 
        This is the newest model in Tesla's lineup.

        The interior is distinct in its lack of buttons: everything is controlled
        via a centre-mounted touch screen and two scroll wheels on the steering wheel. 
        All models come with a glass roof. There is a small luggage compartment in
        the front of the car, in addition to the boot. 

        There is a rear-wheel drive, single motor variant, as well as an all-wheel
        drive, dual-motor variant.

        For new cars purchased in New Zealand, the battery and drive unit are 
        covered by a warranty for 8 years or 160,000 km
        (or 192,000km for the Long Range variant), 
        whichever comes first, with minimum 70% retention of battery capacity over 
        the warranty period.

        AC charging allows up to 70km of range to be added in an hour, while DC
        charging allows up to 290km of range to be added in 15 minutes (via the 
        latest Tesla Supercharger, not deployed in New Zealand currently).

        Tesla regularly updates the software in the car over the air, adding new 
        functionality. 

        Can be purchased new in New Zealand via the Tesla website. 

|> Page 
    id = tesla/model-s/2014
    text = 
        Model S has been the longest range EV since 2012, and remains so at present. 

        It is also the fastest accelerating production car in the world. 

        There is a rear-wheel drive, single motor variant, as well as an all-wheel
        drive, dual-motor variant.

        For new cars purchased in New Zealand, the battery and drive unit are covered 
        by a warranty for 8 years.

        The interior can have 5 or 7 seats. The car has a small luggage compartment 
        in the front, in addition to the boot. 

        Tesla regularly updates the software in the car over the air, adding new 
        functionality. 

        Can be purchased new in New Zealand via the Tesla website. 

|> Page 
    id = tesla/model-x/2017
    text = 
        Over 100,000 Model X vehicles have been sold around the world.

        The car is equipped with dual electric motors in an all-wheel drive configuration.

        For new cars purchased in New Zealand, the battery and drive unit are covered 
        by a warranty for 8 years.

        The interior can have 5, 6, or 7 seats. The car has distinctive gull-wing 
        front doors. It has a smaller luggage compartment in the front, in addition 
        to the boot. 

        Tesla regularly updates the software in the car over the air, adding new 
        functionality. 

        Can be purchased new in New Zealand via the Tesla website. 

|> Page 
    id = volkswagen/e-golf/2015
    text = 
        From 2017, e-Golf is equipped with a larger 36kWh battery.

        Using Type 2`/`CCS fast charging, the battery can be charged to 80% in about 45 minutes.
        However, the DC fast charging function is limited to two consecutive uses to avoid
        overheating the battery. 

        Can be purchased new in New Zealand.

"""
