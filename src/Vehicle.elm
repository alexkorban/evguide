module Vehicle exposing (..)

import Cons exposing (Cons, cons)
import List.Extra as List
import Types exposing (..)
import Util exposing (..)


type alias Vehicle =
    { id : VehicleId
    , batteries : List Int
    , comment : Maybe String
    , count : Int
    , make : String
    , model : String
    , price : SingleOrPair Int
    , range : SingleOrPair Int
    , seats : List Int
    , years : SingleOrPair Int
    }


allCommentsShort : Bool
allCommentsShort =
    Cons.all (\v -> (String.length <| Maybe.withDefault "" v.comment) < 46) data


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
        , comment = Just "Available soon"
        , count = 28
        , make = "Audi"
        , model = "e-tron"
        , price = Range 149000 180000
        , range = Single 328
        , seats = [ 5 ]
        , years = RangeFrom 2019
        }
        [ { id = "bmw/i3/2013"
          , batteries = [ 22, 33, 42 ]
          , comment = Just "260km range model coming in the near future"
          , count = 199
          , make = "BMW"
          , model = "i3"
          , price = Range 35000 85000
          , range = Range 130 260
          , seats = [ 4 ]
          , years = RangeFrom 2013
          }
        , { id = "hyundai/ioniq/2017"
          , batteries = [ 28, 38 ]
          , comment = Just "300km range model coming in the near future"
          , count = 544
          , make = "Hyundai"
          , model = "Ioniq"
          , price = Range 45000 60000
          , range = Range 219 300
          , seats = [ 4 ]
          , years = RangeFrom 2017
          }
        , { id = "hyundai/kona/2019"
          , batteries = [ 64 ]
          , comment = Nothing
          , count = 341
          , make = "Hyundai"
          , model = "Kona"
          , price = Range 74000 78000
          , range = Single 415
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "jaguar/i-pace/2019"
          , batteries = [ 90 ]
          , comment = Nothing
          , count = 51
          , make = "Jaguar"
          , model = "I-PACE"
          , price = Range 160000 195000
          , range = Single 377
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "kia/niro/2019"
          , batteries = [ 39, 64 ]
          , comment = Nothing
          , count = 53
          , make = "Kia"
          , model = "Niro"
          , price = Range 68000 74000
          , range = Range 289 455
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
          , range = Range 150 179
          , seats = [ 5 ]
          , years = RangeFrom 2015
          }
        , { id = "ldv/ev80/2019"
          , batteries = [ 56 ]
          , comment = Just "Range is manufacturer claim, not EPA"
          , count = 36
          , make = "LDV"
          , model = "EV80"
          , price = Single 50000
          , range = Single 180
          , seats = [ 3 ]
          , years = RangeFrom 2019
          }
        , { id = "mitsubishi/i-miev/2009"
          , batteries = [ 11, 14, 16 ]
          , comment = Just "Also sold as Peugeot iOn & Citroën C-Zero"
          , count = 108
          , make = "Mitsubishi"
          , model = "i-MiEV"
          , price = RangeFrom 11000
          , range = Range 75 100
          , seats = [ 4 ]
          , years = RangeFrom 2009
          }
        , { id = "nissan/e-nv200/2014"
          , batteries = [ 24, 40 ]
          , comment = Nothing
          , count = 359
          , make = "Nissan"
          , model = "e-NV200"
          , price = Range 28000 65000
          , range = Range 121 194
          , seats = [ 2, 5, 7 ]
          , years = RangeFrom 2014
          }
        , { id = "nissan/leaf/2011"
          , batteries = [ 24, 30 ]
          , comment = Nothing
          , count = 8160
          , make = "Nissan"
          , model = "Leaf"
          , price = Range 10000 30000
          , range = Range 117 172
          , seats = [ 5 ]
          , years = Range 2011 2017
          }
        , { id = "nissan/leaf/2018"
          , batteries = [ 40, 62 ]
          , comment = Just "Longer range model coming in 2019"
          , count = 8160
          , make = "Nissan"
          , model = "Leaf"
          , price = Range 50000 59000
          , range = Range 243 363
          , seats = [ 5 ]
          , years = RangeFrom 2018
          }
        , { id = "renault/kangoo/2011"
          , batteries = [ 22, 33 ]
          , comment = Nothing
          , count = 53
          , make = "Renault"
          , model = "Kangoo"
          , price = Range 46000 75000
          , range = Range 120 190
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
          , seats = [ 5 ]
          , years = RangeFrom 2013
          }
        , { id = "tesla/model-3/2019"
          , batteries = [ 55, 65 ]
          , comment = Nothing
          , count = 3
          , make = "Tesla"
          , model = "Model 3"
          , price = Range 74000 113000
          , range = Range 460 620
          , seats = [ 5 ]
          , years = RangeFrom 2019
          }
        , { id = "tesla/model-s/2014"
          , batteries = [ 75, 85, 90, 100 ]
          , comment = Nothing
          , count = 339
          , make = "Tesla"
          , model = "Model S"
          , price = Range 71000 189000
          , range = Range 401 595
          , seats = [ 5, 7 ]
          , years = RangeFrom 2014
          }
        , { id = "tesla/model-x/2017"
          , batteries = [ 75, 85, 90, 100 ]
          , comment = Nothing
          , count = 317
          , make = "Tesla"
          , model = "Model X"
          , price = Range 110000 207000
          , range = Range 383 523
          , seats = [ 5, 6, 7 ]
          , years = RangeFrom 2017
          }
        , { id = "volkswagen/e-golf/2015"
          , batteries = [ 24, 36 ]
          , comment = Nothing
          , count = 243
          , make = "Volkswagen"
          , model = "e-Golf"
          , price = Range 40000 69000
          , range = Range 133 201
          , seats = [ 5 ]
          , years = RangeFrom 2015
          }
        ]


vehicleTextMarkup : String
vehicleTextMarkup =
    """
|> Page 
    id = bmw/i3/2013
    text = 
        This model had a number of revisions. 

        It started out with a 22 kWh battery and a range of 130 km, and
        was later upgraded to 33 kWh and 183 km. 

        |> H2
            Third revision
        
        The third revision will have a 42 kWh battery and a range of 260 km.

|> Page 
    id = nissan/leaf/2011
    text = 
        Note that the count of registered vehicles includes the completely different
        next generation of this model. 

|> Page 
    id = nissan/leaf/2018
    text = 
        Note that the count of registered vehicles includes the completely different
        previous generation of this model, currently more widespread by far. 

"""
