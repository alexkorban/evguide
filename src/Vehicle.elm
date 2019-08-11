module Vehicle exposing (..)

import Cons exposing (Cons, cons)
import List.Extra as List
import Types exposing (..)
import Util exposing (..)


type alias Vehicle =
    { make : String
    , model : String
    , range : SingleOrPair Int
    , years : SingleOrPair Int
    , price : SingleOrPair Int
    , batteries : List Int
    , comment : Maybe String
    }


allCommentsShort : Bool
allCommentsShort =
    Cons.all (\v -> (String.length <| Maybe.withDefault "" v.comment) < 46) data


find : VehicleMake -> VehicleModel -> Year -> Maybe Vehicle
find make model year =
    data
        |> Cons.toList
        |> List.find
            (\v ->
                equalBy String.toLower v.make make
                    && equalBy String.toLower v.model model
                    && (rangeFrom v.years == year)
            )


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


pricesAsString : SingleOrPair Int -> String
pricesAsString prices =
    case prices of
        Single price ->
            asMoneyStr price

        Range from to ->
            asMoneyStr from ++ "-" ++ asMoneyStr to


yearsAsString : SingleOrPair Year -> String
yearsAsString years =
    case years of
        Single year ->
            String.fromInt year ++ "-now"

        Range from to ->
            String.fromInt from ++ "-" ++ String.fromInt to



-- trimRange : (Trim -> comparable) -> Vehicle -> ( comparable, comparable )
-- trimRange trimField vehicle =
--     ( Cons.minimum <| Cons.map trimField vehicle.trims, Cons.maximum <| Cons.map trimField vehicle.trims )


data : Cons Vehicle
data =
    cons
        { make = "BMW"
        , model = "i3"
        , range = Range 130 260
        , years = Single 2013
        , price = Range 35000 85000
        , batteries = [ 22, 33, 42 ]
        , comment = Just "260km range model coming in the near future"
        }
        [ { make = "Hyundai"
          , model = "Ioniq"
          , range = Range 219 300
          , years = Single 2017
          , price = Range 45000 60000
          , batteries = [ 28, 38 ]
          , comment = Just "300km range model coming in the near future"
          }
        , { make = "Hyundai"
          , model = "Kona"
          , range = Single 415
          , years = Single 2019
          , price = Range 74000 78000
          , batteries = [ 64 ]
          , comment = Nothing
          }
        , { make = "Kia"
          , model = "Niro"
          , range = Range 289 455
          , years = Single 2019
          , price = Range 68000 74000
          , batteries = [ 39, 64 ]
          , comment = Nothing
          }
        , { make = "Nissan"
          , model = "Leaf"
          , range = Range 117 172
          , years = Range 2011 2017
          , price = Range 10000 30000
          , batteries = [ 24, 30 ]
          , comment = Nothing
          }
        , { make = "Nissan"
          , model = "Leaf"
          , range = Range 243 363
          , years = Single 2018
          , price = Range 50000 59000
          , batteries = [ 40, 62 ]
          , comment = Just "Longer range model coming in 2019"
          }
        , { make = "Tesla"
          , model = "Model 3"
          , range = Range 460 620
          , years = Single 2019
          , price = Range 74000 104000
          , batteries = [ 55, 65 ]
          , comment = Nothing
          }
        ]


vehicleText : String
vehicleText =
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
"""
