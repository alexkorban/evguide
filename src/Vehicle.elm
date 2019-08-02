module Vehicle exposing (SingleOrPair(..), Vehicle, data)

import Cons exposing (Cons, cons)


type SingleOrPair a
    = Single a
    | Range a a


type alias Vehicle =
    { make : String
    , model : String
    , imageUrl : String
    , range : SingleOrPair Int
    , years : SingleOrPair Int
    , price : SingleOrPair Int
    , batteries : List Int
    , comment : Maybe String
    }



-- trimRange : (Trim -> comparable) -> Vehicle -> ( comparable, comparable )
-- trimRange trimField vehicle =
--     ( Cons.minimum <| Cons.map trimField vehicle.trims, Cons.maximum <| Cons.map trimField vehicle.trims )


data : Cons Vehicle
data =
    cons
        { make = "Kia"
        , model = "Niro"
        , imageUrl = "/img/2019-kia-nero-small.jpg"
        , range = Range 289 455
        , years = Single 2019
        , price = Range 68000 74000
        , batteries = [ 39, 64 ]
        , comment = Nothing
        }
        [ { make = "Nissan"
          , model = "Leaf"
          , imageUrl = "/img/2016-nissan-leaf-small.jpg"
          , range = Range 120 180
          , years = Range 2011 2017
          , price = Range 11000 30000
          , batteries = [ 24, 30 ]
          , comment = Nothing
          }
        , { make = "Tesla"
          , model = "Model 3"
          , imageUrl = "/img/2019-tesla-model-3-small.jpg"
          , range = Range 460 620
          , years = Single 2019
          , price = Range 74000 104000
          , batteries = [ 55, 65 ]
          , comment = Just "Available from August 2019"
          }
        ]
