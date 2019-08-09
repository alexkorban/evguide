module Types exposing (SingleOrPair(..), VehicleMake, VehicleModel, Year, rangeFrom)


type SingleOrPair a
    = Single a
    | Range a a


type alias VehicleMake =
    String


type alias VehicleModel =
    String


type alias Year =
    Int


rangeFrom : SingleOrPair a -> a
rangeFrom range =
    case range of
        Single a ->
            a

        Range a b ->
            a
