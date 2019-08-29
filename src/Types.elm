module Types exposing (..)

import Dict exposing (Dict)
import Element exposing (Element)


type SingleOrPair a
    = Single a
    | Range a a
    | RangeFrom a


type alias PageDict msg =
    Dict String (Size -> Element msg)


type alias Size =
    { height : Int, width : Int }


type alias VehicleId =
    String


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

        Range a _ ->
            a

        RangeFrom a ->
            a


rangeTo : SingleOrPair a -> a
rangeTo range =
    case range of
        Single a ->
            a

        Range _ a ->
            a

        RangeFrom a ->
            a


rangeMidpoint : SingleOrPair Int -> Int
rangeMidpoint range =
    case range of
        Single a ->
            a

        Range a b ->
            round (toFloat (a + b) / 2)

        RangeFrom a ->
            a
