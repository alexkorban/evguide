module Cards exposing (main)

import Cons
import Element exposing (..)
import Main as App
import Ui
import UiCards exposing (card, cardError, deck, show)


vehicle =
    Cons.head App.vehicles


initialModel =
    {}


main =
    show App.update
        [ deck "Wide layout"
            [ card "Layout" initialModel <|
                \model ->
                    App.view model
            , card "Logo" initialModel <|
                \model ->
                    layout [ width <| px 800 ] <| Ui.logo <| text "EV Guide"
            , card "Vehicle card" initialModel <|
                \model ->
                    layout [ width <| px 800 ] <| App.vehicleCard vehicle
            ]
        , deck "Narrow layout"
            [ card "Layout" initialModel <|
                \model ->
                    App.view model
            ]
        ]
