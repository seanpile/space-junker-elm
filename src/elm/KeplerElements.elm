module KeplerElements exposing (KeplerElements)

import Types exposing (Degrees)


type alias DistanceElement =
    { epoch : Float, delta : Float }


type alias AngleElement =
    { epoch : Degrees, delta : Degrees }


type alias KeplerElements =
    { a : DistanceElement
    , e : DistanceElement
    , i : AngleElement
    , l : AngleElement
    , w : AngleElement
    , omega : AngleElement
    }
