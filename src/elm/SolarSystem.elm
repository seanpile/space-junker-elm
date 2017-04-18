module SolarSystem exposing (au, SolarSystem, seed, advance)

import Time exposing (Time)
import Orbits exposing (Orbit)
import KeplerElements exposing (KeplerElements)


au : Float
au =
    1.4959787e11


type alias SolarSystem =
    { sun : Body
    , time : Time
    }


type Body
    = Body
        { constants : BodyConstants
        , secondaries : List Body
        , orbit : Maybe Orbit
        }


type BodyConstants
    = Planet PlanetConstants


type alias PlanetConstants =
    { name : String
    , elements : Maybe KeplerElements
    , radius : Float
    , mass : Float
    , u : Float
    }



-- Scale down a value by the Astronomical Unit (au)


scale : Float -> Float
scale x =
    x / au



-- Scale down a value by the 3rd power of the Astronomical Unit (au)


scale3 : Float -> Float
scale3 x =
    x / (List.product (List.repeat 3 x))


sun : BodyConstants
sun =
    Planet
        { name = "sun"
        , radius = scale 6.96e8
        , u = scale3 1.3271243800000001e20
        , mass = 1.9891e30
        , elements = Nothing
        }


earth : BodyConstants
earth =
    Planet
        { name = "earth"
        , radius = scale 6.3781e6
        , mass = 5.9737e24
        , u = scale3 3.986e14
        , elements =
            Just
                { a = { epoch = 1.00000018, delta = -0.00000003 }
                , e = { epoch = 0.01673163, delta = -0.00003661 }
                , i = { epoch = -0.00054346, delta = -0.01337178 }
                , l = { epoch = 100.46691572, delta = 35999.37306329 }
                , w = { epoch = 102.93005885, delta = 0.3179526 }
                , omega = { epoch = -5.11260389, delta = -0.24123856 }
                }
        }


seed : Time -> SolarSystem
seed time =
    let
        system =
            { time = time
            , sun =
                Body
                    { constants = sun
                    , orbit = Nothing
                    , secondaries =
                        [ Body
                            { constants = earth
                            , secondaries = []
                            , orbit = Nothing
                            }
                        ]
                    }
            }
    in
        { system | sun = initialize time Nothing system.sun }



{-
   Initialize a body given a specific time and it's primary
-}


initialize : Time -> Maybe Body -> Body -> Body
initialize t primary body =
    body


advance : Float -> SolarSystem -> SolarSystem
advance dx system =
    { system | time = system.time + dx }
