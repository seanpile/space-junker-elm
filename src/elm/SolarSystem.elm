module SolarSystem exposing (au, SolarSystem, find, findByName, seed, advance, visit)

import Time exposing (Time)
import Orbits exposing (fromKeplerElements)
import Types exposing (..)
import Math.Vector3 exposing (vec3, Vec3)


au : Float
au =
    1.4959787e11


type alias SolarSystem =
    { sun : Body
    , time : Time
    }


sun : BodyConstants
sun =
    { radius = scale 6.96e6
    , gm = scale3 1.3271243800000001e20
    , mass = 1.9891e30
    , elements =
        { a = { epoch = 0, delta = 0 }
        , e = { epoch = 0, delta = 0 }
        , i = { epoch = 0, delta = 0 }
        , l = { epoch = 0, delta = 0 }
        , w = { epoch = 0, delta = 0 }
        , omega = { epoch = 0, delta = 0 }
        }
    }


mercury : BodyConstants
mercury =
    { radius = scale 2.4397e6
    , gm = scale3 2.203e13
    , mass = 3.302e22
    , elements =
        { a = { epoch = 0.38709843, delta = 0 }
        , e = { epoch = 0.20563661, delta = 0.00002123 }
        , i = { epoch = 7.00559432, delta = -0.00590158 }
        , l = { epoch = 252.25166724, delta = 149472.67486623 }
        , w = { epoch = 77.45771895, delta = 0.15940013 }
        , omega = { epoch = 48.33961819, delta = -0.12214182 }
        }
    }


venus : BodyConstants
venus =
    { radius = scale 6.0518e6
    , gm = scale3 3.249e14
    , mass = 4.8685e24
    , elements =
        { a = { epoch = 0.72332102, delta = -0.00000026 }
        , e = { epoch = 0.00676399, delta = -0.00005107 }
        , i = { epoch = 3.39777545, delta = 0.00043494 }
        , l = { epoch = 181.9797085, delta = 58517.8156026 }
        , w = { epoch = 131.76755713, delta = 0.05679648 }
        , omega = { epoch = 76.67261496, delta = -0.27274174 }
        }
    }


earth : BodyConstants
earth =
    { radius = scale 6.3781e6
    , mass = 5.9737e24
    , gm = scale3 3.986e14
    , elements =
        { a = { epoch = 1.00000018, delta = -0.00000003 }
        , e = { epoch = 0.01673163, delta = -0.00003661 }
        , i = { epoch = -0.00054346, delta = -0.01337178 }
        , l = { epoch = 100.46691572, delta = 35999.37306329 }
        , w = { epoch = 102.93005885, delta = 0.3179526 }
        , omega = { epoch = -5.11260389, delta = -0.24123856 }
        }
    }



{-
   Return the kepler elements associated with this body
-}


stationaryOrbit : Orbit
stationaryOrbit =
    { parameters = Stationary
    , derived =
        { position = vec3 0 0 0
        , velocity = vec3 0 0 0
        , semiMajorAxis = 0
        , semiMinorAxis = 0
        , center = vec3 0 0 0
        , apoapsis = vec3 0 0 0
        , periapsis = vec3 0 0 0
        }
    }


seed : Time -> SolarSystem
seed time =
    let
        system =
            { time = time
            , sun =
                { name = "sun"
                , constants = sun
                , orbit = stationaryOrbit
                , secondaries =
                    Secondaries
                        [ { name = "mercury"
                          , constants = mercury
                          , secondaries = Secondaries ([])
                          , orbit = stationaryOrbit
                          }
                        , { name = "venus"
                          , constants = venus
                          , secondaries = Secondaries ([])
                          , orbit = stationaryOrbit
                          }
                        , { name = "earth"
                          , constants = earth
                          , secondaries = Secondaries ([])
                          , orbit = stationaryOrbit
                          }
                        ]
                }
            }

        -- Initialize each Orbit for the given time
        initializeFn : Maybe Primary -> Body -> Body
        initializeFn primary body =
            case primary of
                Nothing ->
                    body

                Just p ->
                    { body | orbit = (fromKeplerElements time p body.constants.elements) }
    in
        map initializeFn system



{-
   Advance the Solar System by dt milliseconds
-}


advance : Time -> SolarSystem -> SolarSystem
advance dt system =
    let
        advanceFn : Maybe Primary -> Body -> Body
        advanceFn primary body =
            { body | orbit = (Orbits.advance dt body.orbit) }
    in
        let
            nextSystem =
                map advanceFn system
        in
            { nextSystem
                | time = system.time + dt
            }



{-
   ----------------
   Helper Functions
   -----------------
-}
-- Scale down a value by the Astronomical Unit (au)


scale : Float -> Float
scale x =
    x / au



-- Scale down a value by the 3rd power of the Astronomical Unit (au)


scale3 : Float -> Float
scale3 x =
    x / (au ^ 3)



-- Find a body by name


findByName : BodyId -> SolarSystem -> Maybe Body
findByName bodyId system =
    find (\{ name } -> name == bodyId) system



-- Find the first body that satisifies the condition


find : (Body -> Bool) -> SolarSystem -> Maybe Body
find fn system =
    let
        findImpl : (Body -> Bool) -> Body -> Maybe Body
        findImpl fn body =
            if (fn body) then
                Just body
            else
                case body.secondaries of
                    Secondaries bodies ->
                        List.filterMap (findImpl fn) bodies
                            |> List.head
    in
        findImpl fn system.sun



-- Apply a function to each body in the SolarSystem, returning the modified System


map : (Maybe Primary -> Body -> Body) -> SolarSystem -> SolarSystem
map fn system =
    let
        mapImpl : (Maybe Primary -> Body -> Body) -> Maybe Primary -> Body -> Body
        mapImpl fn primary body =
            let
                mapped =
                    (fn primary body)
            in
                case mapped.secondaries of
                    Secondaries bodies ->
                        { mapped
                            | secondaries = Secondaries (List.map (mapImpl fn (Just mapped)) bodies)
                        }
    in
        { system
            | sun = mapImpl fn Nothing system.sun
        }



{-
   Apply a function to each body in the solar system (primary bodies are always processed before
   their secondary bodies, but the traversal order is otherwise not guaranteed
-}


visit : (Body -> a) -> SolarSystem -> List a
visit fn system =
    let
        visitImpl : (Body -> a) -> Body -> List a
        visitImpl fn body =
            case body.secondaries of
                Secondaries bodies ->
                    (fn body) :: List.concat (List.map (visitImpl fn) bodies)
    in
        visitImpl fn system.sun
