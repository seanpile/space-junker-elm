module Orbits exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Time exposing (Time)
import Date exposing (Date, fromString, fromTime)
import Date.Extra.Duration exposing (diffDays)
import Types exposing (Degrees, Radians)
import KeplerElements exposing (KeplerElements)
import OrbitUtils exposing (calculateEccentricAnomaly, calculateMeanAnomaly, transformToEcliptic)


type alias Orbit =
    { parameters : OrbitalParameters
    , derived : OrbitalStatistics
    }


type OrbitalParameters
    = Elliptic OrbitalElements



{-
   Each Orbit is uniquely defined by 6 values that can position a body in space and time
-}


type alias OrbitalElements =
    { semiMajorAxis : Float
    , eccentricity : Float
    , inclination : Radians
    , argumentPerihelion : Radians
    , longitudeAscendingNode : Radians
    , meanAnomaly : Radians
    }



{-
   Calculated values that are dependent on the OrbitalElements
-}


type alias OrbitalStatistics =
    { position : Vec3, velocity : Vec3 }


j2000Date : Date
j2000Date =
    case fromString ("2000-01-01") of
        Ok val ->
            val

        Err err ->
            Debug.crash ("Shouldn't happen!")



{-
   Transform a specific set of KeplerElements into an orbit at the given time
-}


fromKeplerElements : Time -> Maybe KeplerElements -> Maybe Orbit
fromKeplerElements t elems =
    case elems of
        Nothing ->
            Nothing

        Just elements ->
            let
                now =
                    fromTime t

                julianDate =
                    (toFloat (diffDays now j2000Date)) / 36525

                a =
                    elements.a.epoch + elements.a.delta * julianDate

                e =
                    elements.e.epoch + elements.e.delta * julianDate

                i =
                    elements.i.epoch + elements.i.delta * julianDate

                l =
                    elements.l.epoch + elements.l.delta * julianDate

                w =
                    elements.w.epoch + elements.w.delta * julianDate

                omega =
                    elements.omega.epoch + elements.omega.epoch * julianDate

                argumentPerihelion =
                    w - omega

                meanAnomaly =
                    calculateMeanAnomaly l w julianDate

                parameters =
                    Elliptic
                        { semiMajorAxis = a
                        , eccentricity = e
                        , inclination = degrees i
                        , argumentPerihelion = degrees argumentPerihelion
                        , longitudeAscendingNode = degrees omega
                        , meanAnomaly = degrees meanAnomaly
                        }
            in
                Just
                    { parameters = parameters
                    , derived = statistics parameters
                    }


statistics : OrbitalParameters -> OrbitalStatistics
statistics parameters =
    case parameters of
        Elliptic params ->
            let
                a =
                    params.semiMajorAxis

                e =
                    params.eccentricity

                m0 =
                    params.meanAnomaly

                eccentricAnomaly =
                    calculateEccentricAnomaly e m0

                trueAnomaly =
                    2
                        * atan2
                            (sqrt (1 + e) * sin (eccentricAnomaly / 2))
                            (sqrt (1 - e) * cos (eccentricAnomaly / 2))

                perifocalPosition =
                    vec3
                        (a * ((cos eccentricAnomaly) - e))
                        (a * sqrt (1 - e * e) * sin (eccentricAnomaly))
                        0
            in
                { position =
                    transformToEcliptic
                        params.argumentPerihelion
                        params.inclination
                        params.longitudeAscendingNode
                        perifocalPosition
                , velocity = vec3 0 0 0
                }
