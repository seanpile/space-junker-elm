module Orbits exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Time exposing (Time)
import Date exposing (Date, fromString, fromTime)
import Date.Extra.Duration exposing (diffDays)
import Types exposing (Degrees, Radians)
import KeplerElements exposing (KeplerElements)


type Orbit
    = Elliptic OrbitalElements


type alias OrbitalElements =
    { semiMajorAxis : Float
    , eccentricity : Float
    , inclination : Radians
    , argumentPerhelion : Radians
    , longitudeAscendingNode : Radians
    , meanAnomaly : Radians
    , position : Vec3
    , velocity : Vec3
    }


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


fromKeplerElements : Time -> KeplerElements -> OrbitalElements
fromKeplerElements t elements =
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
    in
        { semiMajorAxis = a
        , eccentricity = e
        , inclination = degrees i
        , argumentPerhelion = degrees argumentPerihelion
        , longitudeAscendingNode = degrees omega
        , meanAnomaly = meanAnomaly
        , position = vec3 0 0 0
        , velocity = vec3 0 0 0
        }


calculateMeanAnomaly : Degrees -> Degrees -> Time -> Radians
calculateMeanAnomaly meanLongitude longitudeOfPerihelion time =
    let
        meanAnomaly =
            rem (round (meanLongitude - longitudeOfPerihelion)) 360
    in
        degrees
            (toFloat
                (if meanAnomaly > 180 then
                    meanAnomaly - 360
                 else if meanAnomaly < -180 then
                    meanAnomaly + 360
                 else
                    meanAnomaly
                )
            )
