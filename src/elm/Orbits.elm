module Orbits exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import Date exposing (Date, fromString, fromTime)
import Date.Extra.Duration exposing (diffDays)
import Types exposing (..)
import OrbitUtils exposing (frem, calculateEccentricAnomaly, calculateMeanAnomaly, transformToEcliptic)


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


fromKeplerElements : Time -> Primary -> KeplerElements -> Orbit
fromKeplerElements t primary elements =
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

        -- TODO: Add Perturbations here for outer planets
        meanAnomaly =
            calculateMeanAnomaly l w julianDate

        parameters =
            Elliptic
                { gm = primary.constants.gm
                , semiMajorAxis = a
                , eccentricity = e
                , inclination = degrees i
                , argumentPerihelion = degrees argumentPerihelion
                , longitudeAscendingNode = degrees omega
                , meanAnomaly = meanAnomaly
                }
    in
        { parameters = parameters
        , derived = statistics parameters
        }


meanAngularMotion : Orbit -> Float
meanAngularMotion orbit =
    case orbit.parameters of
        Stationary ->
            0

        Elliptic elements ->
            sqrt (elements.gm / (elements.semiMajorAxis ^ 3))


advance : Time -> Orbit -> Orbit
advance dt orbit =
    case orbit.parameters of
        ------------------------------------
        Stationary ->
            orbit

        ------------------------------------
        Elliptic parameters ->
            let
                n =
                    meanAngularMotion orbit

                m0 =
                    parameters.meanAnomaly + (n * dt / 1000)

                meanAnomaly =
                    if (parameters.eccentricity < 1) then
                        frem m0 (2 * pi)
                    else
                        m0

                params =
                    { parameters | meanAnomaly = meanAnomaly }

                stats =
                    statistics (Elliptic params)
            in
                { parameters = Elliptic params
                , derived = stats
                }


statistics : OrbitalParameters -> OrbitalStatistics
statistics parameters =
    case parameters of
        ------------------------------------
        Stationary ->
            { position = vec3 0 0 0
            , velocity = vec3 0 0 0
            , semiMajorAxis = 0
            , semiMinorAxis = 0
            , center = vec3 0 0 0
            , periapsis = vec3 0 0 0
            , apoapsis = vec3 0 0 0
            }

        ------------------------------------
        Elliptic params ->
            let
                a =
                    params.semiMajorAxis

                e =
                    params.eccentricity

                b =
                    a * sqrt (1 - e ^ 2)

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
                        (a * sqrt (1 - e ^ 2) * sin (eccentricAnomaly))
                        0

                periapsis =
                    vec3 (a * (1 - e)) 0 0

                apoapsis =
                    vec3 (-a * (1 + e)) 0 0

                center =
                    vec3 ((Vec3.getX periapsis) - a) 0 0

                transformFn =
                    transformToEcliptic
                        params.argumentPerihelion
                        params.inclination
                        params.longitudeAscendingNode
            in
                { position = transformFn perifocalPosition
                , velocity = vec3 0 0 0
                , semiMajorAxis = a
                , semiMinorAxis = b
                , periapsis = transformFn periapsis
                , apoapsis = transformFn apoapsis
                , center = transformFn center
                }
