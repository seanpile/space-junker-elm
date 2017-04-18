module OrbitUtils exposing (..)

import Time exposing (Time)
import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (makeRotate, transform)
import Types exposing (Degrees, Radians)


-- Remainder function supporting Float params


frem : Float -> Float -> Float
frem a b =
    a - b * (toFloat (truncate (a / b)))



-- TODO: Add offset


transformToEcliptic : Radians -> Radians -> Radians -> Vec3 -> Vec3
transformToEcliptic argumentPerihelion inclination longitudeAscendingNode vec =
    let
        m1 =
            makeRotate argumentPerihelion Math.Vector3.k

        m2 =
            makeRotate inclination Math.Vector3.i

        m3 =
            makeRotate longitudeAscendingNode Math.Vector3.k
    in
        vec
            |> transform m1
            |> transform m2
            |> transform m3



-- Normalize an angle between -PI and +PI


normalize : Radians -> Radians
normalize rads =
    if (rads >= -pi && rads <= pi) then
        rads
    else
        let
            rem =
                frem rads (2 * pi)
        in
            if (rem < -pi) then
                rem + 2 * pi
            else if (rem > pi) then
                rem - 2 * pi
            else
                rem



-- TODO Finish converting this; only applies to ecliptic orbits


calculateEccentricAnomaly : Float -> Radians -> Radians
calculateEccentricAnomaly e m0 =
    let
        tol =
            1.0e-12

        maxTimes =
            15

        m =
            normalize m0

        offset =
            m0 - m

        iterate : Float -> Int -> Float
        iterate current times =
            let
                err =
                    (current - (e * sin (current)) - m) / (1 - (e * cos (current)))

                next =
                    current - err
            in
                if (abs err <= tol) then
                    next + offset
                else if ((times + 1) >= maxTimes) then
                    Debug.crash ("Failed to derive eccentric anomaly")
                else
                    iterate next (times + 1)
    in
        iterate (atan2 (sin m) ((cos m) - e)) 0


calculateMeanAnomaly : Degrees -> Degrees -> Time -> Radians
calculateMeanAnomaly meanLongitude longitudeOfPerihelion time =
    frem (meanLongitude - longitudeOfPerihelion) 360
        |> degrees
        |> normalize
