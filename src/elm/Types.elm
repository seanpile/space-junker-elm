module Types exposing (..)

import Math.Vector3 exposing (Vec3)


type alias Degrees =
    Float


type alias Radians =
    Float


type alias BodyId =
    String


type alias Body =
    { name : BodyId
    , constants : BodyConstants
    , secondaries : Secondaries
    , orbit : Orbit
    }


type alias Primary =
    Body


type Secondaries
    = Secondaries (List Body)


type alias BodyConstants =
    { elements : KeplerElements
    , radius : Float
    , mass : Float
    , gm : Float
    }


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


type alias Orbit =
    { parameters : OrbitalParameters
    , derived : OrbitalStatistics
    }


type OrbitalParameters
    = Stationary
    | Elliptic OrbitalElements



{-
   Each Orbit is uniquely defined by 6 values that can position a body in space and time
-}


type alias OrbitalElements =
    { gm : Float
    , semiMajorAxis : Float
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
    { position : Vec3
    , velocity : Vec3
    , semiMajorAxis : Float
    , semiMinorAxis : Float
    , center : Vec3
    , periapsis : Vec3
    , apoapsis : Vec3
    }
