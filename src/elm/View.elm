module View exposing (..)

import Html exposing (Html)
import Color exposing (Color)
import Html.Attributes exposing (width, height, style)
import Window
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Meshes exposing (Vertex, circleMesh, discMesh)
import SolarSystem exposing (..)
import Types exposing (..)


render : Window.Size -> SolarSystem -> Html msg
render window solarSystem =
    WebGL.toHtml
        [ width window.width
        , height window.height
        , style [ ( "display", "block" ) ]
        ]
        (let
            perspective =
                Mat4.makePerspective 45 (toFloat window.width / toFloat window.height) 0.01 100

            cameraPosition =
                vec3 0 0 5

            camera =
                Mat4.makeLookAt cameraPosition (vec3 0 0 0) (vec3 0 1 0)

            cameraMatrix =
                Mat4.mul perspective camera

            toEntity : Body -> List WebGL.Entity
            toEntity body =
                let
                    ( bodyMesh, bodyMatrix ) =
                        bodyToEntity body cameraPosition

                    trajectoryEntity =
                        trajectoryToEntity body
                in
                    case trajectoryEntity of
                        Nothing ->
                            [ WebGL.entity
                                vertexShader
                                fragmentShader
                                bodyMesh
                                { matrix = Mat4.mul cameraMatrix bodyMatrix }
                            ]

                        Just ( trajectoryMesh, trajectoryMatrix ) ->
                            [ WebGL.entity
                                vertexShader
                                fragmentShader
                                bodyMesh
                                { matrix = Mat4.mul cameraMatrix bodyMatrix }
                            , WebGL.entity
                                vertexShader
                                fragmentShader
                                trajectoryMesh
                                { matrix = Mat4.mul cameraMatrix trajectoryMatrix }
                            ]
         in
            List.concat (SolarSystem.visit toEntity solarSystem)
        )


type alias Uniforms =
    { matrix : Mat4 }


color : Body -> Vec3
color body =
    let
        c =
            Color.toRgb
                (case body.name of
                    "sun" ->
                        Color.yellow

                    "mercury" ->
                        Color.gray

                    "venus" ->
                        Color.orange

                    "earth" ->
                        Color.blue

                    _ ->
                        Color.gray
                )
    in
        vec3
            (toFloat c.red / 255)
            (toFloat c.green / 255)
            (toFloat c.blue / 255)


bodyToEntity : Body -> Vec3 -> ( Mesh Vertex, Mat4 )
bodyToEntity body cameraPosition =
    let
        position =
            body.orbit.derived.position

        scale =
            (max (0.005 * (Vec3.distance cameraPosition position)) body.constants.radius) / body.constants.radius

        matrix =
            List.foldl Mat4.mul
                Mat4.identity
                [ Mat4.makeScale3 scale scale scale
                , Mat4.makeTranslate
                    position
                ]
    in
        ( discMesh 50 body.constants.radius (color body), matrix )


trajectoryToEntity : Body -> Maybe ( Mesh Vertex, Mat4 )
trajectoryToEntity body =
    case body.orbit.parameters of
        ------------------------
        Stationary ->
            Nothing

        ------------------------
        Elliptic elements ->
            let
                matrix =
                    List.foldl Mat4.mul
                        Mat4.identity
                        [ Mat4.makeScale3
                            body.orbit.derived.semiMajorAxis
                            body.orbit.derived.semiMinorAxis
                            1
                        , Mat4.makeRotate elements.argumentPerihelion Vec3.k
                        , Mat4.makeRotate elements.inclination Vec3.i
                        , Mat4.makeRotate elements.longitudeAscendingNode Vec3.k
                        , Mat4.makeTranslate body.orbit.derived.center
                        ]
            in
                Just ( circleMesh 50 1 (color body), matrix )



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 matrix;
        varying vec3 vcolor;
        void main () {
            gl_Position = matrix * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
