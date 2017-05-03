module View exposing (RenderingContext, init, render)

import Dict exposing (Dict)
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


type alias RenderingContext =
    { camera : Mat4
    , window : Window.Size
    , meshes : Dict String (Mesh Vertex)
    }


emptyMesh : Mesh Vertex
emptyMesh =
    WebGL.points [ { color = vec3 0 0 0, position = vec3 0 0 0 } ]


init : Window.Size -> SolarSystem -> RenderingContext
init window solarSystem =
    let
        -- Precompute Meshes on init to avoid this costly operation each time we render
        toMesh : Body -> List ( String, Mesh Vertex )
        toMesh body =
            [ ( body.name, (discMesh 50 body.constants.radius (color body)) )
            , ( body.name ++ "-trajectory"
              , circleMesh 50 1 (color body)
              )
            ]

        meshes : Dict String (Mesh Vertex)
        meshes =
            Dict.fromList (List.concat (SolarSystem.forEach toMesh solarSystem))
    in
        { camera =
            Mat4.makePerspective 45 (toFloat window.width / toFloat window.height) 0.01 100
        , window = window
        , meshes = meshes
        }


render : RenderingContext -> SolarSystem -> Html msg
render context solarSystem =
    WebGL.toHtml
        [ width context.window.width
        , height context.window.height
        , style [ ( "display", "block" ), ( "background-color", "black" ) ]
        ]
        (let
            cameraPosition =
                vec3 0 0 5

            camera =
                Mat4.makeLookAt cameraPosition (vec3 0 0 0) (vec3 0 1 0)

            cameraMatrix =
                Mat4.mul context.camera camera

            -- Mapping function that will convert each body into something we can pass to WebGL
            toEntity : Body -> List WebGL.Entity
            toEntity body =
                let
                    bodyMesh =
                        Maybe.withDefault emptyMesh (Dict.get body.name context.meshes)

                    trajectoryMesh =
                        Maybe.withDefault emptyMesh (Dict.get (body.name ++ "-trajectory") context.meshes)
                in
                    case (trajectoryMatrix body) of
                        Nothing ->
                            [ WebGL.entity
                                vertexShader
                                fragmentShader
                                bodyMesh
                                { matrix = Mat4.mul cameraMatrix (bodyMatrix body cameraPosition) }
                            ]

                        Just trajectoryMatrix ->
                            [ WebGL.entity
                                vertexShader
                                fragmentShader
                                bodyMesh
                                { matrix = Mat4.mul cameraMatrix (bodyMatrix body cameraPosition) }
                            , WebGL.entity
                                vertexShader
                                fragmentShader
                                trajectoryMesh
                                { matrix = Mat4.mul cameraMatrix trajectoryMatrix }
                            ]
         in
            List.concat (SolarSystem.forEach toEntity solarSystem)
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


bodyMatrix : Body -> Vec3 -> Mat4
bodyMatrix body cameraPosition =
    let
        position =
            body.orbit.derived.position

        scale =
            (max (0.005 * (Vec3.distance cameraPosition position)) body.constants.radius) / body.constants.radius
    in
        List.foldl Mat4.mul
            Mat4.identity
            [ Mat4.makeScale3 scale scale scale
            , Mat4.makeTranslate
                position
            ]


trajectoryMatrix : Body -> Maybe Mat4
trajectoryMatrix body =
    case body.orbit.parameters of
        ------------------------
        Stationary ->
            Nothing

        ------------------------
        Elliptic elements ->
            Just
                (List.foldl
                    Mat4.mul
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
                )



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
