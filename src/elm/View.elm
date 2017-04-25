module View exposing (..)

import Html exposing (Html)
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

            camera =
                Mat4.makeLookAt (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)

            uniforms =
                { matrix = Mat4.identity
                }

            toEntity : Body -> WebGL.Entity
            toEntity body =
                let
                    matrix =
                        List.foldl Mat4.mul
                            uniforms.matrix
                            [ Mat4.makeTranslate body.orbit.derived.position
                            , camera
                            , perspective
                            ]
                in
                    WebGL.entity
                        vertexShader
                        fragmentShader
                        (discMesh 50 0.5)
                        { uniforms | matrix = matrix }
         in
            SolarSystem.visit toEntity solarSystem
        )


type alias Uniforms =
    { matrix : Mat4 }



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
