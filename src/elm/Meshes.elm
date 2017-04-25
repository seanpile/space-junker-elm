module Meshes exposing (Vertex, circleMesh, discMesh)

import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


discMesh : Int -> Float -> Mesh Vertex
discMesh segments radius =
    (List.range 0 (segments - 1))
        |> List.map (\idx -> (toFloat idx) * ((pi * 2) / (toFloat segments)))
        |> List.map
            (\angle ->
                { color = vec3 0 0 0
                , position = Vector3.fromTuple ( radius * cos angle, radius * sin angle, 0 )
                }
            )
        |> (::) { color = vec3 0 0 0, position = vec3 0 0 0 }
        |> flip WebGL.indexedTriangles
            ((List.range 0 (segments - 1))
                |> List.map
                    (\idx ->
                        ( 0
                        , idx + 1
                        , if idx < segments - 1 then
                            idx + 2
                          else
                            1
                        )
                    )
            )


circleMesh : Int -> Float -> Mesh Vertex
circleMesh segments radius =
    (List.range 0 (segments - 1))
        |> List.map (\idx -> (toFloat idx) * ((pi * 2) / (toFloat segments)))
        |> List.map
            (\angle ->
                { color = vec3 0 0 0
                , position = Vector3.fromTuple ( radius * cos angle, radius * sin angle, 0 )
                }
            )
        |> WebGL.lineLoop
