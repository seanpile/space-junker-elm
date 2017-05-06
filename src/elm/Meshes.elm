module Meshes exposing (Vertex, circle, disc, sphere)

import Color exposing (Color)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


sphere : Float -> Int -> Int -> Vec3 -> Mesh Vertex
sphere radius widthSegments heightSegments color =
    let
        -- Generate all of the vertices by ranging over height & width segments, using
        -- spherical coordinates to map to cartesian (x, y, z)
        vertices =
            (List.range 0 ((heightSegments * widthSegments) - 1))
                |> List.map
                    (\idx ->
                        let
                            thetaIdx =
                                idx // heightSegments

                            phiIdx =
                                idx % heightSegments

                            theta =
                                toFloat thetaIdx * (2 * pi / (toFloat widthSegments - 1))

                            phi =
                                toFloat phiIdx * (2 * pi / (toFloat heightSegments - 1))
                        in
                            { color = color
                            , position =
                                Vector3.fromTuple
                                    ( radius * cos (theta) * sin (phi)
                                    , radius * sin (theta) * sin (phi)
                                    , radius * cos (phi)
                                    )
                            }
                    )
    in
        -- After generating the vertices, we need to go an construct the triangles that form
        -- the faces of this mesh
        WebGL.indexedTriangles vertices
            (List.range 0 ((heightSegments * widthSegments) - 1)
                |> List.map
                    (\idx ->
                        let
                            thetaIdx =
                                idx // heightSegments

                            phiIdx =
                                idx % heightSegments
                        in
                            if phiIdx == 0 then
                                []
                            else if phiIdx == 1 then
                                -- The bottom segments is a single triangle
                                [ ( thetaIdx * heightSegments + 1
                                  , thetaIdx * heightSegments
                                  , ((thetaIdx + 1) % widthSegments) * heightSegments + 1
                                  )
                                ]
                            else if phiIdx == (heightSegments - 1) then
                                -- The top segment is a single triangle
                                [ ( thetaIdx * heightSegments + phiIdx
                                  , thetaIdx * heightSegments + phiIdx - 1
                                  , ((thetaIdx + 1) % widthSegments) * heightSegments + phiIdx - 1
                                  )
                                ]
                            else
                                -- Do two triangles
                                [ ( thetaIdx * heightSegments + phiIdx
                                  , thetaIdx * heightSegments + phiIdx - 1
                                  , ((thetaIdx + 1) % widthSegments) * heightSegments + phiIdx - 1
                                  )
                                , ( thetaIdx * heightSegments + phiIdx
                                  , ((thetaIdx + 1) % widthSegments) * heightSegments + phiIdx
                                  , ((thetaIdx + 1) % widthSegments) * heightSegments + phiIdx - 1
                                  )
                                ]
                    )
                |> List.concat
            )


disc : Int -> Float -> Vec3 -> Mesh Vertex
disc segments radius color =
    (List.range 0 (segments - 1))
        |> List.map (\idx -> (toFloat idx) * ((pi * 2) / (toFloat segments)))
        |> List.map
            (\angle ->
                { color = color
                , position = Vector3.fromTuple ( radius * cos angle, radius * sin angle, 0 )
                }
            )
        -- Place origin at the head of the list
        |> (::) { color = color, position = vec3 0 0 0 }
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


circle : Int -> Float -> Vec3 -> Mesh Vertex
circle segments radius color =
    (List.range 0 (segments - 1))
        |> List.map (\idx -> (toFloat idx) * ((pi * 2) / (toFloat segments)))
        |> List.map
            (\angle ->
                { color = color
                , position = Vector3.fromTuple ( radius * cos angle, radius * sin angle, 0 )
                }
            )
        |> WebGL.lineLoop
