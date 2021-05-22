module Encoding exposing
    ( faces
    , options
    , points
    , polylines
    , texturedFaces
    , texturedTriangles
    , triangles
    )

import Array
import Expect
import Length exposing (Meters)
import Obj.Encode as Encode exposing (Options, defaultOptions)
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Unitless)
import Test exposing (Test)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


triangles : Test
triangles =
    Test.describe "triangles"
        [ Test.test "correctly writes positions and indices" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.triangles trianglesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "f 1 2 3\n"
                            , "f 1 3 4\n"
                            ]
                        )
        ]


faces : Test
faces =
    Test.describe "faces"
        [ Test.test "correctly writes positions, normal vectors and indices" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.faces texturedFacesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "f 1//1 2//2 3//3\n"
                            , "f 1//1 3//3 4//4\n"
                            ]
                        )
        ]


texturedTriangles : Test
texturedTriangles =
    Test.describe "texturedTriangles"
        [ Test.test "correctly writes positions, UV and indices" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.texturedTriangles texturedFacesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "f 1/1 2/2 3/3\n"
                            , "f 1/1 3/3 4/4\n"
                            ]
                        )
        ]


texturedFaces : Test
texturedFaces =
    Test.describe "texturedFaces"
        [ Test.test "correctly writes positions, UV and indices" <|
            \_ ->
                Encode.encode Length.inCentimeters (Encode.texturedFaces texturedFacesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -450.000000 450.000000 0.000000\n"
                            , "v 450.000000 450.000000 0.000000\n"
                            , "v 450.000000 -450.000000 0.000000\n"
                            , "v -450.000000 -450.000000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "f 1/1/1 2/2/2 3/3/3\n"
                            , "f 1/1/1 3/3/3 4/4/4\n"
                            ]
                        )
        ]


points : Test
points =
    Test.describe "points"
        [ Test.test "correctly writes positions and indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.points
                        [ Point3d.meters -4.5 4.5 0
                        , Point3d.meters 4.5 4.5 0
                        , Point3d.meters 4.5 -4.5 0
                        , Point3d.meters -4.5 -4.5 0
                        ]
                    )
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "p 1\n"
                            , "p 2\n"
                            , "p 3\n"
                            , "p 4\n"
                            ]
                        )
        , Test.test "skips empty points" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.points [])
                    |> Expect.equal ""
        ]


polylines : Test
polylines =
    Test.describe "polylines"
        [ Test.test "correctly writes positions and indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.polylines
                        [ Polyline3d.fromVertices
                            [ Point3d.meters -4.5 4.5 0
                            , Point3d.meters 4.5 4.5 0
                            ]
                        , Polyline3d.fromVertices
                            [ Point3d.meters 4.5 -4.5 0
                            , Point3d.meters -4.5 -4.5 0
                            ]
                        ]
                    )
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "l 1 2\n"
                            , "l 3 4\n"
                            ]
                        )
        , Test.test "skips empty polylines" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.polylines [])
                    |> Expect.equal ""
        , Test.test "skips polylines without vertices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.polylines [ Polyline3d.fromVertices [] ])
                    |> Expect.equal ""
        ]


options : Test
options =
    Test.describe "options"
        [ Test.test "respects the precision option" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.trianglesWith { defaultOptions | precision = 3 } precisionTriangle)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -5.000 0.000 0.123\n"
                            , "v -0.123 -1.123 0.000\n"
                            , "v 0.000 0.000 0.000\n"
                            , "f 3 2 1\n"
                            ]
                        )
        , Test.test "clamps the precision to 1" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.trianglesWith { defaultOptions | precision = 0 } precisionTriangle)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -5.0 0.0 0.1\n"
                            , "v -0.1 -1.1 0.0\n"
                            , "v 0.0 0.0 0.0\n"
                            , "f 3 2 1\n"
                            ]
                        )
        , Test.test "clamps the precision to 10" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.trianglesWith { defaultOptions | precision = 20 } precisionTriangle)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -5.0000000000 0.0000000000 0.1234567890\n"
                            , "v -0.1234567890 -1.1234567890 0.0000000000\n"
                            , "v 0.0000000000 0.0000000000 0.0000000001\n"
                            , "f 3 2 1\n"
                            ]
                        )
        , Test.test "sanitizes metadata" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.pointsWith unsafeOptions [ Point3d.meters 1 2 3 ])
                    |> Expect.equal
                        (String.concat
                            [ "o ObjectName\n"
                            , "g Group1 Group2\n"
                            , "mtllib TestMaterial\n"
                            , "v 1.000000 2.000000 3.000000\n"
                            , "p 1\n"
                            ]
                        )
        ]



-- FIXTURES


unsafeOptions : Options
unsafeOptions =
    { precision = 6
    , object = Just "Object Name"
    , groups = [ "Group 1", "Group\n 2" ]
    , material = Just "Test Material"
    }


trianglesSquare : TriangularMesh (Point3d Meters coords)
trianglesSquare =
    let
        vertices =
            Array.fromList
                [ Point3d.meters -4.5 4.5 0
                , Point3d.meters 4.5 4.5 0
                , Point3d.meters 4.5 -4.5 0
                , Point3d.meters -4.5 -4.5 0
                ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


texturedFacesSquare : TriangularMesh { position : Point3d Meters coords, normal : Vector3d Unitless coords, uv : ( Float, Float ) }
texturedFacesSquare =
    let
        vertices =
            Array.fromList
                [ { position = Point3d.meters -4.5 4.5 0, uv = ( 0, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 4.5 0, uv = ( 1, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 -4.5 0, uv = ( 1, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -4.5 -4.5 0, uv = ( 0, 0 ), normal = Vector3d.unitless 0 0 1 }
                ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


precisionTriangle : TriangularMesh (Point3d Meters coords)
precisionTriangle =
    let
        vertices =
            Array.fromList
                [ Point3d.meters -5 0 0.123456789
                , Point3d.meters -0.123456789 -1.123456789 0
                , Point3d.meters (0 / 0) (1 / 0) 1.23456789e-10
                ]
    in
    TriangularMesh.indexed vertices [ ( 2, 1, 0 ) ]
