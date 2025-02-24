module Viewer exposing (main)

{-| This example demonstrates how to load a mesh from a file.
It can also be used to test the parser :-)

Now try dragging and dropping some OBJ files from <http://people.math.sc.edu/Burkardt/data/obj/obj.html>!

-}

import Angle exposing (Angle)
import Array
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d
import File exposing (File)
import File.Select
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length exposing (Meters)
import Mtl.Decode
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material exposing (Material, Texture)
import Scene3d.Mesh exposing (Textured, Uniform)
import Task
import TriangularMesh exposing (TriangularMesh)
import Viewpoint3d
import WebGL.Texture exposing (Error(..))


type alias MeshWithMaterial =
    ( String, ViewMesh )


type ViewMesh
    = TexturedMesh (Textured ObjCoordinates)
    | UniformMesh (Uniform ObjCoordinates)


{-| Because we don’t know the exect format of a mesh, we try decoding different
primitives: from the most specific to the most simple one.
-}
meshWithBoundingBoxDecoder : Decoder ( List MeshWithMaterial, BoundingBox3d Meters ObjCoordinates )
meshWithBoundingBoxDecoder =
    Obj.Decode.oneOf
        [ withBoundingBox .position (Scene3d.Mesh.texturedFaces >> TexturedMesh) Obj.Decode.texturedFaces
        , withBoundingBox .position (Scene3d.Mesh.indexedFaces >> UniformMesh) Obj.Decode.faces
        , withBoundingBox .position (Scene3d.Mesh.texturedFacets >> TexturedMesh) Obj.Decode.texturedTriangles
        , withBoundingBox identity (Scene3d.Mesh.indexedFacets >> UniformMesh) Obj.Decode.triangles
        ]


trianglesForMaterials : Decoder a -> List String -> Decoder (List ( String, a ))
trianglesForMaterials trianglesDecoder names =
    names
        |> List.map
            (\materialName ->
                Obj.Decode.material materialName trianglesDecoder
                    |> Obj.Decode.map (\mesh -> ( materialName, mesh ))
            )
        |> Obj.Decode.combine


withMaterials : Decoder a -> Decoder (List ( String, a ))
withMaterials trianglesDecoder =
    Obj.Decode.materialNames
        |> Obj.Decode.andThen (trianglesForMaterials trianglesDecoder)


withBoundingBox :
    (a -> Point3d Meters ObjCoordinates) -- a function that knows how to extract position of a vertex
    -> (TriangularMesh a -> ViewMesh) -- a function that knows how to create a ViewMesh
    -> Decoder (TriangularMesh a) -- a primitive decoder
    -> Decoder ( List MeshWithMaterial, BoundingBox3d Meters ObjCoordinates )
withBoundingBox getPosition createMesh meshDecoder =
    meshDecoder
        |> withMaterials
        |> Obj.Decode.map
            (List.foldl
                (\( material, triangularMesh ) ( meshes, boundingBox ) ->
                    ( ( material, createMesh triangularMesh ) :: meshes
                    , BoundingBox3d.union boundingBox
                        (case List.map getPosition (Array.toList (TriangularMesh.vertices triangularMesh)) of
                            first :: rest ->
                                BoundingBox3d.hull first rest

                            [] ->
                                BoundingBox3d.singleton Point3d.origin
                        )
                    )
                )
                ( [], BoundingBox3d.singleton Point3d.origin )
            )


type LoadState a
    = Empty
    | Loaded a
    | Error String


type alias Model =
    { materials : LoadState (Dict String Color)
    , texture : LoadState (Texture Color)
    , meshWithBoundingBox : LoadState ( List MeshWithMaterial, BoundingBox3d Meters ObjCoordinates )
    , hover : Bool
    , azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , orbiting : Bool
    }


type Msg
    = PickClicked
    | ResetClicked
    | DragEnter
    | DragLeave
    | LoadedMaterials (Result String (Dict String Color))
    | LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMeshes (Result String ( List MeshWithMaterial, BoundingBox3d Meters ObjCoordinates ))
    | GotFiles File (List File)
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseWheel Float


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { materials = Empty
                  , texture = Empty
                  , meshWithBoundingBox = Empty
                  , hover = False
                  , azimuth = Angle.degrees -45
                  , elevation = Angle.degrees 35
                  , zoom = 0
                  , orbiting = False
                  }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
            ]

    else
        Browser.Events.onMouseDown (Json.Decode.succeed MouseDown)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetClicked ->
            ( { model | texture = Empty, meshWithBoundingBox = Empty }, Cmd.none )

        PickClicked ->
            ( model, File.Select.files [] GotFiles )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        GotFiles file files ->
            let
                ( imageFiles, objAndMtlFiles ) =
                    List.partition
                        (File.mime >> String.startsWith "image/")
                        (file :: files)

                loadTextureCmd =
                    case imageFiles of
                        textureFile :: _ ->
                            File.toUrl textureFile
                                |> Task.andThen
                                    (Scene3d.Material.loadWith
                                        Scene3d.Material.nearestNeighborFiltering
                                    )
                                |> Task.attempt LoadedTexture

                        [] ->
                            Cmd.none

                ( mtlFiles, objFiles ) =
                    List.partition
                        (File.name >> String.endsWith ".mtl")
                        objAndMtlFiles

                loadAndDecodeMeshCmd =
                    case objFiles of
                        objFile :: _ ->
                            File.toString objFile
                                |> Task.andThen
                                    (\string ->
                                        case
                                            Obj.Decode.decodeString
                                                Length.meters
                                                meshWithBoundingBoxDecoder
                                                string
                                        of
                                            Ok m ->
                                                Task.succeed m

                                            Err err ->
                                                Task.fail err
                                    )
                                |> Task.attempt LoadedMeshes

                        [] ->
                            Cmd.none

                loadMaterialsCmd =
                    case mtlFiles of
                        mtlFile :: _ ->
                            File.toString mtlFile
                                |> Task.andThen
                                    (\string ->
                                        case
                                            Mtl.Decode.decodeString
                                                { createMaterial = createMaterialFromString }
                                                string
                                        of
                                            Ok m ->
                                                Task.succeed m

                                            Err err ->
                                                Task.fail err
                                    )
                                |> Task.attempt LoadedMaterials

                        [] ->
                            Cmd.none
            in
            ( { model | hover = False }, Cmd.batch [ loadMaterialsCmd, loadTextureCmd, loadAndDecodeMeshCmd ] )

        LoadedMeshes result ->
            ( { model
                | meshWithBoundingBox =
                    case result of
                        Err err ->
                            Error err

                        Ok m ->
                            Loaded m
              }
            , Cmd.none
            )

        LoadedMaterials result ->
            ( { model
                | materials =
                    case result of
                        Err err ->
                            Error err

                        Ok m ->
                            Loaded m
              }
            , Cmd.none
            )

        LoadedTexture result ->
            ( { model
                | texture =
                    case result of
                        Err LoadError ->
                            Error "Texture load error"

                        Err (SizeError _ _) ->
                            Error "Texture size error"

                        Ok texture ->
                            Loaded texture
              }
            , Cmd.none
            )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Quantity.per Pixels.pixel (Angle.degrees 1)
                in
                ( { model
                    | azimuth =
                        model.azimuth
                            |> Quantity.minus (Quantity.at rotationRate dx)
                    , elevation =
                        model.elevation
                            |> Quantity.plus (Quantity.at rotationRate dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseWheel deltaY ->
            ( { model | zoom = clamp 0 1 (model.zoom - deltaY * 0.002) }, Cmd.none )


createMaterialFromString : String -> List String -> Maybe Color -> Maybe Color
createMaterialFromString name parameters maybeColor =
    case parameters of
        "Kd" :: values ->
            case List.map String.toFloat values of
                [ Just red, Just green, Just blue ] ->
                    Just <| Color.rgb red green blue

                _ ->
                    maybeColor

        [ "D", value ] ->
            case ( maybeColor |> Maybe.map Color.toRgba, String.toFloat value ) of
                ( Just { red, green, blue }, Just alpha ) ->
                    Just <| Color.rgba red green blue alpha

                _ ->
                    maybeColor

        _ ->
            maybeColor


meshView : Camera3d Meters ObjCoordinates -> LoadState (Dict String Color) -> LoadState (Texture Color) -> List MeshWithMaterial -> Html Msg
meshView camera loadingMaterials loadingTexture meshes =
    let
        entities =
            meshes
                |> List.map entity

        entity ( materialName, mesh ) =
            case mesh of
                TexturedMesh texturedMesh ->
                    case loadingTexture of
                        Loaded texture ->
                            Scene3d.mesh (Scene3d.Material.texturedMatte texture) texturedMesh

                        Error _ ->
                            Scene3d.mesh (Scene3d.Material.matte Color.red) texturedMesh

                        _ ->
                            Scene3d.mesh (Scene3d.Material.matte Color.blue) texturedMesh

                UniformMesh uniformMesh ->
                    let
                        color =
                            case loadingMaterials of
                                Loaded materials ->
                                    Dict.get materialName materials
                                        |> Maybe.withDefault Color.red

                                _ ->
                                    case materialName of
                                        "Black" ->
                                            Color.black

                                        "Blue" ->
                                            Color.blue

                                        "Green" ->
                                            Color.green

                                        _ ->
                                            Color.red
                    in
                    Scene3d.mesh (Scene3d.Material.nonmetal { baseColor = color, roughness = 0.2 }) uniformMesh
    in
    Scene3d.sunny
        { upDirection = Direction3d.z
        , sunlightDirection = Direction3d.negativeZ
        , shadows = False
        , camera = camera
        , dimensions = ( Pixels.int 640, Pixels.int 640 )
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.1
        , entities = entities
        }


view : Model -> Html Msg
view model =
    centeredContents
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , hijackOn "dragenter" (Json.Decode.succeed DragEnter)
        , hijackOn "dragover" (Json.Decode.succeed DragEnter)
        , hijackOn "dragleave" (Json.Decode.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        , Html.Events.preventDefaultOn "wheel"
            (Json.Decode.map
                (\deltaY -> ( MouseWheel deltaY, True ))
                (Json.Decode.field "deltaY" Json.Decode.float)
            )
        ]
        [ centeredContents
            [ Html.Attributes.style "border"
                (case ( model.hover, model.meshWithBoundingBox ) of
                    ( True, _ ) ->
                        "3px dashed green"

                    ( False, Loaded _ ) ->
                        "3px dashed rgb(52, 101, 164)"

                    ( False, Error _ ) ->
                        "3px dashed red"

                    _ ->
                        "3px dashed #ccc"
                )
            , Html.Attributes.style "width" "640px"
            , Html.Attributes.style "height" "640px"
            , Html.Attributes.style "position" "relative"
            ]
            (case model.meshWithBoundingBox of
                Empty ->
                    [ Html.button [ Html.Events.onClick PickClicked ]
                        [ Html.text "select an OBJ file, an MTL file and/or an image" ]
                    ]

                Error err ->
                    [ Html.p [ Html.Attributes.style "color" "red" ]
                        [ Html.text err ]
                    , Html.button [ Html.Events.onClick PickClicked ]
                        [ Html.text "try a different OBJ file" ]
                    ]

                Loaded ( meshes, boundingBox ) ->
                    let
                        { minX, maxX, minY, maxY, minZ, maxZ } =
                            BoundingBox3d.extrema boundingBox

                        distance =
                            List.map Quantity.abs [ minX, maxX, minY, maxY, minZ, maxZ ]
                                |> List.foldl Quantity.max Quantity.zero
                                |> Quantity.multiplyBy 2
                                |> Quantity.multiplyBy (2 - model.zoom)

                        camera =
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.orbitZ
                                        { focalPoint = BoundingBox3d.centerPoint boundingBox
                                        , azimuth = model.azimuth
                                        , elevation = model.elevation
                                        , distance = distance
                                        }
                                , verticalFieldOfView = Angle.degrees 30
                                }
                    in
                    [ Html.div []
                        [ meshView camera model.materials model.texture meshes ]
                    , Html.button
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "right" "10px"
                        , Html.Attributes.style "top" "10px"
                        , Html.Events.onClick ResetClicked
                        ]
                        [ Html.text "close" ]
                    ]
            )
        ]


centeredContents : List (Attribute msg) -> List (Html msg) -> Html msg
centeredContents attributes =
    Html.div
        ([ Html.Attributes.style "align-items" "center"
         , Html.Attributes.style "justify-content" "center"
         , Html.Attributes.style "display" "flex"
         , Html.Attributes.style "flex-direction" "column"
         ]
            ++ attributes
        )


dropDecoder : Json.Decode.Decoder Msg
dropDecoder =
    Json.Decode.at [ "dataTransfer", "files" ]
        (Json.Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Json.Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    Html.Events.preventDefaultOn event
        (Json.Decode.map (\val -> ( val, True )) decoder)


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "movementX" (Json.Decode.map Pixels.float Json.Decode.float))
        (Json.Decode.field "movementY" (Json.Decode.map Pixels.float Json.Decode.float))
