module Viewer exposing (main)

{-| This example demonstrates how to load a mesh from a file.
It can also be used to test the parser :-)

Now try dragging and dropping some OBJ files from <http://people.math.sc.edu/Burkardt/data/obj/obj.html>!

-}

import Angle exposing (Angle)
import Array
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Dom
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
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
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
import WebGL exposing (Mesh, Shader)
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
    , width : Float
    , height : Float
    , time : Float
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
    | Resize Float Float
    | TimeDiff Float


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
                  , width = 0
                  , height = 0
                  , time = 0
                  }
                , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) Browser.Dom.getViewport
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta TimeDiff
        , Browser.Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove decodeMouseMove
                , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Json.Decode.succeed MouseDown)
        ]


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

        Resize width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )

        TimeDiff time ->
            ( { model | time = model.time + time }
            , Cmd.none
            )


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
                    [ WebGL.toHtml
                        [ Html.Attributes.width 640
                        , Html.Attributes.height 640
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "left" "0"
                        , Html.Attributes.style "top" "0"
                        , Html.Attributes.style "display" "block"
                        , Html.Attributes.style "z-index" "1"
                        ]
                        [ WebGL.entity
                            vertexShader
                            fragmentShader
                            quad
                            { iResolution = vec3 480 640 0
                            , iGlobalTime = model.time / 1000
                            , iMouse = vec4 (Angle.inRadians model.azimuth) (Angle.inRadians model.elevation) 0 0
                            }
                        ]
                    , Html.div
                        [ Html.Attributes.style "z-index" "2" ]
                        [ meshView camera model.materials model.texture meshes ]
                    , Html.button
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "right" "10px"
                        , Html.Attributes.style "top" "10px"
                        , Html.Attributes.style "z-index" "3"
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



-- Mesh


quad : Mesh { position : Vec3 }
quad =
    WebGL.triangles
        [ ( { position = vec3 -1 1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 -1 -1 0 }
          )
        , ( { position = vec3 -1 -1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]



-- Shaders


type alias Uniforms =
    { iResolution : Vec3
    , iGlobalTime : Float
    , iMouse : Vec4
    }


vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        varying vec2 vFragCoord;
        uniform vec3 iResolution;
        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0 * iResolution.xy;
        }
    |]


fragmentShader : Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2      vFragCoord;
        uniform vec3      iResolution;           // viewport resolution (in pixels)
        uniform float     iGlobalTime;           // shader playback time (in seconds)
        uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
        /*
         * "Seascape" by Alexander Alekseev aka TDM - 2014
         * License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
         * Contact: tdmaav@gmail.com
         */
        const int NUM_STEPS = 8;
        const float PI = 3.1415;
        const float EPSILON = 1e-3;
        // sea
        const int ITER_GEOMETRY = 3;
        const int ITER_FRAGMENT = 5;
        const float SEA_HEIGHT = 0.6;
        const float SEA_CHOPPY = 4.0;
        const float SEA_SPEED = 0.8;
        const float SEA_FREQ = 0.16;
        const vec3 SEA_BASE = vec3(0.1,0.19,0.22);
        const vec3 SEA_WATER_COLOR = vec3(0.8,0.9,0.6);
        const mat2 octave_m = mat2(1.6,1.2,-1.2,1.6);
        // math
        mat3 fromEuler(vec3 ang) {
                vec2 a1 = vec2(sin(ang.x),cos(ang.x));
                vec2 a2 = vec2(sin(ang.y),cos(ang.y));
                vec2 a3 = vec2(sin(ang.z),cos(ang.z));
                mat3 m;
                m[0] = vec3(a1.y*a3.y+a1.x*a2.x*a3.x,a1.y*a2.x*a3.x+a3.y*a1.x,-a2.y*a3.x);
                m[1] = vec3(-a2.y*a1.x,a1.y*a2.y,a2.x);
                m[2] = vec3(a3.y*a1.x*a2.x+a1.y*a3.x,a1.x*a3.x-a1.y*a3.y*a2.x,a2.y*a3.y);
                return m;
            }
        float hash( vec2 p ) {
                float h = dot(p,vec2(127.1,311.7));
                return fract(sin(h)*43758.5453123);
            }
        float noise( in vec2 p ) {
                vec2 i = floor( p );
                vec2 f = fract( p );
                vec2 u = f*f*(3.0-2.0*f);
                return -1.0 + 2.0 *
                    mix(
                        mix(
                            hash(i + vec2(0.0,0.0)),
                            hash(i + vec2(1.0,0.0)),
                            u.x
                           ),
                        mix(
                            hash(i + vec2(0.0,1.0)),
                            hash(i + vec2(1.0,1.0)),
                            u.x
                           ),
                        u.y
                       );
            }

        // lighting
        float diffuse(vec3 n,vec3 l,float p) {
            return pow(dot(n,l) * 0.4 + 0.6,p);
        }
        float specular(vec3 n,vec3 l,vec3 e,float s) {
            float nrm = (s + 8.0) / (3.1415 * 8.0);
            return pow(max(dot(reflect(e,n),l),0.0),s) * nrm;
        }
        // sky
        vec3 getSkyColor(vec3 e) {
            e.y = max(e.y,0.0);
            return vec3(pow(1.0-e.y,2.0), 1.0-e.y, 0.6+(1.0-e.y)*0.4);
        }
        // sea
        float sea_octave(vec2 uv, float choppy) {
            uv += noise(uv);
            vec2 wv = 1.0-abs(sin(uv));
            vec2 swv = abs(cos(uv));
            wv = mix(wv,swv,wv);
            return pow(1.0-pow(wv.x * wv.y,0.65),choppy);
        }
        float map(vec3 p) {
            float SEA_TIME = 1.0 + iGlobalTime * SEA_SPEED;
            float freq = SEA_FREQ;
            float amp = SEA_HEIGHT;
            float choppy = SEA_CHOPPY;
            vec2 uv = p.xz; uv.x *= 0.75;
            float d, h = 0.0;
            for(int i = 0; i < ITER_GEOMETRY; i++) {
                    d = sea_octave((uv+SEA_TIME)*freq,choppy);
                    d += sea_octave((uv-SEA_TIME)*freq,choppy);
                    h += d * amp;
                    uv *= octave_m; freq *= 1.9; amp *= 0.22;
                    choppy = mix(choppy,1.0,0.2);
                }
            return p.y - h;
        }
        float map_detailed(vec3 p) {
                float SEA_TIME = 1.0 + iGlobalTime * SEA_SPEED;
                float freq = SEA_FREQ;
                float amp = SEA_HEIGHT;
                float choppy = SEA_CHOPPY;
                vec2 uv = p.xz; uv.x *= 0.75;
                float d, h = 0.0;
                for(int i = 0; i < ITER_FRAGMENT; i++) {
                    d = sea_octave((uv+SEA_TIME)*freq,choppy);
                    d += sea_octave((uv-SEA_TIME)*freq,choppy);
                    h += d * amp;
                    uv *= octave_m; freq *= 1.9; amp *= 0.22;
                    choppy = mix(choppy,1.0,0.2);
                }
                return p.y - h;
            }
        vec3 getSeaColor(vec3 p, vec3 n, vec3 l, vec3 eye, vec3 dist) {
            float fresnel = clamp(1.0 - dot(n,-eye), 0.0, 1.0);
            fresnel = pow(fresnel,3.0) * 0.65;
            vec3 reflected = getSkyColor(reflect(eye,n));
            vec3 refracted = SEA_BASE + diffuse(n,l,80.0) * SEA_WATER_COLOR * 0.12;
            vec3 color = mix(refracted,reflected,fresnel);
            float atten = max(1.0 - dot(dist,dist) * 0.001, 0.0);
            color += SEA_WATER_COLOR * (p.y - SEA_HEIGHT) * 0.18 * atten;
            color += vec3(specular(n,l,eye,60.0));
            return color;
        }
        // tracing
        vec3 getNormal(vec3 p, float eps) {
            vec3 n;
            n.y = map_detailed(p);
            n.x = map_detailed(vec3(p.x+eps,p.y,p.z)) - n.y;
            n.z = map_detailed(vec3(p.x,p.y,p.z+eps)) - n.y;
            n.y = eps;
            return normalize(n);
        }
        float heightMapTracing(vec3 ori, vec3 dir, out vec3 p) {
            float tm = 0.0;
            float tx = 1000.0;
            float hx = map(ori + dir * tx);
            if(hx > 0.0) return tx;
            float hm = map(ori + dir * tm);
            float tmid = 0.0;
            for(int i = 0; i < NUM_STEPS; i++) {
                tmid = mix(tm,tx, hm/(hm-hx));
                p = ori + dir * tmid;
                float hmid = map(p);
                if (hmid < 0.0) {
                    tx = tmid;
                    hx = hmid;
                } else {
                    tm = tmid;
                    hm = hmid;
                }
            }
            return tmid;
        }
        // main
        void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
            vec2 uv = fragCoord.xy / iResolution.xy;
            uv = uv * 2.0 - 1.0;
            uv.x *= iResolution.x / iResolution.y;
            float time = iGlobalTime * 0.3;
            // ray
            vec3 ang = vec3(sin(iMouse.x),sin(iMouse.y),time);
            vec3 ori = vec3(0.0,3.5,0.0); //speed? time*5.0);
            vec3 dir = normalize(vec3(uv.xy,-2.0)); dir.z += length(uv) * 0.15;
            dir = normalize(dir) * fromEuler(ang);
            // tracing
            vec3 p;
            heightMapTracing(ori,dir,p);
            vec3 dist = p - ori;
            float EPSILON_NRM = 0.1 / iResolution.x;
            vec3 n = getNormal(p, dot(dist,dist) * EPSILON_NRM);
            vec3 light = normalize(vec3(0.0,1.0,0.8));
            // color
            vec3 color = mix(
                getSkyColor(dir),
                getSeaColor(p,n,light,dir,dist),
                pow(smoothstep(0.0,-0.05,dir.y),0.3)
            );
            // post
            fragColor = vec4(pow(color,vec3(0.75)), 1.0);
        }
        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }
    |]
