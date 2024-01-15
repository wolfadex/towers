module LevelEditor exposing (..)

import AStar.Generalised
import Angle exposing (Angle)
import AngularSpeed exposing (AngularSpeed)
import Axis3d
import Basics.Extra
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Css
import Css.Color
import Dict
import Direction3d
import Duration exposing (Duration)
import Ecs
import Ecs.Component
import Ecs.Config
import Ecs.Entity
import Ecs.System
import Frame3d
import Hex exposing (Hex)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Length
import Level exposing (Level)
import LineSegment3d
import List.Extra
import Meshes exposing (Meshes)
import Obj.Decode
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Random
import Random.List
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set
import SketchPlane3d
import Task exposing (Task)
import Tea exposing (Tea)
import Time
import TriangularMesh exposing (TriangularMesh)
import Util.Dict
import Util.Ecs.Component
import Util.Maybe
import Util.Obj.Decode
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)



-----------
-- MODEL --
-----------


type Model
    = Initializing InitializingModel
    | InitializingFailed String
    | Ready ReadyModel


type alias InitializingModel =
    Meshes.Model WorldCoordinates Msg


type alias ReadyModel =
    { meshes : Meshes WorldCoordinates
    , lastTimestamp : Time.Posix
    , highlightedTile : Maybe Hex
    , cameraEyePoint : Point3d Length.Meters WorldCoordinates
    , cameraRotation : { current : Angle, destination : Angle }
    , level : Level
    }


type ScreenCoordinates
    = ScreenCoordinates Never


type WorldCoordinates
    = WorldCoordinates Never



----------
-- INIT --
----------


init : { toMsg : Msg -> msg, toModel : Model -> model } -> Tea model msg
init cfg =
    Meshes.init
        { onUpdates = MeshesLoading
        , onFailure = MeshesLoadFailed
        , onSuccess = MeshesLoaded
        }
        |> Tea.fromTuple
        |> Tea.mapModel Initializing
        |> Tea.map cfg



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : { toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions cfg model =
    case model of
        Initializing _ ->
            Sub.none

        InitializingFailed _ ->
            Sub.none

        Ready _ ->
            [ Browser.Events.onAnimationFrame Tick
            , Browser.Events.onKeyPress decodeKeyPressed
            ]
                |> Sub.batch
                |> Tea.mapSub cfg


decodeKeyPressed : Json.Decode.Decoder Msg
decodeKeyPressed =
    Json.Decode.map5
        (\key alt control shift meta ->
            KeyPressed
                { key = key
                , alt = alt
                , control = control
                , shift = shift
                , meta = meta
                }
        )
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


type alias Key =
    { key : String
    , alt : Bool
    , control : Bool
    , shift : Bool
    , meta : Bool
    }



------------
-- UPDATE --
------------


type Msg
    = Tick Time.Posix
    | KeyPressed Key
    | Clicked (Point2d Pixels ScreenCoordinates)
    | MouseMoved (Point2d Pixels ScreenCoordinates)
    | RotateCamera HandedDirection
      -- | TrapTypeSelected TrapType
    | MeshesLoading (Meshes.LoadingMsg WorldCoordinates)
    | MeshesLoadFailed Http.Error
    | MeshesLoaded (Meshes.RawMesh WorldCoordinates) (Meshes.RawMesh WorldCoordinates) (Meshes.RawMesh WorldCoordinates) (Meshes.EnemySphereRawMesh WorldCoordinates) (Meshes.WallRawMesh WorldCoordinates) (Meshes.ThingToProtectRawMesh WorldCoordinates)
    | TimeInitialized (Meshes WorldCoordinates) Time.Posix


type HandedDirection
    = Left
    | Right


update : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> Model -> Tea model msg
update cfg msg model =
    case model of
        Initializing model_ ->
            updateInitializing cfg msg model_

        InitializingFailed _ ->
            model
                |> Tea.save
                |> Tea.map cfg

        Ready model_ ->
            updateReady cfg msg model_


updateInitializing : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> InitializingModel -> Tea model msg
updateInitializing cfg msg model =
    Tea.map cfg <|
        case msg of
            MeshesLoading msg_ ->
                let
                    ( nextMeshLoadingModel, meshLoadingCmd ) =
                        Meshes.update msg_ model
                in
                nextMeshLoadingModel
                    |> Initializing
                    |> Tea.save
                    |> Tea.withCmd meshLoadingCmd

            MeshesLoadFailed error ->
                let
                    errorAsString : String
                    errorAsString =
                        case error of
                            Http.BadUrl url ->
                                "Invalid URL: " ++ url

                            Http.Timeout ->
                                "HTTP Timeout"

                            Http.NetworkError ->
                                "Network error"

                            Http.BadStatus status ->
                                "Bad HTTP status: " ++ String.fromInt status

                            Http.BadBody body ->
                                "Invalid mesh data: " ++ body
                in
                errorAsString
                    |> InitializingFailed
                    |> Tea.save

            MeshesLoaded laserTowerMesh hexTileMesh hexTileHighlightMesh enemySphereMesh wallMesh thingToProtect ->
                model
                    |> Initializing
                    |> Tea.save
                    |> Tea.withCmd
                        (Time.now
                            |> Task.perform
                                (Meshes.fromRaw
                                    laserTowerMesh
                                    hexTileMesh
                                    hexTileHighlightMesh
                                    enemySphereMesh
                                    wallMesh
                                    thingToProtect
                                    |> TimeInitialized
                                )
                        )

            TimeInitialized meshes currentTime ->
                initReady
                    { currentTime = currentTime
                    , meshes = meshes
                    }

            _ ->
                model
                    |> Initializing
                    |> Tea.save


initReady : { currentTime : Time.Posix, meshes : Meshes WorldCoordinates } -> Tea Model Msg
initReady cfg =
    Ready
        { level = Level.init
        , meshes = cfg.meshes
        , lastTimestamp = cfg.currentTime
        , highlightedTile = Nothing
        , cameraEyePoint = Point3d.meters 25 0 35
        , cameraRotation = { current = Angle.degrees 0, destination = Angle.degrees 0 }
        }
        |> Tea.save


updateReady : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> ReadyModel -> Tea model msg
updateReady cfg msg model =
    Tea.map cfg <|
        Tea.mapModel Ready <|
            case msg of
                Tick currentTimestamp ->
                    let
                        deltaTime : Duration
                        deltaTime =
                            (Time.posixToMillis currentTimestamp - Time.posixToMillis model.lastTimestamp)
                                |> toFloat
                                |> Duration.milliseconds
                    in
                    { model
                        | lastTimestamp = currentTimestamp
                    }
                        |> moveCamera deltaTime
                        |> Tea.save

                -- TrapTypeSelected trapType ->
                --     { model
                --         | trapType = trapType
                --     }
                --         |> Tea.save
                RotateCamera handedDirection ->
                    rotateCamera handedDirection model
                        |> Tea.save

                KeyPressed { key, alt, control, meta } ->
                    if (key == "a" || key == "A") && not alt && not control && not meta then
                        rotateCamera Left model
                            |> Tea.save

                    else if (key == "d" || key == "D") && not alt && not control && not meta then
                        rotateCamera Right model
                            |> Tea.save

                    else
                        model
                            |> Tea.save

                MouseMoved screenPoint ->
                    let
                        maybeHoverPoint : Maybe (Point3d Length.Meters WorldCoordinates)
                        maybeHoverPoint =
                            Camera3d.ray
                                (defaultCamera model.cameraRotation.current model.cameraEyePoint)
                                screenRectangle
                                screenPoint
                                |> Axis3d.intersectionWithPlane Plane3d.xy
                    in
                    case maybeHoverPoint of
                        Nothing ->
                            { model
                                | highlightedTile = Nothing
                            }
                                |> Tea.save

                        Just hoverPoint ->
                            let
                                hex : Hex
                                hex =
                                    hoverPoint
                                        |> Point3d.projectInto SketchPlane3d.xy
                                        |> Hex.fromPoint2d hexMapLayout
                                        |> Hex.toIntCoordinates
                            in
                            { model
                                | highlightedTile = Just hex
                            }
                                |> Tea.save

                Clicked screenPoint ->
                    let
                        maybeClickedPoint : Maybe (Point3d Length.Meters WorldCoordinates)
                        maybeClickedPoint =
                            Camera3d.ray
                                (defaultCamera model.cameraRotation.current model.cameraEyePoint)
                                screenRectangle
                                screenPoint
                                |> Axis3d.intersectionWithPlane Plane3d.xy
                    in
                    case maybeClickedPoint of
                        Nothing ->
                            model
                                |> Tea.save

                        Just clickedPoint ->
                            let
                                clickedHex : Hex
                                clickedHex =
                                    clickedPoint
                                        |> Point3d.projectInto SketchPlane3d.xy
                                        |> Hex.fromPoint2d hexMapLayout
                                        |> Hex.toIntCoordinates
                            in
                            case Dict.get (Hex.toKey clickedHex) model.level.tilemap of
                                Nothing ->
                                    { model
                                        | level = Level.addTile clickedHex model.level
                                    }
                                        |> Tea.save

                                Just _ ->
                                    { model
                                        | level = Level.removeTile clickedHex model.level
                                    }
                                        |> Tea.save

                _ ->
                    model
                        |> Tea.save


moveCamera : Duration -> ReadyModel -> ReadyModel
moveCamera deltaTime model =
    if Quantity.equalWithin (Angle.degrees 0.1) model.cameraRotation.current model.cameraRotation.destination then
        model

    else
        let
            desiredRotationAmount : Angle
            desiredRotationAmount =
                model.cameraRotation.destination
                    |> Quantity.minus model.cameraRotation.current

            desiredRotationSign : Float
            desiredRotationSign =
                desiredRotationAmount
                    |> Quantity.sign

            rotationSpeed : AngularSpeed
            rotationSpeed =
                desiredRotationSign
                    |> (*) 45
                    |> AngularSpeed.degreesPerSecond

            maxRotationAmount : Angle
            maxRotationAmount =
                deltaTime
                    |> Quantity.at rotationSpeed

            cameraRotation : { current : Angle, destination : Angle }
            cameraRotation =
                model.cameraRotation
        in
        { model
            | cameraRotation =
                { cameraRotation
                    | current =
                        cameraRotation.current
                            |> Quantity.plus
                                (if desiredRotationSign < 0 then
                                    Quantity.max
                                        maxRotationAmount
                                        desiredRotationAmount

                                 else
                                    Quantity.min
                                        maxRotationAmount
                                        desiredRotationAmount
                                )
                }
        }


rotateCamera : HandedDirection -> ReadyModel -> ReadyModel
rotateCamera handedDirection model =
    let
        cameraRotation : { current : Angle, destination : Angle }
        cameraRotation =
            model.cameraRotation

        handedSign : Float
        handedSign =
            case handedDirection of
                Left ->
                    -1

                Right ->
                    1
    in
    { model
        | cameraRotation =
            { cameraRotation
                | destination =
                    cameraRotation.destination
                        |> Quantity.plus (Angle.degrees (60 * handedSign))
            }
    }


screenRectangle : Rectangle2d Pixels ScreenCoordinates
screenRectangle =
    Rectangle2d.with
        { x1 = Pixels.pixels 0
        , y1 = Pixels.pixels 600
        , x2 = Pixels.pixels 800
        , y2 = Pixels.pixels 0
        }



----------
-- VIEW --
----------


view : { toMsg : Msg -> msg } -> Model -> List (Html msg)
view cfg model =
    List.map (Tea.mapView cfg) <|
        case model of
            Initializing _ ->
                [ Html.div [ Css.initializing ]
                    [ Html.text "Initializing..."
                    , Html.div
                        [ Css.loading ]
                        []
                    ]
                ]

            InitializingFailed err ->
                [ Html.div [ Css.initializing ]
                    [ Html.text "Initializing failed 😞"
                    , Html.br [] []
                    , Html.p [ Css.errorReport ]
                        [ Html.text err ]
                    , Html.br [] []
                    , Html.p [ Css.errorReport ]
                        [ Html.text "If you have a moment, I'd apprecate if you could report this error at "
                        , Html.a
                            [ Html.Attributes.href "https://github.com/wolfadex/towers/issues" ]
                            [ Html.text "https://github.com/wolfadex/towers/issues" ]
                        , Html.text "."
                        ]
                    ]
                ]

            Ready readyModel ->
                viewReady readyModel


viewReady : ReadyModel -> List (Html Msg)
viewReady model =
    [ Html.div
        [ Html.Events.on "click" decodeClick
        , Html.Events.on "mousemove" decodeMouseMove
        , Html.Attributes.style "width" "800px"
        , Html.Attributes.style "height" "600px"
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.positiveZ
            , sunlightDirection =
                Direction3d.negativeZ
                    |> Direction3d.rotateAround
                        Axis3d.x
                        (Angle.degrees 15)
                    |> Direction3d.rotateAround
                        Axis3d.y
                        (Angle.degrees -5)
            , shadows = True
            , dimensions = ( Pixels.int 800, Pixels.int 600 )
            , camera = defaultCamera model.cameraRotation.current model.cameraEyePoint
            , clipDepth = Length.meters 0.1
            , background = Scene3d.backgroundColor Color.black
            , entities =
                List.concat
                    [ viewHexGridMap3d model.meshes model.level.tilemap
                    , viewHighlightedTile3d model.highlightedTile model.meshes.hexTileHighlight

                    -- , viewEnemies3d model
                    -- , viewStaticMeshes model
                    , viewThingsToProtect3d model
                    ]
            }
        ]
    , Html.div []
        [ Html.div
            [ Css.cameraControlsLabel ]
            [ Html.text "Camera Controls" ]
        , Html.div
            [ Css.cameraControls ]
            [ Html.button
                [ Html.Events.onClick (RotateCamera Left) ]
                [ Html.text "Rotate Left" ]
            , Html.button
                [ Html.Events.onClick (RotateCamera Right) ]
                [ Html.text "Rotate Right" ]
            ]
        ]
    ]


defaultCamera : Angle -> Point3d Length.Meters WorldCoordinates -> Camera3d Length.Meters WorldCoordinates
defaultCamera rotation eyePoint =
    let
        cameraViewpoint : Viewpoint3d Length.Meters WorldCoordinates
        cameraViewpoint =
            Viewpoint3d.lookAt
                { eyePoint =
                    eyePoint
                        |> Point3d.rotateAround
                            Axis3d.z
                            rotation
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = Angle.degrees 30
        }


hexMapLayout : Hex.Layout
hexMapLayout =
    { orientation = Hex.pointyOrientation
    , size = ( 1, 1 )
    , origin = ( 0, 0 )
    }


decodeClick : Json.Decode.Decoder Msg
decodeClick =
    Json.Decode.map2
        (\x y ->
            Point2d.pixels x y
                |> Clicked
        )
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2
        (\x y ->
            Point2d.pixels x y
                |> MouseMoved
        )
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)


viewThingsToProtect3d : ReadyModel -> List (Scene3d.Entity WorldCoordinates)
viewThingsToProtect3d model =
    -- let
    --     ( meshCore, shadowCore ) =
    --         model.meshes.thingToProtect.core
    --     ( meshRingInner, shadowRingInner ) =
    --         model.meshes.thingToProtect.ringInner
    --     ( meshRingMiddle, shadowRingMiddle ) =
    --         model.meshes.thingToProtect.ringMiddle
    --     ( meshRingOuter, shadowRingOuter ) =
    --         model.meshes.thingToProtect.ringOuter
    -- in
    -- Ecs.System.foldl2
    --     (\position turns acc ->
    --         let
    --             translateBy : Vector3d Length.Meters WorldCoordinates
    --             translateBy =
    --                 originPosition
    --                     |> Vector3d.from Point3d.origin
    --             originPosition : Point3d Length.Meters WorldCoordinates
    --             originPosition =
    --                 position
    --                     |> Hex.toPoint2d hexMapLayout
    --                     |> Point3d.on SketchPlane3d.xy
    --                     |> Point3d.translateIn Direction3d.positiveZ (Length.meters 2)
    --             core3d : Scene3d.Entity WorldCoordinates
    --             core3d =
    --                 Scene3d.meshWithShadow
    --                     (Scene3d.Material.metal
    --                         { baseColor = Color.yellow
    --                         , roughness = 0.5
    --                         }
    --                     )
    --                     meshCore
    --                     shadowCore
    --                     |> Scene3d.translateBy translateBy
    --             ringInner3d : Scene3d.Entity WorldCoordinates
    --             ringInner3d =
    --                 Scene3d.meshWithShadow
    --                     (Scene3d.Material.metal
    --                         { baseColor = Color.lightBlue
    --                         , roughness = 0.5
    --                         }
    --                     )
    --                     meshRingInner
    --                     shadowRingInner
    --                     |> Scene3d.translateBy translateBy
    --                     |> Scene3d.rotateAround
    --                         (Axis3d.through originPosition
    --                             Direction3d.positiveX
    --                         )
    --                         turns
    --             ringMiddle3d : Scene3d.Entity WorldCoordinates
    --             ringMiddle3d =
    --                 Scene3d.meshWithShadow
    --                     (Scene3d.Material.metal
    --                         { baseColor = Color.lightRed
    --                         , roughness = 0.5
    --                         }
    --                     )
    --                     meshRingMiddle
    --                     shadowRingMiddle
    --                     |> Scene3d.translateBy translateBy
    --                     |> Scene3d.rotateAround
    --                         (Axis3d.through originPosition
    --                             Direction3d.positiveZ
    --                         )
    --                         turns
    --             ringOuter3d : Scene3d.Entity WorldCoordinates
    --             ringOuter3d =
    --                 Scene3d.meshWithShadow
    --                     (Scene3d.Material.metal
    --                         { baseColor = Color.lightGreen
    --                         , roughness = 0.5
    --                         }
    --                     )
    --                     meshRingOuter
    --                     shadowRingOuter
    --                     |> Scene3d.translateBy translateBy
    --                     |> Scene3d.rotateAround
    --                         (Axis3d.through originPosition
    --                             Direction3d.positiveY
    --                         )
    --                         turns
    --         in
    --         core3d
    --             :: ringInner3d
    --             :: ringMiddle3d
    --             :: ringOuter3d
    --             :: acc
    --     )
    --     model.positionComponent
    --     model.thingToProtectAnimationComponent
    []



-- viewStaticMeshes : ReadyModel -> List (Scene3d.Entity WorldCoordinates)
-- viewStaticMeshes model =
--     Ecs.System.foldl (::)
--         model.staticMeshComponent
--         []


viewHexGridMap3d : Meshes WorldCoordinates -> Hex.Map (Maybe Level.Prop) -> List (Scene3d.Entity WorldCoordinates)
viewHexGridMap3d meshes hexMap =
    let
        ( mesh, shadow ) =
            meshes.hexTile

        to3dEntity : Hex.Key -> Scene3d.Entity WorldCoordinates
        to3dEntity key =
            let
                translateBy : Vector3d Length.Meters WorldCoordinates
                translateBy =
                    key
                        |> Hex.fromKey
                        |> Hex.toPoint2d hexMapLayout
                        |> Point3d.on SketchPlane3d.xy
                        |> Vector3d.from Point3d.origin
            in
            Scene3d.meshWithShadow
                (Scene3d.Material.metal
                    { baseColor = Color.gray
                    , roughness = 0.5
                    }
                )
                mesh
                shadow
                |> Scene3d.translateBy translateBy
    in
    hexMap
        |> Dict.keys
        |> List.map to3dEntity


viewHighlightedTile3d : Maybe Hex -> Meshes.MeshAndShadow WorldCoordinates -> List (Scene3d.Entity WorldCoordinates)
viewHighlightedTile3d maybeHighlitedHex ( mesh, _ ) =
    case maybeHighlitedHex of
        Nothing ->
            []

        Just highlightedHex ->
            let
                translateBy : Vector3d Length.Meters WorldCoordinates
                translateBy =
                    highlightedHex
                        |> Hex.toPoint2d hexMapLayout
                        |> Point3d.on SketchPlane3d.xy
                        |> Vector3d.from Point3d.origin
            in
            [ Scene3d.mesh
                (Scene3d.Material.metal
                    { baseColor = Color.lightRed
                    , roughness = 0.7
                    }
                )
                mesh
                |> Scene3d.translateBy translateBy
            ]
