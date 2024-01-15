module Game exposing
    ( AttackAnimation(..)
    , AttackStyle(..)
    , Delay
    , Enemy(..)
    , Health
    , Model
    , Msg
    , Player(..)
    , ScreenCoordinates(..)
    , Trap(..)
    , TrapType
    , Wave(..)
    , WorldCoordinates(..)
    , init
    , subscriptions
    , update
    , view
    )

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
import LineSegment3d
import List.Extra
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
import Task.Parallel
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
    Task.Parallel.State5 Msg GameMesh GameMesh GameMesh WallGameMesh ThingToProtectGameMesh


type alias ReadyModel =
    { tilemap : Hex.Map ( Scene3d.Entity WorldCoordinates, Maybe Ecs.Entity )
    , cameraPosition : Point2d Pixels ScreenCoordinates
    , seed : Random.Seed
    , lastTimestamp : Time.Posix
    , currency : Int
    , waves : Maybe Wave
    , tillNextWave : Maybe { initial : Duration, remaining : Duration }
    , trapType : TrapType
    , meshes : Meshes

    -- ECS
    , ecsConfig : Ecs.Config
    , colorComponent : Ecs.Component String
    , staticMeshComponent : Ecs.Component (Scene3d.Entity WorldCoordinates)
    , thingToProtectAnimationComponent : Ecs.Component Angle
    , positionComponent : Ecs.Component Hex
    , healthComponent : Ecs.Component Health
    , pathComponent :
        Ecs.Component
            { path : List Hex
            , distance : Float
            , point : Point2d Length.Meters WorldCoordinates
            }
    , playerComponent : Ecs.Component Player
    , enemyComponent : Ecs.Component Enemy
    , trapComponent : Ecs.Component Trap
    , delayComponent : Ecs.Component Delay
    , attackStyleComponent : Ecs.Component AttackStyle
    , attackAnimationComponent : Ecs.Component AttackAnimation
    , wallComponent : Ecs.Component Wall
    }


type alias Meshes =
    { laserTower : MeshAndShadow
    , hexTile : MeshAndShadow
    , enemySphere : MeshAndShadow
    , wall : WallMeshAndShadow
    , thingToProtect : ThingToProtectMeshAndShadow
    }


type alias MeshAndShadow =
    ( Scene3d.Mesh.Textured WorldCoordinates
    , Scene3d.Mesh.Shadow WorldCoordinates
    )


type alias WallMeshAndShadow =
    { core : MeshAndShadow
    , northEast : MeshAndShadow
    , east : MeshAndShadow
    , southEast : MeshAndShadow
    , southWest : MeshAndShadow
    , west : MeshAndShadow
    , northWest : MeshAndShadow
    }


type alias ThingToProtectMeshAndShadow =
    { core : MeshAndShadow
    , ringInner : MeshAndShadow
    , ringMiddle : MeshAndShadow
    , ringOuter : MeshAndShadow
    }


type TrapType
    = AttackTower
    | BlockingWall


type ScreenCoordinates
    = ScreenCoordinates Never


type WorldCoordinates
    = WorldCoordinates Never


ecsConfigSpec : Ecs.Config.Spec ReadyModel
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


colorSpec : Ecs.Component.Spec String ReadyModel
colorSpec =
    { get = .colorComponent
    , set = \colorComponent world -> { world | colorComponent = colorComponent }
    }


staticMeshSpec : Ecs.Component.Spec (Scene3d.Entity WorldCoordinates) ReadyModel
staticMeshSpec =
    { get = .staticMeshComponent
    , set = \staticMeshComponent world -> { world | staticMeshComponent = staticMeshComponent }
    }


thingToProtectAnimationSpec : Ecs.Component.Spec Angle ReadyModel
thingToProtectAnimationSpec =
    { get = .thingToProtectAnimationComponent
    , set = \thingToProtectAnimationComponent world -> { world | thingToProtectAnimationComponent = thingToProtectAnimationComponent }
    }


positionSpec : Ecs.Component.Spec Hex ReadyModel
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


type alias Health =
    { current : Int
    , max : Int
    }


healthSpec : Ecs.Component.Spec Health ReadyModel
healthSpec =
    { get = .healthComponent
    , set = \healthComponent world -> { world | healthComponent = healthComponent }
    }


pathSpec :
    Ecs.Component.Spec
        { path : List Hex
        , distance : Float
        , point : Point2d Length.Meters WorldCoordinates
        }
        ReadyModel
pathSpec =
    { get = .pathComponent
    , set = \pathComponent world -> { world | pathComponent = pathComponent }
    }


type Player
    = Player


playerSpec : Ecs.Component.Spec Player ReadyModel
playerSpec =
    { get = .playerComponent
    , set = \playerComponent world -> { world | playerComponent = playerComponent }
    }


type Enemy
    = Enemy


enemySpec : Ecs.Component.Spec Enemy ReadyModel
enemySpec =
    { get = .enemyComponent
    , set = \enemyComponent world -> { world | enemyComponent = enemyComponent }
    }


type Trap
    = Trap


trapSpec : Ecs.Component.Spec Trap ReadyModel
trapSpec =
    { get = .trapComponent
    , set = \trapComponent world -> { world | trapComponent = trapComponent }
    }


type Wall
    = Wall


wallSpec : Ecs.Component.Spec Wall ReadyModel
wallSpec =
    { get = .wallComponent
    , set = \wallComponent world -> { world | wallComponent = wallComponent }
    }


type alias Delay =
    { between : Duration
    , remaining : Duration
    }


delaySpec : Ecs.Component.Spec Delay ReadyModel
delaySpec =
    { get = .delayComponent
    , set = \delayComponent world -> { world | delayComponent = delayComponent }
    }


type AttackStyle
    = Laser


attackStyleSpec : Ecs.Component.Spec AttackStyle ReadyModel
attackStyleSpec =
    { get = .attackStyleComponent
    , set = \attackStyleComponent world -> { world | attackStyleComponent = attackStyleComponent }
    }


type AttackAnimation
    = NoAttack
    | Attacking
        { from : Point2d Length.Meters WorldCoordinates
        , to : Point2d Length.Meters WorldCoordinates
        , duration : Duration
        }


attackAnimationSpec : Ecs.Component.Spec AttackAnimation ReadyModel
attackAnimationSpec =
    { get = .attackAnimationComponent
    , set = \attackAnimationComponent world -> { world | attackAnimationComponent = attackAnimationComponent }
    }



----------
-- INIT --
----------


init : { toMsg : Msg -> msg, toModel : Model -> model } -> Tea model msg
init cfg =
    loadMeshes
        |> Tea.fromTuple
        |> Tea.mapModel Initializing
        |> Tea.map cfg


getMesh : String -> Obj.Decode.Decoder a -> Task Http.Error a
getMesh fileName decoder =
    Http.task
        { method = "GET"
        , url = "mesh/" ++ fileName ++ ".obj"
        , resolver =
            Obj.Decode.decodeString
                Length.meters
                decoder
                |> decodeStringResolver
                |> Http.stringResolver
        , body = Http.emptyBody
        , headers = []
        , timeout = Nothing
        }


decodeStringResolver : (String -> Result String a) -> Http.Response String -> Result Http.Error a
decodeStringResolver stringDecoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case stringDecoder body of
                Err err ->
                    Err (Http.BadBody err)

                Ok good ->
                    Ok good


loadMeshes :
    ( Task.Parallel.State5 Msg GameMesh GameMesh GameMesh WallGameMesh ThingToProtectGameMesh
    , Cmd Msg
    )
loadMeshes =
    let
        namedTexturedFacesIn : String -> Obj.Decode.Decoder GameMesh
        namedTexturedFacesIn name =
            meshFilterNameEquals name
                (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
    in
    Task.Parallel.attempt5
        { task1 = getMesh "laser_tower" (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
        , task2 = getMesh "ground_hex" (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
        , task3 = getMesh "enemy_sphere" (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
        , task4 =
            getMesh "wall"
                (Obj.Decode.succeed WallGameMesh
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Core")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_NE")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_E")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_SE")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_SW")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_W")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_NW")
                )
        , task5 =
            getMesh "thing_to_protect"
                (Obj.Decode.map4 ThingToProtectGameMesh
                    (namedTexturedFacesIn "Core")
                    (namedTexturedFacesIn "Rings_Inner")
                    (namedTexturedFacesIn "Rings_Middle")
                    (namedTexturedFacesIn "Rings_Outer")
                )
        , onUpdates = MeshesLoading
        , onFailure = MeshesLoadFailed
        , onSuccess = MeshesLoaded
        }


meshFilterNameEquals : String -> Obj.Decode.Decoder a -> Obj.Decode.Decoder a
meshFilterNameEquals name =
    Obj.Decode.filter
        (\{ object } ->
            case object of
                Just objectName ->
                    objectName == name

                Nothing ->
                    False
        )


type Wave
    = FinalWave Int
    | NextWave Int Duration Wave


createPlayer : ReadyModel -> ReadyModel
createPlayer model =
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( thingToProtectAnimationSpec, Angle.turns 0 )
        |> Ecs.Entity.with ( healthSpec, { current = 100, max = 100 } )
        |> Ecs.Entity.with ( playerSpec, Player )
        |> Ecs.Entity.with ( positionSpec, Hex.origin )
        |> Tuple.second


createEnemy : ReadyModel -> ReadyModel
createEnemy model =
    let
        edgeHexes : List Hex
        edgeHexes =
            model.tilemap
                |> Dict.keys
                |> List.filterMap
                    (\key ->
                        let
                            hex : Hex
                            hex =
                                Hex.fromKey key

                            nbrs : Int
                            nbrs =
                                hex
                                    |> Hex.neighbors
                                    |> List.filterMap
                                        (\neighbor ->
                                            Dict.get
                                                (Hex.toKey neighbor)
                                                model.tilemap
                                        )
                                    |> List.length
                        in
                        if nbrs < 6 then
                            Just hex

                        else
                            Nothing
                    )

        ( maybeHex, nextSeed ) =
            Random.step
                (edgeHexes
                    |> Random.List.choose
                    |> Random.map Tuple.first
                )
                model.seed
    in
    case maybeHex of
        Nothing ->
            { model | seed = nextSeed }

        Just hex ->
            { model | seed = nextSeed }
                |> Ecs.Entity.create ecsConfigSpec
                |> Ecs.Entity.with ( colorSpec, "red" )
                |> Ecs.Entity.with ( healthSpec, { current = 10, max = 10 } )
                |> Ecs.Entity.with ( positionSpec, hex )
                |> Ecs.Entity.with ( enemySpec, Enemy )
                |> Ecs.Entity.with
                    ( pathSpec
                    , { path =
                            findPath
                                { from = hex
                                , to = Hex.origin
                                , tilemap = model.tilemap
                                }
                      , distance = 0
                      , point = Hex.toPoint2d hexMapLayout hex
                      }
                    )
                |> Tuple.second


findPath :
    { from : Hex
    , to : Hex
    , tilemap : Hex.Map ( Scene3d.Entity WorldCoordinates, Maybe Ecs.Entity )
    }
    -> List Hex
findPath cfg =
    AStar.Generalised.findPath
        (\a b ->
            Hex.distance
                (Hex.fromKey a)
                (Hex.fromKey b)
                |> toFloat
        )
        (\a ->
            a
                |> Hex.fromKey
                |> Hex.neighbors
                |> List.filterMap
                    (\neighbor ->
                        case Dict.get (Hex.toKey neighbor) cfg.tilemap of
                            Nothing ->
                                Nothing

                            Just ( _, Just _ ) ->
                                Nothing

                            Just ( _, Nothing ) ->
                                Just (Hex.toKey neighbor)
                    )
                |> Set.fromList
        )
        (Hex.toKey cfg.from)
        (Hex.toKey cfg.to)
        |> Maybe.withDefault []
        |> List.map Hex.fromKey


removeEnemy : Ecs.Entity -> ReadyModel -> ReadyModel
removeEnemy entity model =
    ( entity, { model | currency = model.currency + 10 } )
        |> Ecs.Entity.remove colorSpec
        |> Ecs.Entity.remove healthSpec
        |> Ecs.Entity.remove positionSpec
        |> Ecs.Entity.remove enemySpec
        |> Ecs.Entity.remove pathSpec
        |> Tuple.second


createAttackTower : Hex -> ReadyModel -> ( Ecs.Entity, ReadyModel )
createAttackTower position model =
    let
        ( mesh, shadow ) =
            model.meshes.laserTower

        translateBy : Vector3d Length.Meters WorldCoordinates
        translateBy =
            position
                |> Hex.toPoint2d hexMapLayout
                |> Point3d.on SketchPlane3d.xy
                |> Vector3d.from Point3d.origin
    in
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with
            ( staticMeshSpec
            , Scene3d.meshWithShadow
                (Scene3d.Material.metal
                    { baseColor = Color.green
                    , roughness = 0.5
                    }
                )
                mesh
                shadow
                |> Scene3d.translateBy translateBy
            )
        |> Ecs.Entity.with ( trapSpec, Trap )
        |> Ecs.Entity.with ( attackStyleSpec, Laser )
        |> Ecs.Entity.with ( positionSpec, position )
        |> Ecs.Entity.with
            ( delaySpec
            , { between = Duration.seconds 2
              , remaining = Duration.seconds 0
              }
            )


createWall : Hex -> ReadyModel -> ( Ecs.Entity, ReadyModel )
createWall position model =
    let
        ( mesh, shadow ) =
            model.meshes.wall.core

        translateBy : Vector3d Length.Meters WorldCoordinates
        translateBy =
            position
                |> Hex.toPoint2d hexMapLayout
                |> Point3d.on SketchPlane3d.xy
                |> Vector3d.from Point3d.origin
    in
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( wallSpec, Wall )
        |> Ecs.Entity.with
            ( staticMeshSpec
            , Scene3d.meshWithShadow
                (Scene3d.Material.metal
                    { baseColor = Color.darkBlue
                    , roughness = 0.5
                    }
                )
                mesh
                shadow
                |> Scene3d.translateBy translateBy
            )
        |> Ecs.Entity.with ( trapSpec, Trap )
        |> Ecs.Entity.with ( positionSpec, position )



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
            Browser.Events.onAnimationFrame Tick
                |> Tea.mapSub cfg



------------
-- UPDATE --
------------


type Msg
    = Tick Time.Posix
    | Clicked (Point2d Pixels ScreenCoordinates)
    | TrapTypeSelected TrapType
    | MeshesLoading (Task.Parallel.Msg5 GameMesh GameMesh GameMesh WallGameMesh ThingToProtectGameMesh)
    | MeshesLoadFailed Http.Error
    | MeshesLoaded GameMesh GameMesh GameMesh WallGameMesh ThingToProtectGameMesh
    | TimeInitialized Meshes Time.Posix


type alias GameMesh =
    TriangularMesh
        { normal : Vector3d Quantity.Unitless WorldCoordinates
        , position : Point3d Length.Meters WorldCoordinates
        , uv : ( Float, Float )
        }


type alias ThingToProtectGameMesh =
    { core : GameMesh
    , ringInner : GameMesh
    , ringMiddle : GameMesh
    , ringOuter : GameMesh
    }


type alias WallGameMesh =
    { core : GameMesh
    , northEast : GameMesh
    , east : GameMesh
    , southEast : GameMesh
    , southWest : GameMesh
    , west : GameMesh
    , northWest : GameMesh
    }


update : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> Model -> Tea model msg
update cfg msg model =
    case model of
        Initializing meshLoadingModel ->
            updateInitializing cfg msg meshLoadingModel

        InitializingFailed _ ->
            model
                |> Tea.save
                |> Tea.map cfg

        Ready readyModel ->
            updateReady cfg msg readyModel


updateInitializing : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> InitializingModel -> Tea model msg
updateInitializing cfg msg model =
    Tea.map cfg <|
        case msg of
            MeshesLoading msg_ ->
                let
                    ( nextMeshLoadingModel, meshLoadingCmd ) =
                        Task.Parallel.update5 model msg_
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

            MeshesLoaded laserTowerMesh hexTileMesh enemySphereMesh wallMesh thingToProtect ->
                let
                    initMesh : GameMesh -> ( Scene3d.Mesh.Textured WorldCoordinates, Scene3d.Mesh.Shadow WorldCoordinates )
                    initMesh m =
                        let
                            mesh : Scene3d.Mesh.Textured WorldCoordinates
                            mesh =
                                Scene3d.Mesh.texturedFaces m
                        in
                        ( mesh
                        , Scene3d.Mesh.shadow mesh
                        )
                in
                model
                    |> Initializing
                    |> Tea.save
                    |> Tea.withCmd
                        (Time.now
                            |> Task.perform
                                (TimeInitialized
                                    { laserTower = initMesh laserTowerMesh
                                    , hexTile = initMesh hexTileMesh
                                    , enemySphere = initMesh enemySphereMesh
                                    , wall =
                                        { core = initMesh wallMesh.core
                                        , northEast = initMesh wallMesh.northEast
                                        , east = initMesh wallMesh.east
                                        , southEast = initMesh wallMesh.southEast
                                        , southWest = initMesh wallMesh.southWest
                                        , west = initMesh wallMesh.west
                                        , northWest = initMesh wallMesh.northWest
                                        }
                                    , thingToProtect =
                                        { core = initMesh thingToProtect.core
                                        , ringInner = initMesh thingToProtect.ringInner
                                        , ringMiddle = initMesh thingToProtect.ringMiddle
                                        , ringOuter = initMesh thingToProtect.ringOuter
                                        }
                                    }
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


initReady : { currentTime : Time.Posix, meshes : Meshes } -> Tea Model Msg
initReady cfg =
    let
        ( mesh, shadow ) =
            cfg.meshes.hexTile

        to3dEntity : Hex -> Scene3d.Entity WorldCoordinates
        to3dEntity hex =
            let
                translateBy : Vector3d Length.Meters WorldCoordinates
                translateBy =
                    hex
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

        tilemap : Hex.Map ( Scene3d.Entity WorldCoordinates, Maybe Ecs.Entity )
        tilemap =
            Hex.origin
                |> Hex.circle 6
                |> List.map (\hex -> ( Hex.toKey hex, ( to3dEntity hex, Nothing ) ))
                |> Dict.fromList
    in
    { tilemap = tilemap
    , cameraPosition = Point2d.origin
    , seed =
        cfg.currentTime
            |> Time.posixToMillis
            |> Random.initialSeed
    , lastTimestamp = cfg.currentTime
    , currency = 500
    , waves =
        FinalWave 21
            |> NextWave 13 (Duration.seconds 10)
            |> NextWave 11 (Duration.seconds 10)
            |> NextWave 7 (Duration.seconds 10)
            |> NextWave 5 (Duration.seconds 10)
            |> NextWave 3 (Duration.seconds 10)
            |> NextWave 2 (Duration.seconds 10)
            |> Just
    , tillNextWave =
        Just
            { initial = Duration.seconds 1
            , remaining = Duration.seconds 1
            }
    , trapType = AttackTower
    , meshes = cfg.meshes

    -- ECS
    , ecsConfig = Ecs.Config.init
    , colorComponent = Ecs.Component.empty
    , staticMeshComponent = Ecs.Component.empty
    , thingToProtectAnimationComponent = Ecs.Component.empty
    , positionComponent = Ecs.Component.empty
    , healthComponent = Ecs.Component.empty
    , pathComponent = Ecs.Component.empty
    , playerComponent = Ecs.Component.empty
    , enemyComponent = Ecs.Component.empty
    , trapComponent = Ecs.Component.empty
    , delayComponent = Ecs.Component.empty
    , attackStyleComponent = Ecs.Component.empty
    , attackAnimationComponent = Ecs.Component.empty
    , wallComponent = Ecs.Component.empty
    }
        |> createPlayer
        |> Ready
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
                        |> applyWave deltaTime
                        |> attackAnimationUpdate deltaTime
                        |> animationUpdate deltaTime
                        |> towerAttack deltaTime
                        |> moveEnemy deltaTime
                        |> enemyAttack
                        |> Tea.save

                TrapTypeSelected trapType ->
                    { model
                        | trapType = trapType
                    }
                        |> Tea.save

                Clicked screenPoint ->
                    let
                        screenRectangle : Rectangle2d Pixels ScreenCoordinates
                        screenRectangle =
                            Rectangle2d.with
                                { x1 = Pixels.pixels 0
                                , y1 = Pixels.pixels 600
                                , x2 = Pixels.pixels 800
                                , y2 = Pixels.pixels 0
                                }

                        maybeClickedPoint : Maybe (Point3d Length.Meters WorldCoordinates)
                        maybeClickedPoint =
                            Camera3d.ray
                                defaultCamera
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
                            case Dict.get (Hex.toKey clickedHex) model.tilemap of
                                Nothing ->
                                    model
                                        |> Tea.save

                                Just ( _, Just _ ) ->
                                    model
                                        |> Tea.save

                                Just ( _, Nothing ) ->
                                    case model.trapType of
                                        AttackTower ->
                                            model
                                                |> createTrap createAttackTower 100 clickedHex
                                                |> updateWallSections
                                                |> Tea.save

                                        BlockingWall ->
                                            model
                                                |> createTrap createWall 50 clickedHex
                                                |> updateWallSections
                                                |> Tea.save

                _ ->
                    model
                        |> Tea.save


updateWallSections : ReadyModel -> ReadyModel
updateWallSections model =
    let
        ( meshCore, shadowCore ) =
            model.meshes.wall.core

        -- The various directions are a bit flipped around because of the way they're
        -- exported from Blender and I was lazy and preferred swapping here rather
        -- than trying to figure out how to get themt to line up properly when exporting.
        ( meshSectionNE, shadowSectionNE ) =
            model.meshes.wall.southEast

        ( meshSectionE, shadowSectionE ) =
            model.meshes.wall.east

        ( meshSectionSE, shadowSectionSE ) =
            model.meshes.wall.northEast

        ( meshSectionSW, shadowSectionSW ) =
            model.meshes.wall.northWest

        ( meshSectionW, shadowSectionW ) =
            model.meshes.wall.west

        ( meshSectionNW, shadowSectionNW ) =
            model.meshes.wall.southWest

        translateBy : Hex -> Vector3d Length.Meters WorldCoordinates
        translateBy position =
            position
                |> Hex.toPoint2d hexMapLayout
                |> Point3d.on SketchPlane3d.xy
                |> Vector3d.from Point3d.origin

        toMaybeSection : Hex -> Hex.Direction -> Scene3d.Mesh.Textured WorldCoordinates -> Scene3d.Mesh.Shadow WorldCoordinates -> ReadyModel -> Maybe (Scene3d.Entity WorldCoordinates)
        toMaybeSection position direction mesh shadow mdl =
            position
                |> Hex.neighbor direction
                |> Hex.toKey
                |> Util.Dict.at mdl.tilemap
                |> Maybe.andThen Tuple.second
                |> Maybe.andThen (Util.Ecs.Component.at mdl.trapComponent)
                |> Maybe.map
                    (\_ ->
                        Scene3d.meshWithShadow
                            (Scene3d.Material.metal
                                { baseColor = Color.darkBlue
                                , roughness = 0.5
                                }
                            )
                            mesh
                            shadow
                            |> Scene3d.translateBy (translateBy position)
                    )
    in
    Ecs.System.indexedFoldl3
        (\wallEntity position _ _ nextModel ->
            let
                updatedEntity3d : Scene3d.Entity WorldCoordinates
                updatedEntity3d =
                    [ toMaybeSection position Hex.NorthEast meshSectionNE shadowSectionNE nextModel
                    , toMaybeSection position Hex.East meshSectionE shadowSectionE nextModel
                    , toMaybeSection position Hex.SouthEast meshSectionSE shadowSectionSE nextModel
                    , toMaybeSection position Hex.SouthWest meshSectionSW shadowSectionSW nextModel
                    , toMaybeSection position Hex.West meshSectionW shadowSectionW nextModel
                    , toMaybeSection position Hex.NorthWest meshSectionNW shadowSectionNW nextModel
                    , Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.darkBlue
                            , roughness = 0.5
                            }
                        )
                        meshCore
                        shadowCore
                        |> Scene3d.translateBy (translateBy position)
                        |> Just
                    ]
                        |> List.filterMap identity
                        |> Scene3d.group
            in
            Util.Ecs.Component.set
                staticMeshSpec
                wallEntity
                updatedEntity3d
                nextModel
        )
        model.positionComponent
        model.staticMeshComponent
        model.wallComponent
        model


createTrap : (Hex -> ReadyModel -> ( Ecs.Entity, ReadyModel )) -> Int -> Hex -> ReadyModel -> ReadyModel
createTrap createFn cost hex model =
    if model.currency >= cost then
        let
            key : Hex.Key
            key =
                Hex.toKey hex
        in
        case Dict.get key model.tilemap of
            Nothing ->
                model

            Just ( _, Just _ ) ->
                model

            Just ( entity3d, Nothing ) ->
                { model
                    | currency = model.currency - cost
                }
                    |> createFn hex
                    |> (\( tower, m ) ->
                            { m
                                | tilemap =
                                    Dict.insert
                                        key
                                        ( entity3d, Just tower )
                                        model.tilemap
                            }
                       )

    else
        model


applyWave : Duration -> ReadyModel -> ReadyModel
applyWave deltaTime model =
    let
        createEnemies : Int -> Maybe Duration -> Maybe Wave -> ReadyModel
        createEnemies enemyCount tillNext rest =
            List.foldl (\_ -> createEnemy)
                { model
                    | tillNextWave =
                        tillNext
                            |> Maybe.map
                                (\t ->
                                    { initial = t
                                    , remaining = t
                                    }
                                )
                    , waves = rest
                }
                (List.range 1 enemyCount)
    in
    case ( model.tillNextWave, model.waves ) of
        ( _, Nothing ) ->
            model

        ( Nothing, Just waves ) ->
            case waves of
                FinalWave enemyCount ->
                    createEnemies enemyCount Nothing Nothing

                NextWave enemyCount tillNext rest ->
                    createEnemies enemyCount (Just tillNext) (Just rest)

        ( Just tillNextWave, Just waves ) ->
            let
                remaining : Duration
                remaining =
                    tillNextWave.remaining
                        |> Quantity.minus deltaTime
            in
            if remaining |> Quantity.greaterThan (Duration.seconds 0) then
                { model
                    | tillNextWave =
                        Just
                            { tillNextWave
                                | remaining = remaining
                            }
                }

            else
                case waves of
                    FinalWave enemyCount ->
                        createEnemies enemyCount Nothing Nothing

                    NextWave enemyCount tillNext rest ->
                        createEnemies enemyCount (Just tillNext) (Just rest)


animationUpdate : Duration -> ReadyModel -> ReadyModel
animationUpdate deltaTime =
    Ecs.System.map
        (\turns ->
            let
                angularSpeed : AngularSpeed
                angularSpeed =
                    AngularSpeed.turnsPerSecond 0.25

                turnsToAdd : Angle
                turnsToAdd =
                    deltaTime
                        |> Quantity.at angularSpeed
            in
            turns
                |> Quantity.plus turnsToAdd
        )
        thingToProtectAnimationSpec


attackAnimationUpdate : Duration -> ReadyModel -> ReadyModel
attackAnimationUpdate deltaTime =
    Ecs.System.map
        (\attackAnimation ->
            case attackAnimation of
                NoAttack ->
                    NoAttack

                Attacking animation ->
                    let
                        remainingDuration : Duration
                        remainingDuration =
                            animation.duration |> Quantity.minus deltaTime
                    in
                    if remainingDuration |> Quantity.greaterThan (Duration.seconds 0) then
                        Attacking { animation | duration = remainingDuration }

                    else
                        NoAttack
        )
        attackAnimationSpec


moveEnemy : Duration -> ReadyModel -> ReadyModel
moveEnemy deltaTime =
    Ecs.System.map2
        (\( position, setPosition ) ( { path, distance }, setPath ) ->
            let
                deltaSeconds : Float
                deltaSeconds =
                    Duration.inSeconds deltaTime

                totalDistance : Float
                totalDistance =
                    distance + deltaSeconds

                tilesToDrop : Int
                tilesToDrop =
                    floor totalDistance

                remainingPath : List Hex
                remainingPath =
                    path
                        |> List.drop tilesToDrop
            in
            case remainingPath of
                [] ->
                    position
                        |> setPosition

                [ finalPos ] ->
                    finalPos
                        |> setPosition

                currentHex :: nextHex :: _ ->
                    let
                        nextDistance : Float
                        nextDistance =
                            totalDistance
                                |> Basics.Extra.fractionalModBy 1.0

                        pointAlong : Point2d Length.Meters WorldCoordinates
                        pointAlong =
                            Point2d.interpolateFrom
                                (Hex.toPoint2d hexMapLayout currentHex)
                                (Hex.toPoint2d hexMapLayout nextHex)
                                nextDistance
                    in
                    setPath
                        { path = remainingPath
                        , distance = nextDistance
                        , point = pointAlong
                        }
                        >> setPosition currentHex
        )
        positionSpec
        pathSpec


enemyAttack : ReadyModel -> ReadyModel
enemyAttack model =
    let
        playerPositions : List ( Ecs.Entity, Hex )
        playerPositions =
            Ecs.System.indexedFoldl2
                (\entity position _ players -> ( entity, position ) :: players)
                model.positionComponent
                model.playerComponent
                []
    in
    Ecs.System.indexedFoldl2
        (\entity position _ nextModel ->
            case List.Extra.find (\( _, playerPos ) -> Hex.similar position playerPos) playerPositions of
                Nothing ->
                    nextModel

                Just ( playerEntity, _ ) ->
                    nextModel
                        |> removeEnemy entity
                        |> Util.Maybe.apply
                            (\( health, _ ) m ->
                                healthSpec.set
                                    (m.healthComponent
                                        |> Ecs.Component.set playerEntity
                                            { health
                                                | current = health.current - 3
                                            }
                                    )
                                    m
                            )
                            (Ecs.Component.get2 playerEntity
                                model.healthComponent
                                model.playerComponent
                            )
        )
        model.positionComponent
        model.enemyComponent
        model


towerAttack : Duration -> ReadyModel -> ReadyModel
towerAttack deltaTime model =
    Ecs.System.indexedFoldl3
        (\towerEntity towerPosition _ delay nextModel ->
            let
                nextRemaining : Duration
                nextRemaining =
                    delay.remaining
                        |> Quantity.minus deltaTime
                        |> Quantity.max (Duration.seconds 0)
            in
            if nextRemaining |> Quantity.greaterThan (Duration.seconds 0) then
                delaySpec.set
                    (nextModel
                        |> delaySpec.get
                        |> Ecs.Component.set
                            towerEntity
                            { delay | remaining = nextRemaining }
                    )
                    nextModel

            else
                let
                    hexRange : List Hex
                    hexRange =
                        towerPosition
                            |> Hex.circle 3
                            |> List.filter
                                (\hex ->
                                    Dict.get (Hex.toKey hex) nextModel.tilemap /= Nothing
                                )

                    nearestEnemy : Maybe ( Ecs.Entity, Hex )
                    nearestEnemy =
                        hexRange
                            |> List.concatMap
                                (\hex ->
                                    Ecs.System.indexedFoldl2
                                        (\enemy enemyPosition _ acc ->
                                            if Hex.similar hex enemyPosition then
                                                ( enemy, enemyPosition ) :: acc

                                            else
                                                acc
                                        )
                                        nextModel.positionComponent
                                        nextModel.enemyComponent
                                        []
                                )
                            |> List.sortBy (\( _, pos ) -> Hex.distance pos towerPosition)
                            |> List.head
                in
                case nearestEnemy of
                    Nothing ->
                        Util.Ecs.Component.update
                            (\_ -> { delay | remaining = nextRemaining })
                            delaySpec
                            towerEntity
                            nextModel

                    Just ( enemy, enemyPosition ) ->
                        nextModel
                            |> removeEnemy enemy
                            |> Util.Ecs.Component.update
                                (\_ -> { delay | remaining = delay.between })
                                delaySpec
                                towerEntity
                            |> Util.Ecs.Component.set
                                attackAnimationSpec
                                towerEntity
                                (Attacking
                                    { from = Hex.toPoint2d hexMapLayout towerPosition
                                    , to =
                                        nextModel.pathComponent
                                            |> Ecs.Component.get enemy
                                            |> Maybe.map .point
                                            |> Maybe.withDefault (Hex.toPoint2d hexMapLayout enemyPosition)
                                    , duration = Duration.seconds 0.5
                                    }
                                )
        )
        model.positionComponent
        model.trapComponent
        model.delayComponent
        model



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
                    [ Html.text "Initializing failed: "
                    , Html.text err
                    ]
                ]

            Ready readyModel ->
                viewReady readyModel


viewReady : ReadyModel -> List (Html Msg)
viewReady model =
    [ Html.div [ Css.gameHeader ]
        [ Html.label
            []
            [ Html.text "Next wave: "
            , case model.tillNextWave of
                Nothing ->
                    Html.text "No more waves"

                Just tillNextWave ->
                    meter
                        { percent =
                            (Duration.inSeconds tillNextWave.remaining / Duration.inSeconds tillNextWave.initial)
                                |> (*) 100
                        , color = Css.Color.blue
                        }
            ]
        , Ecs.System.foldl2
            (\_ health acc ->
                Html.label
                    []
                    [ Html.text "Health: "
                    , meter
                        { percent =
                            (toFloat health.current / toFloat health.max)
                                |> (*) 100
                        , color = Css.Color.yellow
                        }
                    ]
                    :: acc
            )
            model.playerComponent
            model.healthComponent
            []
            |> Html.div []
        , Html.label
            []
            [ Html.text "Money: "
            , Html.text ("¥" ++ String.fromInt model.currency)
            ]
        , Html.div [ Css.trapSelection ]
            [ Html.button
                [ Html.Events.onClick (TrapTypeSelected AttackTower)
                , case model.trapType of
                    AttackTower ->
                        Css.selectedTrap

                    _ ->
                        Html.Attributes.class ""
                ]
                [ Html.text "Laser (¥100)" ]
            , Html.button
                [ Html.Events.onClick (TrapTypeSelected BlockingWall)
                , case model.trapType of
                    BlockingWall ->
                        Css.selectedTrap

                    _ ->
                        Html.Attributes.class ""
                ]
                [ Html.text "Wall  (¥50)" ]
            ]
        ]
    , Html.div
        [ Html.Events.on "click" decodeClick
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
            , camera = defaultCamera
            , clipDepth = Length.meters 0.1
            , background = Scene3d.backgroundColor Color.black
            , entities =
                List.concat
                    [ viewHexGridMap3d model.tilemap
                    , viewEnemies3d model
                    , viewStaticMeshes model
                    , viewAttacks3d model
                    , viewThingsToProtect3d model
                    ]
            }
        ]
    ]


defaultCamera : Camera3d Length.Meters WorldCoordinates
defaultCamera =
    let
        cameraViewpoint : Viewpoint3d Length.Meters WorldCoordinates
        cameraViewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 25 0 35
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


viewAttacks3d : ReadyModel -> List (Scene3d.Entity WorldCoordinates)
viewAttacks3d model =
    Ecs.System.foldl2
        (\animation style acc ->
            case animation of
                NoAttack ->
                    acc

                Attacking { from, to } ->
                    case style of
                        Laser ->
                            let
                                from3d : Point3d Length.Meters WorldCoordinates
                                from3d =
                                    from
                                        |> Point3d.on SketchPlane3d.xy
                                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 3.5)

                                to3d : Point3d Length.Meters WorldCoordinates
                                to3d =
                                    to
                                        |> Point3d.on SketchPlane3d.xy
                                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 1.5)
                            in
                            (LineSegment3d.from from3d to3d
                                |> Scene3d.lineSegment
                                    (Scene3d.Material.color Color.red)
                            )
                                :: acc
        )
        model.attackAnimationComponent
        model.attackStyleComponent
        []


viewEnemies3d : ReadyModel -> List (Scene3d.Entity WorldCoordinates)
viewEnemies3d model =
    let
        ( mesh, shadow ) =
            model.meshes.enemySphere
    in
    Ecs.System.foldl3
        (\{ point } _ _ acc ->
            (let
                translateBy : Vector3d Length.Meters WorldCoordinates
                translateBy =
                    point
                        |> Point3d.on SketchPlane3d.xy
                        |> Vector3d.from Point3d.origin
             in
             Scene3d.meshWithShadow
                (Scene3d.Material.metal
                    { baseColor = Color.red
                    , roughness = 0.5
                    }
                )
                mesh
                shadow
                |> Scene3d.translateBy translateBy
            )
                :: acc
        )
        model.pathComponent
        model.colorComponent
        model.enemyComponent
        []


viewThingsToProtect3d : ReadyModel -> List (Scene3d.Entity WorldCoordinates)
viewThingsToProtect3d model =
    let
        ( meshCore, shadowCore ) =
            model.meshes.thingToProtect.core

        ( meshRingInner, shadowRingInner ) =
            model.meshes.thingToProtect.ringInner

        ( meshRingMiddle, shadowRingMiddle ) =
            model.meshes.thingToProtect.ringMiddle

        ( meshRingOuter, shadowRingOuter ) =
            model.meshes.thingToProtect.ringOuter
    in
    Ecs.System.foldl2
        (\position turns acc ->
            let
                translateBy : Vector3d Length.Meters WorldCoordinates
                translateBy =
                    originPosition
                        |> Vector3d.from Point3d.origin

                originPosition : Point3d Length.Meters WorldCoordinates
                originPosition =
                    position
                        |> Hex.toPoint2d hexMapLayout
                        |> Point3d.on SketchPlane3d.xy
                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 2)

                core3d : Scene3d.Entity WorldCoordinates
                core3d =
                    Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.yellow
                            , roughness = 0.5
                            }
                        )
                        meshCore
                        shadowCore
                        |> Scene3d.translateBy translateBy

                ringInner3d : Scene3d.Entity WorldCoordinates
                ringInner3d =
                    Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.lightBlue
                            , roughness = 0.5
                            }
                        )
                        meshRingInner
                        shadowRingInner
                        |> Scene3d.translateBy translateBy
                        |> Scene3d.rotateAround
                            (Axis3d.through originPosition
                                Direction3d.positiveX
                            )
                            turns

                ringMiddle3d : Scene3d.Entity WorldCoordinates
                ringMiddle3d =
                    Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.lightRed
                            , roughness = 0.5
                            }
                        )
                        meshRingMiddle
                        shadowRingMiddle
                        |> Scene3d.translateBy translateBy
                        |> Scene3d.rotateAround
                            (Axis3d.through originPosition
                                Direction3d.positiveZ
                            )
                            turns

                ringOuter3d : Scene3d.Entity WorldCoordinates
                ringOuter3d =
                    Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.lightGreen
                            , roughness = 0.5
                            }
                        )
                        meshRingOuter
                        shadowRingOuter
                        |> Scene3d.translateBy translateBy
                        |> Scene3d.rotateAround
                            (Axis3d.through originPosition
                                Direction3d.positiveY
                            )
                            turns
            in
            core3d
                :: ringInner3d
                :: ringMiddle3d
                :: ringOuter3d
                :: acc
        )
        model.positionComponent
        model.thingToProtectAnimationComponent
        []


viewStaticMeshes : ReadyModel -> List (Scene3d.Entity WorldCoordinates)
viewStaticMeshes model =
    Ecs.System.foldl (::)
        model.staticMeshComponent
        []


viewHexGridMap3d : Hex.Map ( Scene3d.Entity WorldCoordinates, Maybe Ecs.Entity ) -> List (Scene3d.Entity WorldCoordinates)
viewHexGridMap3d hexMap =
    hexMap
        |> Dict.values
        |> List.map Tuple.first


meter : { percent : Float, color : String } -> Html msg
meter { percent, color } =
    Html.div
        [ Css.meter
        ]
        [ Html.div
            [ Css.meterInner
            , Html.Attributes.style "background-color" color
            , Html.Attributes.style "width" (String.fromFloat percent ++ "%")
            ]
            []
        ]
