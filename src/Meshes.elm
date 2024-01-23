module Meshes exposing
    ( EnemySphereMeshAndShadow
    , EnemySphereRawMesh
    , LoadingMsg
    , MeshAndShadow
    , Meshes
    , Model
    , RawMesh
    , ScreenCoordinates
    , ThingToProtectMeshAndShadow
    , ThingToProtectRawMesh
    , WallMeshAndShadow
    , WallRawMesh
    , WorldCoordinates
    , fromRaw
    , init
    , update
    )

import Frame3d
import Http
import Length
import Obj.Decode
import Point3d exposing (Point3d)
import Quantity
import Scene3d.Mesh
import Task exposing (Task)
import Task.Parallel
import TriangularMesh exposing (TriangularMesh)
import Util.Obj.Decode
import Vector3d exposing (Vector3d)


type ScreenCoordinates
    = ScreenCoordinates Never


type WorldCoordinates
    = WorldCoordinates Never


type alias Meshes coordinates =
    { laserTower : MeshAndShadow coordinates
    , hexTile : MeshAndShadow coordinates
    , hexTileHighlight : MeshAndShadow coordinates
    , enemySphere : EnemySphereMeshAndShadow coordinates
    , wall : WallMeshAndShadow coordinates
    , thingToProtect : ThingToProtectMeshAndShadow coordinates
    }


type alias MeshAndShadow coordinates =
    ( Scene3d.Mesh.Textured coordinates
    , Scene3d.Mesh.Shadow coordinates
    )


type alias WallMeshAndShadow coordinates =
    { core : MeshAndShadow coordinates
    , northEast : MeshAndShadow coordinates
    , east : MeshAndShadow coordinates
    , southEast : MeshAndShadow coordinates
    , southWest : MeshAndShadow coordinates
    , west : MeshAndShadow coordinates
    , northWest : MeshAndShadow coordinates
    }


type alias ThingToProtectMeshAndShadow coordinates =
    { core : MeshAndShadow coordinates
    , ringInner : MeshAndShadow coordinates
    , ringMiddle : MeshAndShadow coordinates
    , ringOuter : MeshAndShadow coordinates
    }


type alias EnemySphereMeshAndShadow coordinates =
    { core : MeshAndShadow coordinates
    , disc : MeshAndShadow coordinates
    }


type alias RawMesh coordinates =
    TriangularMesh
        { normal : Vector3d Quantity.Unitless coordinates
        , position : Point3d Length.Meters coordinates
        , uv : ( Float, Float )
        }


type alias ThingToProtectRawMesh coordinates =
    { core : RawMesh coordinates
    , ringInner : RawMesh coordinates
    , ringMiddle : RawMesh coordinates
    , ringOuter : RawMesh coordinates
    }


type alias WallRawMesh coordinates =
    { core : RawMesh coordinates
    , northEast : RawMesh coordinates
    , east : RawMesh coordinates
    , southEast : RawMesh coordinates
    , southWest : RawMesh coordinates
    , west : RawMesh coordinates
    , northWest : RawMesh coordinates
    }


type alias EnemySphereRawMesh coordinates =
    { core : RawMesh coordinates
    , disc : RawMesh coordinates
    }


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


type alias LoadingMsg coordinates =
    Task.Parallel.Msg6
        (RawMesh coordinates)
        (RawMesh coordinates)
        (RawMesh coordinates)
        (EnemySphereRawMesh coordinates)
        (WallRawMesh coordinates)
        (ThingToProtectRawMesh coordinates)


type alias Model coordinates msg =
    Task.Parallel.State6
        msg
        (RawMesh coordinates)
        (RawMesh coordinates)
        (RawMesh coordinates)
        (EnemySphereRawMesh coordinates)
        (WallRawMesh coordinates)
        (ThingToProtectRawMesh coordinates)


init :
    { onUpdates : LoadingMsg coordinates -> msg
    , onFailure : Http.Error -> msg
    , onSuccess :
        RawMesh coordinates
        -> RawMesh coordinates
        -> RawMesh coordinates
        -> EnemySphereRawMesh coordinates
        -> WallRawMesh coordinates
        -> ThingToProtectRawMesh coordinates
        -> msg
    }
    -> ( Model coordinates msg, Cmd msg )
init cfg =
    let
        namedTexturedFacesIn : String -> Obj.Decode.Decoder (RawMesh coordinates)
        namedTexturedFacesIn name =
            meshFilterNameEquals name
                (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
    in
    Task.Parallel.attempt6
        { task1 = getMesh "laser_tower" (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
        , task2 = getMesh "ground_hex" (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
        , task3 = getMesh "ground_hex_highlight" (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
        , task4 =
            getMesh "enemy_sphere"
                -- (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
                (Obj.Decode.map2 EnemySphereRawMesh
                    (namedTexturedFacesIn "Core")
                    (namedTexturedFacesIn "Disc")
                )
        , task5 =
            getMesh "wall"
                (Obj.Decode.succeed WallRawMesh
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Core")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_NE")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_E")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_SE")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_SW")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_W")
                    |> Util.Obj.Decode.andMap (namedTexturedFacesIn "Section_NW")
                )
        , task6 =
            getMesh "thing_to_protect"
                (Obj.Decode.map4 ThingToProtectRawMesh
                    (namedTexturedFacesIn "Core")
                    (namedTexturedFacesIn "Rings_Inner")
                    (namedTexturedFacesIn "Rings_Middle")
                    (namedTexturedFacesIn "Rings_Outer")
                )
        , onUpdates = cfg.onUpdates
        , onFailure = cfg.onFailure
        , onSuccess = cfg.onSuccess
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


update : LoadingMsg coordinates -> Model coordinates msg -> ( Model coordinates msg, Cmd msg )
update msg model =
    Task.Parallel.update6 model msg


initMesh : RawMesh coordinates -> MeshAndShadow coordinates
initMesh m =
    let
        mesh : Scene3d.Mesh.Textured coordinates
        mesh =
            Scene3d.Mesh.texturedFaces m
    in
    ( mesh
    , Scene3d.Mesh.shadow mesh
    )


fromRaw :
    RawMesh coordinates
    -> RawMesh coordinates
    -> RawMesh coordinates
    -> EnemySphereRawMesh coordinates
    -> WallRawMesh coordinates
    -> ThingToProtectRawMesh coordinates
    -> Meshes coordinates
fromRaw laserTowerMesh hexTileMesh hexTileHighlightMesh enemySphereMesh wallMesh thingToProtect =
    { laserTower = initMesh laserTowerMesh
    , hexTile = initMesh hexTileMesh
    , hexTileHighlight = initMesh hexTileHighlightMesh
    , enemySphere =
        { core = initMesh enemySphereMesh.core
        , disc = initMesh enemySphereMesh.disc
        }
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
