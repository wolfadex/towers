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


type alias Model =
    {}



----------
-- INIT --
----------


init : { toMsg : Msg -> msg, toModel : Model -> model } -> Tea model msg
init cfg =
    {}
        |> Tea.save
        |> Tea.map cfg



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : { toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions cfg model =
    -- InitializingFailed _ ->
    --     Sub.none
    -- Ready _ ->
    --     [ Browser.Events.onAnimationFrame Tick
    --     , Browser.Events.onKeyPress decodeKeyPressed
    --     ]
    --         |> Sub.batch
    --         |> Tea.mapSub cfg
    Sub.none



------------
-- UPDATE --
------------


type
    Msg
    -- = Tick Time.Posix
    -- | KeyPressed Key
    -- | Clicked (Point2d Pixels ScreenCoordinates)
    -- | MouseMoved (Point2d Pixels ScreenCoordinates)
    -- | RotateCamera HandedDirection
    -- | TrapTypeSelected TrapType
    -- | MeshesLoading (Task.Parallel.Msg6 RawMesh RawMesh RawMesh EnemySphereRawMesh WallRawMesh ThingToProtectRawMesh)
    -- | MeshesLoadFailed Http.Error
    -- | MeshesLoaded RawMesh RawMesh RawMesh EnemySphereRawMesh WallRawMesh ThingToProtectRawMesh
    -- | TimeInitialized Meshes Time.Posix
    = NoOp


update : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> Model -> Tea model msg
update cfg msg model =
    model
        |> Tea.save
        |> Tea.map cfg



----------
-- VIEW --
----------


view : { toMsg : Msg -> msg } -> Model -> List (Html msg)
view cfg model =
    List.map (Tea.mapView cfg) <|
        [ Html.div [ Css.initializing ]
            [ Html.text "Initializing..."
            , Html.div
                [ Css.loading ]
                []
            ]
        ]
