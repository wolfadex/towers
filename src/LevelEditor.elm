module LevelEditor exposing (HandedDirection(..), InitializingModel, Key, Model(..), Msg(..), PaintMode(..), ReadyModel, ScreenCoordinates(..), WorldCoordinates(..), init, subscriptions, update, view)

import Angle exposing (Angle)
import AngularSpeed exposing (AngularSpeed)
import Axis3d
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Css
import Dict
import Direction3d
import Duration exposing (Duration)
import Hex exposing (Hex)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Length
import Level exposing (Level)
import Meshes exposing (Meshes)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Ports
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material
import SketchPlane3d
import Task
import Tea exposing (Tea)
import Time
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
    , paintMode : PaintMode
    , isApplyingPaint : Bool
    , selectedProp : Level.Prop
    , levelToImport : String
    , levelToImportError : Maybe String
    }


type PaintMode
    = Add
    | Erase
    | Place
    | Remove


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
    | MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseMoved (Point2d Pixels ScreenCoordinates)
    | MouseUp
    | RotateCamera HandedDirection
    | MeshesLoading (Meshes.LoadingMsg WorldCoordinates)
    | MeshesLoadFailed Http.Error
    | MeshesLoaded (Meshes.RawMesh WorldCoordinates) (Meshes.RawMesh WorldCoordinates) (Meshes.RawMesh WorldCoordinates) (Meshes.EnemySphereRawMesh WorldCoordinates) (Meshes.WallRawMesh WorldCoordinates) (Meshes.ThingToProtectRawMesh WorldCoordinates)
    | TimeInitialized (Meshes WorldCoordinates) Time.Posix
    | PaintModeSelected PaintMode
    | PropSelected Level.Prop
    | ExportLevel
    | CopyLevelJson String
    | CancelExport
    | ImportLevel
    | LevelToImportChanged String
    | CancelImport
    | DecodeImportLevel


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
        , paintMode = Add
        , selectedProp = Level.Target
        , isApplyingPaint = False
        , meshes = cfg.meshes
        , lastTimestamp = cfg.currentTime
        , highlightedTile = Nothing
        , cameraEyePoint = Point3d.meters 25 0 35
        , cameraRotation = { current = Angle.degrees 0, destination = Angle.degrees 0 }
        , levelToImport = ""
        , levelToImportError = Nothing
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

                PaintModeSelected paintMode ->
                    { model
                        | paintMode = paintMode
                    }
                        |> Tea.save

                PropSelected prop ->
                    { model
                        | selectedProp = prop
                    }
                        |> Tea.save

                ExportLevel ->
                    model
                        |> Tea.save
                        |> Tea.withCmd (Ports.openModal "level-export")

                CopyLevelJson levelJson ->
                    model
                        |> Tea.save
                        |> Tea.withCmd (Ports.copyToClipboard levelJson)

                CancelExport ->
                    model
                        |> Tea.save
                        |> Tea.withCmd (Ports.closeModal "level-export")

                ImportLevel ->
                    { model
                        | levelToImportError = Nothing
                        , levelToImport = ""
                    }
                        |> Tea.save
                        |> Tea.withCmd (Ports.openModal "level-import")

                LevelToImportChanged levelToImport ->
                    { model
                        | levelToImport = levelToImport
                    }
                        |> Tea.save

                CancelImport ->
                    model
                        |> Tea.save
                        |> Tea.withCmd (Ports.closeModal "level-import")

                DecodeImportLevel ->
                    case Json.Decode.decodeString Level.decode model.levelToImport of
                        Err err ->
                            { model
                                | levelToImportError = Just (Json.Decode.errorToString err)
                            }
                                |> Tea.save

                        Ok level ->
                            { model
                                | level = level
                            }
                                |> Tea.save
                                |> Tea.withCmd (Ports.closeModal "level-import")

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

                    else if key == "1" && not alt && not control && not meta then
                        { model | paintMode = Add }
                            |> Tea.save

                    else if key == "2" && not alt && not control && not meta then
                        { model | paintMode = Erase }
                            |> Tea.save

                    else if key == "3" && not alt && not control && not meta then
                        { model | paintMode = Place }
                            |> Tea.save

                    else if key == "4" && not alt && not control && not meta then
                        { model | paintMode = Remove }
                            |> Tea.save

                    else if (key == "q" || key == "Q") && not alt && not control && not meta then
                        { model | selectedProp = Level.Target }
                            |> Tea.save

                    else if (key == "w" || key == "W") && not alt && not control && not meta then
                        { model | selectedProp = Level.Spawner }
                            |> Tea.save
                        -- else if (key == "e" || key == "E") && not alt && not control && not meta then
                        --     { model | selectedProp = Level.Wall }
                        --         |> Tea.save

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
                                , level =
                                    if model.isApplyingPaint then
                                        case model.paintMode of
                                            Add ->
                                                Level.addTile hex Nothing model.level

                                            Erase ->
                                                Level.removeTile hex model.level

                                            Place ->
                                                Level.addTile hex (Just model.selectedProp) model.level

                                            Remove ->
                                                Level.removeProp hex model.level

                                    else
                                        model.level
                            }
                                |> Tea.save

                MouseDown screenPoint ->
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
                            { model
                                | isApplyingPaint = True
                            }
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
                            { model
                                | isApplyingPaint = True
                                , level =
                                    case model.paintMode of
                                        Add ->
                                            Level.addTile clickedHex Nothing model.level

                                        Erase ->
                                            Level.removeTile clickedHex model.level

                                        Place ->
                                            Level.addTile clickedHex (Just model.selectedProp) model.level

                                        Remove ->
                                            Level.removeProp clickedHex model.level
                            }
                                |> Tea.save

                MouseUp ->
                    { model
                        | isApplyingPaint = False
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
                    |> (*) 75
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


viewEditorButton : Bool -> Msg -> String -> Html Msg
viewEditorButton isSelected onClick label =
    Html.button
        [ Html.Events.onClick onClick
        , if isSelected then
            Css.selectedButton

          else
            Html.Attributes.class ""
        ]
        [ Html.text label ]


viewReady : ReadyModel -> List (Html Msg)
viewReady model =
    [ Html.div
        [ Css.editorControls ]
        [ Html.label [] [ Html.text "Paint Mode:" ]
        , viewEditorButton
            (model.paintMode == Add)
            (PaintModeSelected Add)
            "Add Tile"
        , viewEditorButton
            (model.paintMode == Erase)
            (PaintModeSelected Erase)
            "Erase Tile"
        , viewEditorButton
            (model.paintMode == Place)
            (PaintModeSelected Place)
            "Place Prop"
        , viewEditorButton
            (model.paintMode == Remove)
            (PaintModeSelected Remove)
            "Remove Prop"
        , Html.label [] [ Html.text "Prop:" ]
        , viewEditorButton
            (model.selectedProp == Level.Target)
            (PropSelected Level.Target)
            "Target"
        , viewEditorButton
            (model.selectedProp == Level.Spawner)
            (PropSelected Level.Spawner)
            "Spawner"

        -- , viewEditorButton
        --     (model.selectedProp == Level.Wall)
        --     (PropSelected Level.Wall)
        --     "Wall"
        ]
    , Html.div
        [ Html.Events.on "mousedown" decodeMouseDown
        , Html.Events.on "mousemove" decodeMouseMove
        , Html.Events.on "mouseup" decodeMouseUp
        , Html.Events.on "mouseover" decodeMouseMove
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
            , Html.span []
                [ model.cameraRotation.current
                    |> Angle.inDegrees
                    |> round
                    |> String.fromInt
                    |> (\d -> d ++ "°")
                    |> Html.text
                ]
            , Html.button
                [ Html.Events.onClick (RotateCamera Right) ]
                [ Html.text "Rotate Right" ]
            ]
        ]
    , Html.div
        [ Css.saveLoadControls ]
        [ Html.button
            [ Html.Events.onClick ExportLevel ]
            [ Html.text "Export Level" ]
        , let
            levelJson : String
            levelJson =
                model.level
                    |> Level.encode
                    |> Json.Encode.encode 0
          in
          Html.node "dialog"
            [ Html.Attributes.id "level-export"
            ]
            [ Html.div [ Css.levelExport ]
                [ Html.label
                    []
                    [ Html.text "Level JSON:"
                    ]
                , Html.textarea
                    [ Html.Attributes.readonly True
                    , levelJson
                        |> Html.Attributes.value
                    ]
                    []
                , Html.br [] []
                , Html.button
                    [ Html.Events.onClick (CopyLevelJson levelJson) ]
                    [ Html.text "Copy" ]
                , Html.button
                    [ Html.Events.onClick CancelExport ]
                    [ Html.text "Close" ]
                ]
            ]
        , Html.button
            [ Html.Events.onClick ImportLevel ]
            [ Html.text "Import Level" ]
        , Html.node "dialog"
            [ Html.Attributes.id "level-import" ]
            [ Html.div [ Css.levelExport ]
                [ Html.label
                    []
                    [ Html.text "Level JSON:"
                    ]
                , Html.textarea
                    [ Html.Attributes.value model.levelToImport
                    , Html.Events.onInput LevelToImportChanged
                    ]
                    []
                , Html.br [] []
                , case model.levelToImportError of
                    Nothing ->
                        Html.text ""

                    Just error ->
                        Html.div
                            []
                            [ Html.h4 [] [ Html.text "Error parsing data" ]
                            , Html.p
                                [ Css.levelImportError ]
                                [ Html.text error ]
                            ]
                , Html.br [] []
                , Html.button
                    [ Html.Events.onClick DecodeImportLevel ]
                    [ Html.text "Import" ]
                , Html.button
                    [ Html.Events.onClick CancelImport ]
                    [ Html.text "Close" ]
                ]
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


decodeMouseDown : Json.Decode.Decoder Msg
decodeMouseDown =
    Json.Decode.map2
        (\x y ->
            Point2d.pixels x y
                |> MouseDown
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


decodeMouseUp : Json.Decode.Decoder Msg
decodeMouseUp =
    Json.Decode.succeed MouseUp


viewHexGridMap3d : Meshes WorldCoordinates -> Hex.Map (Maybe Level.Prop) -> List (Scene3d.Entity WorldCoordinates)
viewHexGridMap3d meshes hexMap =
    let
        ( meshTargetCore, shadowTargetCore ) =
            meshes.thingToProtect.core

        ( meshRingInner, shadowRingInner ) =
            meshes.thingToProtect.ringInner

        ( meshRingMiddle, shadowRingMiddle ) =
            meshes.thingToProtect.ringMiddle

        ( meshRingOuter, shadowRingOuter ) =
            meshes.thingToProtect.ringOuter

        ( meshHexTile, shadowTile ) =
            meshes.hexTile

        ( meshSpawnerCore, shadowSpawnerCore ) =
            meshes.enemySphere.core

        ( meshSpawnerDisc, shadowSpawnerDisc ) =
            meshes.enemySphere.disc

        toTileEntity : Hex.Key -> Scene3d.Entity WorldCoordinates
        toTileEntity key =
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
                meshHexTile
                shadowTile
                |> Scene3d.translateBy translateBy

        viewProp : Hex.Key -> Level.Prop -> List (Scene3d.Entity WorldCoordinates)
        viewProp key prop =
            case prop of
                Level.Target ->
                    let
                        translateBy : Vector3d Length.Meters WorldCoordinates
                        translateBy =
                            originPosition
                                |> Vector3d.from Point3d.origin

                        originPosition : Point3d Length.Meters WorldCoordinates
                        originPosition =
                            key
                                |> Hex.fromKey
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
                                meshTargetCore
                                shadowTargetCore
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
                    in
                    [ core3d, ringInner3d, ringMiddle3d, ringOuter3d ]

                Level.Wall ->
                    []

                Level.Spawner ->
                    let
                        translateBy : Vector3d Length.Meters WorldCoordinates
                        translateBy =
                            originPosition
                                |> Point3d.translateIn Direction3d.positiveZ (Length.meters 1.5)
                                |> Vector3d.from Point3d.origin

                        originPosition : Point3d Length.Meters WorldCoordinates
                        originPosition =
                            key
                                |> Hex.fromKey
                                |> Hex.toPoint2d hexMapLayout
                                |> Point3d.on SketchPlane3d.xy
                    in
                    [ Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.lightRed
                            , roughness = 0.5
                            }
                        )
                        meshSpawnerCore
                        shadowSpawnerCore
                        |> Scene3d.translateBy translateBy
                    , Scene3d.meshWithShadow
                        (Scene3d.Material.metal
                            { baseColor = Color.darkRed
                            , roughness = 0.5
                            }
                        )
                        meshSpawnerDisc
                        shadowSpawnerDisc
                        |> Scene3d.translateBy translateBy
                    ]

        toEntities : ( Hex.Key, Maybe Level.Prop ) -> List (Scene3d.Entity WorldCoordinates)
        toEntities ( key, maybeProp ) =
            toTileEntity key
                :: (case maybeProp of
                        Nothing ->
                            []

                        Just prop ->
                            viewProp key prop
                   )
    in
    hexMap
        |> Dict.toList
        |> List.concatMap toEntities


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
