module Main exposing (main)

import Basics.Extra
import Browser
import Browser.Events
import Circle2d
import Dict exposing (Dict)
import Duration exposing (Duration)
import Ecs
import Ecs.Component
import Ecs.Config
import Ecs.Entity
import Ecs.System
import Geometry.Svg
import Hex exposing (Hex)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Length
import List.Extra
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time
import Util.Maybe


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    Int



-----------
-- MODEL --
-----------


type alias Model =
    { tilemap : Hex.Map (Maybe Ecs.Entity)
    , cameraPosition : Point2d Pixels ScreenCoordinates
    , seed : Random.Seed
    , lastTimestamp : Time.Posix
    , currency : Int

    -- ECS
    , ecsConfig : Ecs.Config
    , colorComponent : Ecs.Component String
    , positionComponent : Ecs.Component Hex
    , healthComponent : Ecs.Component Health
    , pathComponent :
        Ecs.Component
            { path : List Hex
            , distance : Float
            , point : Point2d Pixels WorldCoordinates
            }
    , playerComponent : Ecs.Component Player
    , enemyComponent : Ecs.Component Enemy
    , towerComponent : Ecs.Component Tower
    }


type ScreenCoordinates
    = ScreenCoordinates Never


type WorldCoordinates
    = WorldCoordinates Never


ecsConfigSpec : Ecs.Config.Spec Model
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


colorSpec : Ecs.Component.Spec String Model
colorSpec =
    { get = .colorComponent
    , set = \colorComponent world -> { world | colorComponent = colorComponent }
    }


positionSpec : Ecs.Component.Spec Hex Model
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


type alias Health =
    { current : Int
    , max : Int
    }


healthSpec : Ecs.Component.Spec Health Model
healthSpec =
    { get = .healthComponent
    , set = \healthComponent world -> { world | healthComponent = healthComponent }
    }


pathSpec :
    Ecs.Component.Spec
        { path : List Hex
        , distance : Float
        , point : Point2d Pixels WorldCoordinates
        }
        Model
pathSpec =
    { get = .pathComponent
    , set = \pathComponent world -> { world | pathComponent = pathComponent }
    }


type Player
    = Player


playerSpec : Ecs.Component.Spec Player Model
playerSpec =
    { get = .playerComponent
    , set = \playerComponent world -> { world | playerComponent = playerComponent }
    }


type Enemy
    = Enemy


enemySpec : Ecs.Component.Spec Enemy Model
enemySpec =
    { get = .enemyComponent
    , set = \enemyComponent world -> { world | enemyComponent = enemyComponent }
    }


type Tower
    = Tower


towerSpec : Ecs.Component.Spec Tower Model
towerSpec =
    { get = .towerComponent
    , set = \towerComponent world -> { world | towerComponent = towerComponent }
    }



----------
-- INIT --
----------


init : Flags -> ( Model, Cmd Msg )
init currentTime =
    let
        tilemap =
            { q = 0, r = 0, s = 0 }
                |> Hex.fromQRSInt
                |> Hex.circle 5
                |> List.map
                    (\hex ->
                        ( Hex.toKey hex, ( hex, Nothing ) )
                    )
                |> Dict.fromList
    in
    ( { tilemap = tilemap
      , cameraPosition = Point2d.origin
      , seed = Random.initialSeed 0
      , lastTimestamp = Time.millisToPosix currentTime
      , currency = 500

      -- ECS
      , ecsConfig = Ecs.Config.init
      , colorComponent = Ecs.Component.empty
      , positionComponent = Ecs.Component.empty
      , healthComponent = Ecs.Component.empty
      , pathComponent = Ecs.Component.empty
      , playerComponent = Ecs.Component.empty
      , enemyComponent = Ecs.Component.empty
      , towerComponent = Ecs.Component.empty
      }
        |> createEnemy
        |> createEnemy
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( colorSpec, "cornflowerblue" )
        |> Ecs.Entity.with ( healthSpec, { current = 100, max = 100 } )
        |> Ecs.Entity.with ( playerSpec, Player )
        |> Ecs.Entity.with
            ( positionSpec
            , { q = 0, r = 0, s = 0 }
                |> Hex.fromQRSInt
            )
        |> Tuple.second
    , Cmd.none
    )


createEnemy : Model -> Model
createEnemy model =
    let
        edgeHexes : List Hex
        edgeHexes =
            model.tilemap
                |> Dict.values
                |> List.filterMap
                    (\( hex, _ ) ->
                        let
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
                            Hex.drawLine
                                hex
                                (Hex.fromQRSInt { q = 0, r = 0, s = 0 })
                      , distance = 0
                      , point = Hex.toPoint2d hexMapLayout hex
                      }
                    )
                |> Tuple.second


removeEnemy : Ecs.Entity -> Model -> Model
removeEnemy entity model =
    ( entity, model )
        |> Ecs.Entity.remove colorSpec
        |> Ecs.Entity.remove healthSpec
        |> Ecs.Entity.remove positionSpec
        |> Ecs.Entity.remove enemySpec
        |> Ecs.Entity.remove pathSpec
        |> Tuple.second


createTower : Hex -> Model -> Model
createTower position model =
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( colorSpec, "chartreuse" )
        |> Ecs.Entity.with ( positionSpec, position )
        |> Ecs.Entity.with ( towerSpec, Tower )
        |> Tuple.second


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrame Tick



------------
-- UPDATE --
------------


type Msg
    = Tick Time.Posix
    | HexSelected Hex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick currentTimestamp ->
            let
                deltaTime : Duration
                deltaTime =
                    (Time.posixToMillis currentTimestamp - Time.posixToMillis model.lastTimestamp)
                        |> toFloat
                        |> Duration.milliseconds
            in
            ( { model
                | lastTimestamp = currentTimestamp
              }
                |> towerAttack deltaTime
                |> moveEnemy deltaTime
                |> enemyAttack
            , Cmd.none
            )

        HexSelected hex ->
            if model.currency >= 100 then
                ( { model
                    | currency = model.currency - 100
                  }
                    |> createTower hex
                    |> createEnemy
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )


moveEnemy : Duration -> Model -> Model
moveEnemy deltaTime =
    Ecs.System.map2
        (\( position, setPosition ) ( { path, distance, point }, setPath ) ->
            let
                deltaSeconds =
                    Duration.inSeconds deltaTime

                totalDistance =
                    distance + deltaSeconds

                tilesToDrop =
                    floor totalDistance

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
                        nextDistance =
                            totalDistance
                                |> Basics.Extra.fractionalModBy 1.0

                        pointAlong : Point2d Pixels WorldCoordinates
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


enemyAttack : Model -> Model
enemyAttack model =
    let
        playerPositions =
            Ecs.System.indexedFoldl2
                (\entity position _ players -> ( entity, position ) :: players)
                model.positionComponent
                model.playerComponent
                []
    in
    Ecs.System.indexedFoldl2
        (\entity position _ nextModel ->
            case List.Extra.find (\( playerEntity, playerPos ) -> Hex.similar position playerPos) playerPositions |> Debug.log "at the end?" of
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


towerAttack : Duration -> Model -> Model
towerAttack deltaTime model =
    Ecs.System.foldl2
        (\towerPosition _ nextModel ->
            let
                hexRange =
                    towerPosition
                        |> Hex.circle 3
                        |> List.filter
                            (\hex ->
                                Dict.get (Hex.toKey hex) nextModel.tilemap /= Nothing
                            )

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
                    nextModel

                Just ( enemy, _ ) ->
                    nextModel
                        |> removeEnemy enemy
        )
        model.positionComponent
        model.towerComponent
        model


enemyPath : Model -> List (Polyline2d Pixels WorldCoordinates)
enemyPath model =
    Ecs.System.foldl2
        (\position color acc ->
            (position
                |> Hex.drawLine (Hex.fromQRSInt { q = 0, r = 0, s = 0 })
                |> List.map
                    (\point ->
                        point
                            |> Hex.toPoint2d hexMapLayout
                    )
                |> Polyline2d.fromVertices
            )
                :: acc
        )
        model.positionComponent
        model.colorComponent
        []



----------
-- VIEW --
----------


view : Model -> Browser.Document Msg
view model =
    { title = "Hello World"
    , body =
        let
            resolution : Quantity Float (Quantity.Rate Pixels Length.Meters)
            resolution =
                Pixels.pixels 1 |> Quantity.per (Length.millimeters 1)

            width : Quantity Float Pixels
            width =
                Pixels.pixels 1920

            height : Quantity Float Pixels
            height =
                Pixels.pixels 1080

            viewBox : String
            viewBox =
                String.join " "
                    [ width
                        |> Quantity.divideBy -2
                        |> Quantity.plus (Point2d.xCoordinate model.cameraPosition)
                        |> Pixels.inPixels
                        |> String.fromFloat
                    , height
                        |> Quantity.divideBy -2
                        |> Quantity.plus (Point2d.yCoordinate model.cameraPosition)
                        |> Pixels.inPixels
                        |> String.fromFloat
                    , width
                        |> Pixels.inPixels
                        |> String.fromFloat
                    , height
                        |> Pixels.inPixels
                        |> String.fromFloat
                    ]
        in
        [ Html.h1 [] [ Html.text "Tower defense" ]
        , Ecs.System.foldl2
            (\_ health acc ->
                Html.label
                    []
                    [ Html.text "Health: "
                    , Html.meter
                        [ Html.Attributes.min "0"
                        , Html.Attributes.max (String.fromInt health.max)
                        , Html.Attributes.value (String.fromInt health.current)
                        , Html.Attributes.attribute "low" (String.fromInt (health.max // 4))
                        , Html.Attributes.attribute "high" (String.fromInt (health.max // 2))
                        , Html.Attributes.attribute "optimum" (String.fromInt (health.max // 4 * 3))
                        ]
                        []
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
        , Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.viewBox viewBox
            , Html.Attributes.attribute "tabindex" "0"

            -- , Html.Events.on "keydown" decodeKeyDown
            -- , Html.Events.on "keyup" decodeKeyUp
            ]
            (List.concat
                [ [ model.tilemap
                        |> viewHexGridMap
                  ]

                -- , highlightPath model
                , viewEnemies resolution model
                , viewTowers resolution model
                ]
            )
        ]
    }


highlightPath : Model -> List (Svg Msg)
highlightPath model =
    model
        |> enemyPath
        |> List.map
            (Geometry.Svg.polyline2d
                [ Svg.Attributes.stroke "orange"
                , Svg.Attributes.strokeWidth "5"
                , Svg.Attributes.fill "none"
                ]
            )


viewEnemies : Quantity Float (Quantity.Rate Pixels Length.Meters) -> Model -> List (Svg Msg)
viewEnemies resolution model =
    Ecs.System.foldl3
        (\{ point } color _ acc ->
            (point
                |> Circle2d.withRadius (Pixels.pixels 15)
                |> Geometry.Svg.circle2d [ Svg.Attributes.fill color ]
            )
                :: acc
        )
        model.pathComponent
        model.colorComponent
        model.enemyComponent
        []


viewTowers : Quantity Float (Quantity.Rate Pixels Length.Meters) -> Model -> List (Svg Msg)
viewTowers resolution model =
    Ecs.System.foldl3
        (\position color _ acc ->
            (position
                |> Hex.toPoint2d hexMapLayout
                |> Circle2d.withRadius (Pixels.pixels 23)
                |> Geometry.Svg.circle2d [ Svg.Attributes.fill color ]
            )
                :: acc
        )
        model.positionComponent
        model.colorComponent
        model.towerComponent
        []


hexMapLayout : Hex.Layout
hexMapLayout =
    { orientation = Hex.pointyOrientation
    , size = ( 32, 32 )
    , origin = ( -160, -160 )
    }


viewHexGridMap : Hex.Map (Maybe Ecs.Entity) -> Svg Msg
viewHexGridMap hexMap =
    let
        toSvg : Hex -> String -> Svg Msg
        toSvg hex cornersCoords =
            Svg.g
                []
                (toPolygon hex cornersCoords)

        toPolygon : Hex -> String -> List (Svg Msg)
        toPolygon hex cornersCoords =
            [ Svg.polygon
                [ Svg.Attributes.style "cursor: pointer"
                , Svg.Attributes.stroke "#ffff00"
                , Svg.Attributes.strokeWidth "1px"
                , Svg.Attributes.fill "#777777"
                , Svg.Attributes.points cornersCoords
                , Svg.Events.onClick (HexSelected hex)
                ]
                []
            ]
    in
    hexMap
        |> Dict.values
        |> List.map
            (\( hex, _ ) ->
                hex
                    |> mapPolygonCorners
                    |> pointsToString
                    |> toSvg hex
            )
        |> Svg.g []


{-| Helper to convert points to SVG string coordinates
-}
pointsToString : List ( Float, Float ) -> String
pointsToString points =
    String.join " " (List.map pointToStringCoords points)


{-| Helper to convert points to SVG string coordinates
-}
pointToStringCoords : ( Float, Float ) -> String
pointToStringCoords ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


mapPolygonCorners : Hex -> List ( Float, Float )
mapPolygonCorners =
    Hex.polygonCorners hexMapLayout
