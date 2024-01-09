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
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes
import Time


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

      -- ECS
      , ecsConfig = Ecs.Config.init
      , colorComponent = Ecs.Component.empty
      , positionComponent = Ecs.Component.empty
      , healthComponent = Ecs.Component.empty
      , pathComponent = Ecs.Component.empty
      }
        |> createEnemy
        |> createEnemy
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( colorSpec, "cornflowerblue" )
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrame Tick



------------
-- UPDATE --
------------


type Msg
    = Tick Time.Posix


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
                |> moveEnemy deltaTime
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

                [ _ ] ->
                    position
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
                    { path = remainingPath
                    , distance = nextDistance
                    , point = pointAlong
                    }
                        |> setPath
        )
        positionSpec
        pathSpec


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
        [ Html.h1 [] [ Html.text "Hello World" ]
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
                , viewCells resolution model
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


viewCells : Quantity Float (Quantity.Rate Pixels Length.Meters) -> Model -> List (Svg Msg)
viewCells resolution model =
    Ecs.System.foldl2
        (\{ point } color acc ->
            (point
                |> Circle2d.withRadius (Pixels.pixels 15)
                |> Geometry.Svg.circle2d [ Svg.Attributes.fill color ]
            )
                :: acc
        )
        model.pathComponent
        model.colorComponent
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
