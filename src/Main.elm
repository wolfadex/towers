module Main exposing (main)

import Browser
import Circle2d
import Dict exposing (Dict)
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
import Polyline2d
import Quantity exposing (Quantity)
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { tilemap : Hex.Map (Maybe Ecs.Entity)
    , cameraPosition : Point2d Pixels ScreenCoordinates
    , seed : Random.Seed

    -- ECS
    , ecsConfig : Ecs.Config
    , colorComponent : Ecs.Component String
    , positionComponent : Ecs.Component Hex
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


init : () -> ( Model, Cmd Msg )
init _ =
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

      -- ECS
      , ecsConfig = Ecs.Config.init
      , colorComponent = Ecs.Component.empty
      , positionComponent = Ecs.Component.empty
      }
      -- |> Ecs.Entity.create ecsConfigSpec
      -- |> Ecs.Entity.with ( colorSpec, "red" )
      -- |> Ecs.Entity.with
      --     ( positionSpec
      --     , { q = 0, r = 0, s = 0 }
      --         |> Hex.fromQRSInt
      --         |> Hex.neighbor Hex.NorthWest
      --         |> Hex.neighbor Hex.NorthWest
      --         |> Hex.neighbor Hex.West
      --         |> Hex.neighbor Hex.West
      --         |> Hex.neighbor Hex.West
      --     )
      -- |> Tuple.second
      -- |> Ecs.Entity.create ecsConfigSpec
      -- |> Ecs.Entity.with ( colorSpec, "cornflowerblue" )
      -- |> Ecs.Entity.with
      --     ( positionSpec
      --     , { q = 0, r = 0, s = 0 }
      --         |> Hex.fromQRSInt
      --     )
      -- |> Tuple.second
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
                |> Geometry.Svg.polyline2d
                    [ Svg.Attributes.stroke "orange"
                    , Svg.Attributes.strokeWidth "5"
                    , Svg.Attributes.fill "none"
                    ]
            )
                :: acc
        )
        model.positionComponent
        model.colorComponent
        []


viewCells : Quantity Float (Quantity.Rate Pixels Length.Meters) -> Model -> List (Svg Msg)
viewCells resolution model =
    Ecs.System.foldl2
        (\position color acc ->
            (position
                |> Hex.toPoint2d hexMapLayout
                |> Circle2d.withRadius (Pixels.pixels 15)
                |> Geometry.Svg.circle2d [ Svg.Attributes.fill color ]
            )
                :: acc
        )
        model.positionComponent
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
            -- let
            --     coords =
            --         hex
            --             |> Hex.toQRSInt
            --     carlCell =
            --         carlHex
            --             |> Hex.toQRSInt
            --     eastCell =
            --         carlHex
            --             |> Hex.neighbor Hex.East
            --             |> Hex.toQRSInt
            --     westCell =
            --         carlHex
            --             |> Hex.neighbor Hex.West
            --             |> Hex.toQRSInt
            -- in
            [ Svg.polygon
                [ Svg.Attributes.style "cursor: pointer"
                , Svg.Attributes.stroke "#ffff00"
                , Svg.Attributes.strokeWidth "1px"
                , Svg.Attributes.fill <|
                    -- if coords.q == carlCell.q && coords.r == carlCell.r && coords.s == carlCell.s then
                    --     "#0F8AD2"
                    -- else if coords.q == eastCell.q && coords.r == eastCell.r && coords.s == eastCell.s then
                    --     "#5FE033"
                    -- else if coords.q == westCell.q && coords.r == westCell.r && coords.s == westCell.s then
                    --     "#990099"
                    -- else
                    "#777777"
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
