module Game exposing
    ( AttackAnimation(..)
    , AttackStyle(..)
    , Delay
    , Enemy(..)
    , Health
    , Model
    , Msg(..)
    , Player(..)
    , ScreenCoordinates(..)
    , Tower(..)
    , TowerCreationType
    , Wave(..)
    , WorldCoordinates(..)
    , init
    , subscriptions
    , update
    , view
    )

import AStar.Generalised
import Basics.Extra
import Browser.Events
import Circle2d
import Css
import Css.Color
import Dict
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
import LineSegment2d
import List.Extra
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Random
import Random.List
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tea exposing (Tea)
import Time
import Util.Ecs.Component
import Util.Maybe



-----------
-- MODEL --
-----------


type alias Model =
    { tilemap : Hex.Map (Maybe Ecs.Entity)
    , cameraPosition : Point2d Pixels ScreenCoordinates
    , seed : Random.Seed
    , lastTimestamp : Time.Posix
    , currency : Int
    , waves : Maybe Wave
    , tillNextWave : Maybe { initial : Duration, remaining : Duration }
    , towerCreationType : TowerCreationType

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
    , delayComponent : Ecs.Component Delay
    , attackStyleComponent : Ecs.Component AttackStyle
    , attackAnimationComponent : Ecs.Component AttackAnimation
    }


type TowerCreationType
    = AttackTower
    | Wall


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


type alias Delay =
    { between : Duration
    , remaining : Duration
    }


delaySpec : Ecs.Component.Spec Delay Model
delaySpec =
    { get = .delayComponent
    , set = \delayComponent world -> { world | delayComponent = delayComponent }
    }


type AttackStyle
    = Laser


attackStyleSpec : Ecs.Component.Spec AttackStyle Model
attackStyleSpec =
    { get = .attackStyleComponent
    , set = \attackStyleComponent world -> { world | attackStyleComponent = attackStyleComponent }
    }


type AttackAnimation
    = NoAttack
    | Attacking
        { from : Point2d Pixels WorldCoordinates
        , to : Point2d Pixels WorldCoordinates
        , duration : Duration
        }


attackAnimationSpec : Ecs.Component.Spec AttackAnimation Model
attackAnimationSpec =
    { get = .attackAnimationComponent
    , set = \attackAnimationComponent world -> { world | attackAnimationComponent = attackAnimationComponent }
    }



----------
-- INIT --
----------


init : { toMsg : Msg -> msg, toModel : Model -> model, currentTime : Int } -> Tea model msg
init cfg =
    let
        tilemap : Hex.Map (Maybe Ecs.Entity)
        tilemap =
            Hex.origin
                |> Hex.circle 5
                |> List.map (\hex -> ( Hex.toKey hex, Nothing ))
                |> Dict.fromList
    in
    { tilemap = tilemap
    , cameraPosition = Point2d.origin
    , seed = Random.initialSeed cfg.currentTime
    , lastTimestamp = Time.millisToPosix cfg.currentTime
    , currency = 500
    , waves =
        FinalWave 7
            |> NextWave 5 (Duration.seconds 10)
            |> NextWave 3 (Duration.seconds 10)
            |> NextWave 2 (Duration.seconds 10)
            |> Just
    , tillNextWave =
        Just
            { initial = Duration.seconds 1
            , remaining = Duration.seconds 1
            }
    , towerCreationType = AttackTower

    -- ECS
    , ecsConfig = Ecs.Config.init
    , colorComponent = Ecs.Component.empty
    , positionComponent = Ecs.Component.empty
    , healthComponent = Ecs.Component.empty
    , pathComponent = Ecs.Component.empty
    , playerComponent = Ecs.Component.empty
    , enemyComponent = Ecs.Component.empty
    , towerComponent = Ecs.Component.empty
    , delayComponent = Ecs.Component.empty
    , attackStyleComponent = Ecs.Component.empty
    , attackAnimationComponent = Ecs.Component.empty
    }
        |> createPlayer
        |> Tea.save
        |> Tea.map cfg


type Wave
    = FinalWave Int
    | NextWave Int Duration Wave


createPlayer : Model -> Model
createPlayer model =
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( colorSpec, "cornflowerblue" )
        |> Ecs.Entity.with ( healthSpec, { current = 100, max = 100 } )
        |> Ecs.Entity.with ( playerSpec, Player )
        |> Ecs.Entity.with ( positionSpec, Hex.origin )
        |> Tuple.second


createEnemy : Model -> Model
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
    , tilemap : Hex.Map (Maybe Ecs.Entity)
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

                            Just (Just _) ->
                                Nothing

                            Just Nothing ->
                                Just (Hex.toKey neighbor)
                    )
                |> Set.fromList
        )
        (Hex.toKey cfg.from)
        (Hex.toKey cfg.to)
        |> Maybe.withDefault []
        |> List.map Hex.fromKey


removeEnemy : Ecs.Entity -> Model -> Model
removeEnemy entity model =
    ( entity, { model | currency = model.currency + 10 } )
        |> Ecs.Entity.remove colorSpec
        |> Ecs.Entity.remove healthSpec
        |> Ecs.Entity.remove positionSpec
        |> Ecs.Entity.remove enemySpec
        |> Ecs.Entity.remove pathSpec
        |> Tuple.second


createAttackTower : Hex -> Model -> ( Ecs.Entity, Model )
createAttackTower position model =
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( colorSpec, "chartreuse" )
        |> Ecs.Entity.with ( towerSpec, Tower )
        |> Ecs.Entity.with ( attackStyleSpec, Laser )
        |> Ecs.Entity.with ( positionSpec, position )
        |> Ecs.Entity.with
            ( delaySpec
            , { between = Duration.seconds 2
              , remaining = Duration.seconds 0
              }
            )


createWall : Hex -> Model -> ( Ecs.Entity, Model )
createWall position model =
    model
        |> Ecs.Entity.create ecsConfigSpec
        |> Ecs.Entity.with ( colorSpec, "black" )
        |> Ecs.Entity.with ( towerSpec, Tower )
        |> Ecs.Entity.with ( positionSpec, position )



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : { toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions cfg _ =
    Browser.Events.onAnimationFrame Tick
        |> Tea.mapSub cfg



------------
-- UPDATE --
------------


type Msg
    = Tick Time.Posix
    | HexSelected Hex
    | TowerTypeSelected TowerCreationType


update : { toMsg : Msg -> msg, toModel : Model -> model } -> Msg -> Model -> Tea model msg
update cfg msg model =
    Tea.map cfg <|
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
                    |> towerAttack deltaTime
                    |> moveEnemy deltaTime
                    |> enemyAttack
                    |> Tea.save

            TowerTypeSelected towerCreationType ->
                { model
                    | towerCreationType = towerCreationType
                }
                    |> Tea.save

            HexSelected hex ->
                case model.towerCreationType of
                    AttackTower ->
                        createTower createAttackTower 100 hex model

                    Wall ->
                        createTower createWall 50 hex model


createTower : (Hex -> Model -> ( Ecs.Entity, Model )) -> Int -> Hex -> Model -> Tea Model Msg
createTower createFn cost hex model =
    if model.currency >= cost then
        let
            key : Hex.Key
            key =
                Hex.toKey hex
        in
        case Dict.get key model.tilemap of
            Nothing ->
                model
                    |> Tea.save

            Just (Just _) ->
                model
                    |> Tea.save

            Just Nothing ->
                { model
                    | currency = model.currency - cost
                }
                    |> createFn hex
                    |> (\( tower, m ) ->
                            { m
                                | tilemap =
                                    Dict.insert
                                        key
                                        (Just tower)
                                        model.tilemap
                            }
                       )
                    |> Tea.save

    else
        model
            |> Tea.save


applyWave : Duration -> Model -> Model
applyWave deltaTime model =
    let
        createEnemies : Int -> Maybe Duration -> Maybe Wave -> Model
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


attackAnimationUpdate : Duration -> Model -> Model
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


moveEnemy : Duration -> Model -> Model
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


towerAttack : Duration -> Model -> Model
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
        model.towerComponent
        model.delayComponent
        model



----------
-- VIEW --
----------


view : { toMsg : Msg -> msg } -> Model -> List (Html msg)
view cfg model =
    List.map (Tea.mapView cfg) <|
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
            , Html.div []
                [ Html.button
                    [ Html.Events.onClick (TowerTypeSelected AttackTower)
                    ]
                    [ Html.text "Tower" ]
                , Html.button
                    [ Html.Events.onClick (TowerTypeSelected Wall)
                    ]
                    [ Html.text "Wall" ]
                ]
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
                , viewEnemies resolution model
                , viewTowers resolution model
                , viewPlayers resolution model
                , viewAttacks resolution model
                ]
            )
        ]


viewAttacks : Quantity Float (Quantity.Rate Pixels Length.Meters) -> Model -> List (Svg Msg)
viewAttacks resolution model =
    Ecs.System.foldl2
        (\animation style acc ->
            case animation of
                NoAttack ->
                    acc

                Attacking { from, to } ->
                    case style of
                        Laser ->
                            (LineSegment2d.from from to
                                |> Geometry.Svg.lineSegment2d
                                    [ Svg.Attributes.stroke "red"
                                    , Svg.Attributes.strokeWidth "5"
                                    ]
                            )
                                :: acc
        )
        model.attackAnimationComponent
        model.attackStyleComponent
        []


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


viewPlayers : Quantity Float (Quantity.Rate Pixels Length.Meters) -> Model -> List (Svg Msg)
viewPlayers resolution model =
    Ecs.System.foldl3
        (\_ position color acc ->
            (position
                |> Hex.toPoint2d hexMapLayout
                |> Circle2d.withRadius (Pixels.pixels 23)
                |> Geometry.Svg.circle2d [ Svg.Attributes.fill color ]
            )
                :: acc
        )
        model.playerComponent
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
        toSvg : Hex -> Maybe Ecs.Entity -> String -> Svg Msg
        toSvg hex maybeEntity cornersCoords =
            Svg.g
                []
                (toPolygon hex maybeEntity cornersCoords)

        toPolygon : Hex -> Maybe Ecs.Entity -> String -> List (Svg Msg)
        toPolygon hex maybeEntity cornersCoords =
            [ Svg.polygon
                ((case maybeEntity of
                    Nothing ->
                        [ Svg.Attributes.style "cursor: pointer"
                        , Svg.Events.onClick (HexSelected hex)
                        ]

                    Just _ ->
                        []
                 )
                    ++ [ Svg.Attributes.stroke "#ffff00"
                       , Svg.Attributes.strokeWidth "1px"
                       , Svg.Attributes.fill "#777777"
                       , Svg.Attributes.points cornersCoords
                       ]
                )
                []
            ]
    in
    hexMap
        |> Dict.toList
        |> List.map
            (\( key, maybeEntity ) ->
                let
                    hex : Hex
                    hex =
                        Hex.fromKey key
                in
                hex
                    |> mapPolygonCorners
                    |> pointsToString
                    |> toSvg hex maybeEntity
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
