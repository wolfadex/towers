module Hex exposing
    ( Hex, Direction(..), Key
    , toQ, toQInt, toR, toRInt, toS, toSInt
    , toQRS, toQRSInt, fromQRSInt
    , fromInt, fromFloat, toIntCoordinates, toFloatCoordinates
    , similar, dissimilar
    , plus, minus, multiplyBy
    , length, distance
    , direction, neighbor, neighbors
    , Layout, Orientation, Square2Matrix
    , flatOrientation, pointyOrientation
    , drawLine
    , fromPoint2d, toPoint2d
    , polygonCorners
    , Map, toKey
    , rectangularFlatTopMap, rectangularPointyTopMap, circle, ring
    )

{-| We treat Cube and Axial systems separately. Cube coordinates are a plane in x,y,z space, where x+y+z = 0. Axial coordinates have two axes q,r that are 60° or 120° apart.

See <http://www.redblobgames.com/grids/hexagons/implementation.html>

Forked from <https://github.com/Voronchuk/hexagons>


# Types

@docs Hex, Direction, Key


# Helpers

@docs toQ, toQInt, toR, toRInt, toS, toSInt
@docs toQRS, toQRSInt, fromQRSInt
@docs fromInt, fromFloat, toIntCoordinates, toFloatCoordinates


# Equality

@docs similar, dissimilar


# Coordinate arithmetic

@docs plus, minus, multiplyBy


# Distance

@docs length, distance


# Neighbors

@docs direction, neighbor, neighbors

#Layout

@docs Layout, Orientation, Square2Matrix
@docs flatOrientation, pointyOrientation

@docs drawLine

@docs fromPoint2d, toPoint2d
@docs polygonCorners

@docs Map, toKey


## Pre-defined map shapes

@docs rectangularFlatTopMap, rectangularPointyTopMap, circle, ring

-}

import Dict exposing (Dict)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)



--
--
-- HEX
--
--


{-| Cubic coordinates
-}
type alias CubeCoords number =
    ( number, number, number )


type alias FloatCubeCoords =
    CubeCoords Float


type alias IntCubeCoords =
    CubeCoords Int


{-| AxialCoords coordinates of an hexagon with a grid
-}
type alias AxialCoords =
    ( Int, Int )


{-| Generic hex field definition
-}
type Hex
    = FloatCubeHex FloatCubeCoords
    | IntCubeHex IntCubeCoords
    | AxialHex AxialCoords


{-| Direction ranges from 0 to 5 by sides of the hexagon, we use North, South, West, East definitions for simplicity
-}
type Direction
    = NorthEast
    | East
    | SouthEast
    | SouthWest
    | West
    | NorthWest


{-| Get q coordinate for Hex as Float value

    toQ IntCubeHex ( 2, 3, -5 ) == 2.0

-}
toQ : Hex -> Float
toQ a =
    case a of
        AxialHex ( a1, _ ) ->
            toFloat a1

        IntCubeHex ( a1, _, _ ) ->
            toFloat a1

        FloatCubeHex ( a1, _, _ ) ->
            a1


{-| Get q coordinate for Hex as Int value, its generally not recommended to use on FloatCubeHex

    toQInt IntCubeHex ( 2, 3, -5 ) == 2

-}
toQInt : Hex -> Int
toQInt a =
    case a of
        AxialHex ( a1, _ ) ->
            a1

        IntCubeHex ( a1, _, _ ) ->
            a1

        FloatCubeHex ( a1, _, _ ) ->
            floor a1


{-| Get r coordinate for Hex as Float value

    toR IntCubeHex ( 2, 3, -5 ) == 3.0

-}
toR : Hex -> Float
toR a =
    case a of
        AxialHex ( _, a2 ) ->
            toFloat a2

        IntCubeHex ( _, a2, _ ) ->
            toFloat a2

        FloatCubeHex ( _, a2, _ ) ->
            a2


{-| Get r coordinate for Hex as Int value, its generally not recommended to use on FloatCubeHex

    toRInt IntCubeHex ( 2, 3, -5 ) == 3

-}
toRInt : Hex -> Int
toRInt a =
    case a of
        AxialHex ( _, a2 ) ->
            a2

        IntCubeHex ( _, a2, _ ) ->
            a2

        FloatCubeHex ( _, a2, _ ) ->
            floor a2


{-| Get s coordinate for Hex as Float value

    toS IntCubeHex ( 2, 3, -5 ) == -5.0

-}
toS : Hex -> Float
toS a =
    case a of
        AxialHex ( a1, a2 ) ->
            toFloat (-a1 - a2)

        IntCubeHex ( _, _, a3 ) ->
            toFloat a3

        FloatCubeHex ( _, _, a3 ) ->
            a3


{-| Get s coordinate for Hex as Int value, its generally not recommended to use on FloatCubeHex

    toSInt IntCubeHex ( 2, 3, -5 ) == 3

-}
toSInt : Hex -> Int
toSInt a =
    case a of
        AxialHex ( a1, a2 ) ->
            -a1 - a2

        IntCubeHex ( _, _, a3 ) ->
            a3

        FloatCubeHex ( _, _, a3 ) ->
            floor a3


toQRS : Hex -> { q : Float, r : Float, s : Float }
toQRS hex =
    { q = toQ hex
    , r = toR hex
    , s = toS hex
    }


toQRSInt : Hex -> { q : Int, r : Int, s : Int }
toQRSInt hex =
    { q = toQInt hex
    , r = toRInt hex
    , s = toSInt hex
    }


fromQRSInt : { q : Int, r : Int, s : Int } -> Hex
fromQRSInt { q, r, s } =
    IntCubeHex ( q, r, s )


{-| Build Hex object from Int coordinates

    fromInt ( 2, 3 )
        |> similar (IntCubeHex ( 2, 3, -5 ))

-}
fromInt : ( Int, Int ) -> Hex
fromInt ( q_, r_ ) =
    IntCubeHex ( q_, r_, -q_ - r_ )


{-| Build Hex object from Float coordinates

    fromFloat ( 2.5, 3.5 )
        |> similar (FloatCubeHex ( 2.5, 3.5, -6.0 ))

-}
fromFloat : ( Float, Float ) -> Hex
fromFloat ( q_, r_ ) =
    FloatCubeHex ( q_, r_, -q_ - r_ )


{-| Convert Hex to IntCubeHex coordinate systems

    FloatCubeHex ( 2.5, 3.5, -6.0 )
        |> toIntCoordinates
        |> similar (IntCubeHex ( 2, 4, -6 ))

-}
toIntCoordinates : Hex -> Hex
toIntCoordinates hex =
    case hex of
        AxialHex ( q1, r1 ) ->
            IntCubeHex ( q1, r1, -q1 - r1 )

        IntCubeHex _ ->
            hex

        FloatCubeHex ( q_, r_, s_ ) ->
            let
                q1 =
                    round q_

                r1 =
                    round r_

                s1 =
                    round s_

                q_diff =
                    abs (toFloat q1 - q_)

                r_diff =
                    abs (toFloat r1 - r_)
            in
            if q_diff > r_diff then
                IntCubeHex ( -r1 - s1, r1, s1 )

            else
                let
                    s_diff =
                        abs (toFloat s1 - s_)
                in
                if r_diff > s_diff then
                    IntCubeHex ( q1, -q1 - s1, s1 )

                else
                    IntCubeHex ( q1, r1, -q1 - r1 )


{-| Convert Hex to FloatCubeHex coordinate systems

    IntCubeHex ( 2, 3, -5 )
        |> toFloatCoordinates
        |> similar (FloatCubeHex ( 2.0, 3.0, -5.0 ))

-}
toFloatCoordinates : Hex -> Hex
toFloatCoordinates hex =
    case hex of
        AxialHex ( q1, r1 ) ->
            let
                q_ =
                    toFloat q1

                r_ =
                    toFloat r1
            in
            FloatCubeHex ( q_, r_, -q_ - r_ )

        IntCubeHex ( q1, r1, s1 ) ->
            FloatCubeHex ( toFloat q1, toFloat r1, toFloat s1 )

        FloatCubeHex _ ->
            hex


{-| Compare two Hex definitions, support both axial and cubic coordinates.

Not a strict comparation, FloatCubeHex is converted to IntCubeHex.

    IntCubeHex ( 2, 3, -5 )
        |> similar (IntCubeHex ( 2, 3, -5 ))

    AxialHex ( 2, 3 )
        |> similar (AxialHex ( 2, 3 ))

-}
similar : Hex -> Hex -> Bool
similar a b =
    case a of
        AxialHex ( a1, a2 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    a1 == b1 && a2 == b2

                IntCubeHex ( b1, b2, b3 ) ->
                    a1 == b1 && a2 == b2 && (-a1 - a2) == b3

                FloatCubeHex _ ->
                    let
                        b_ =
                            toIntCoordinates b

                        b1_ =
                            toQInt b_

                        b2_ =
                            toRInt b_

                        b3_ =
                            toSInt b_
                    in
                    a1 == b1_ && a2 == b2_ && (-a1 - a2) == b3_

        IntCubeHex ( a1, a2, a3 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    a1 == b1 && a2 == b2 && a3 == (-b1 - b2)

                IntCubeHex ( b1, b2, b3 ) ->
                    a1 == b1 && a2 == b2 && a3 == b3

                FloatCubeHex _ ->
                    let
                        b_ =
                            toIntCoordinates b

                        b1_ =
                            toQInt b_

                        b2_ =
                            toRInt b_

                        b3_ =
                            toSInt b_
                    in
                    a1 == b1_ && a2 == b2_ && a3 == b3_

        FloatCubeHex ( a1, a2, a3 ) ->
            let
                a_ =
                    toIntCoordinates a

                a1_ =
                    toQInt a_

                a2_ =
                    toRInt a_

                a3_ =
                    toSInt a_
            in
            case b of
                AxialHex ( b1, b2 ) ->
                    a1_ == b1 && a2_ == b2 && a3_ == -b1 - b2

                IntCubeHex ( b1, b2, b3 ) ->
                    a1_ == b1 && a2_ == b2 && a3_ == b3

                FloatCubeHex ( b1, b2, b3 ) ->
                    a1 == b1 && a2 == b2 && a3 == b3


{-| Compare two Hex definitions, if they are not equal, inversion of `similar`

    IntCubeHex ( 2, 3, -5 )
        |> dissimilar (IntCubeHex ( 1, 1, -2 ))

    AxialHex ( 2, 3 )
        |> dissimilar (AxialHex ( 2, 1 ))

-}
dissimilar : Hex -> Hex -> Bool
dissimilar a b =
    similar a b
        |> not


{-| Since cube coordinates come from 3d cartesian coordinates, you automatically get things like addition, subtraction, multiplication, and division. For example, you can have Hex(2, 0, -2) that represents two steps northeast, and plus that to location Hex(3, -5, 2) the obvious way: Hex(2 + 3, 0 + -5, -2 + -2). With other coordinate systems like offset coordinates, you can’t do that and get what you want. These operations are just what you’d implement with 3d cartesian vectors, but I am using q, r, s names in this class instead of x, y, z

    IntCubeHex ( 2, 3, -5 )
        |> plus (IntCubeHex ( 1, 2, -3 ))
        |> similar (IntCubeHex ( 3, 5, -8 ))

-}
plus : Hex -> Hex -> Hex
plus a b =
    case a of
        AxialHex ( a1, a2 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    IntCubeHex ( a1 + b1, a2 + b2, (-a1 - a2) + (-b1 - b2) )

                IntCubeHex ( b1, b2, b3 ) ->
                    IntCubeHex ( a1 + b1, a2 + b2, (-a1 - a2) + b3 )

                FloatCubeHex ( b1, b2, b3 ) ->
                    let
                        a1_ =
                            toFloat a1

                        a2_ =
                            toFloat a2

                        a3_ =
                            -a1_ - a2_
                    in
                    FloatCubeHex ( a1_ + b1, a2_ + b2, a3_ + b3 )

        IntCubeHex ( a1, a2, a3 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    IntCubeHex ( a1 + b1, a2 + b2, a3 + (-b1 - b2) )

                IntCubeHex ( b1, b2, b3 ) ->
                    IntCubeHex ( a1 + b1, a2 + b2, a3 + b3 )

                FloatCubeHex ( b1, b2, b3 ) ->
                    let
                        a1_ =
                            toFloat a1

                        a2_ =
                            toFloat a2

                        a3_ =
                            toFloat a3
                    in
                    FloatCubeHex ( a1_ + b1, a2_ + b2, a3_ + b3 )

        FloatCubeHex ( a1, a2, a3 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    let
                        b1_ =
                            toFloat b1

                        b2_ =
                            toFloat b2

                        b3_ =
                            -b1_ - b2_
                    in
                    FloatCubeHex ( a1 + b1_, a2 + b2_, a3 + b3_ )

                IntCubeHex ( b1, b2, b3 ) ->
                    let
                        b1_ =
                            toFloat b1

                        b2_ =
                            toFloat b2

                        b3_ =
                            toFloat b3
                    in
                    FloatCubeHex ( a1 + b1_, a2 + b2_, a3 + b3_ )

                FloatCubeHex ( b1, b2, b3 ) ->
                    FloatCubeHex ( a1 + b1, a2 + b2, a3 + b3 )


{-| Subtraction of Hexes, more info in `sum` description

    IntCubeHex ( 1, 2, -3 )
        |> minus (IntCubeHex ( 2, 3, -5 ))
        |> similar (IntCubeHex ( 1, 1, -2 ))

-}
minus : Hex -> Hex -> Hex
minus a b =
    case a of
        AxialHex ( a1, a2 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    IntCubeHex ( a1 - b1, a2 - b2, (-a1 - a2) - (-b1 - b2) )

                IntCubeHex ( b1, b2, b3 ) ->
                    IntCubeHex ( a1 - b1, a2 - b2, (-a1 - a2) - b3 )

                FloatCubeHex ( b1, b2, b3 ) ->
                    let
                        a1_ =
                            toFloat a1

                        a2_ =
                            toFloat a2

                        a3_ =
                            -a1_ - a2_
                    in
                    FloatCubeHex ( a1_ - b1, a2_ - b2, a3_ - b3 )

        IntCubeHex ( a1, a2, a3 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    IntCubeHex ( a1 - b1, a2 - b2, a3 - (-b1 - b2) )

                IntCubeHex ( b1, b2, b3 ) ->
                    IntCubeHex ( a1 - b1, a2 - b2, a3 - b3 )

                FloatCubeHex ( b1, b2, b3 ) ->
                    let
                        a1_ =
                            toFloat a1

                        a2_ =
                            toFloat a2

                        a3_ =
                            toFloat a3
                    in
                    FloatCubeHex ( a1_ - b1, a2_ - b2, a3_ - b3 )

        FloatCubeHex ( a1, a2, a3 ) ->
            case b of
                AxialHex ( b1, b2 ) ->
                    let
                        b1_ =
                            toFloat b1

                        b2_ =
                            toFloat b2

                        b3_ =
                            -b1_ - b2_
                    in
                    FloatCubeHex ( a1 - b1_, a2 - b2_, a3 - b3_ )

                IntCubeHex ( b1, b2, b3 ) ->
                    let
                        b1_ =
                            toFloat b1

                        b2_ =
                            toFloat b2

                        b3_ =
                            toFloat b3
                    in
                    FloatCubeHex ( a1 - b1_, a2 - b2_, a3 - b3_ )

                FloatCubeHex ( b1, b2, b3 ) ->
                    FloatCubeHex ( a1 - b1, a2 - b2, a3 - b3 )


{-| Multiplication of Hexes, more info in `sum` description

    IntCubeHex ( 2, 3, -5 )
        |> mult 5
        |> similar (IntCubeHex ( 10, 15, -25 ))

-}
multiplyBy : Int -> Hex -> Hex
multiplyBy k a =
    case a of
        AxialHex ( a1, a2 ) ->
            AxialHex ( a1 * k, a2 * k )

        IntCubeHex ( a1, a2, a3 ) ->
            IntCubeHex ( a1 * k, a2 * k, a3 * k )

        FloatCubeHex ( a1, a2, a3 ) ->
            let
                k_ =
                    toFloat k
            in
            FloatCubeHex ( a1 * k_, a2 * k_, a3 * k_ )


{-| Length of Hex.

    length (IntCubeHex ( 2, 3, -5 )) == 5

    length (FloatCubeHex ( 2.2, 3.3, -5.5 )) == 5

-}
length : Hex -> Int
length a =
    let
        a1 =
            abs <| toQ a

        a2 =
            abs <| toR a

        a3 =
            abs <| toS a
    in
    floor <| (a1 + a2 + a3) / 2


{-| The distance between two hexes is the length of the line between them.

    distance (IntCubeHex ( 2, 3, -5 )) (FloatCubeHex ( 3.2, 4.3, -7.5 )) == 2

-}
distance : Hex -> Hex -> Int
distance a b =
    minus a b
        |> length


{-| Direction relative to Hex polygon lines, we used shortcuts for the mix of North, East, South, West directions
-}
direction : Direction -> Hex
direction dir =
    case dir of
        East ->
            IntCubeHex ( 1, 0, -1 )

        NorthEast ->
            IntCubeHex ( 1, -1, 0 )

        NorthWest ->
            IntCubeHex ( 0, -1, 1 )

        West ->
            IntCubeHex ( -1, 0, 1 )

        SouthWest ->
            IntCubeHex ( -1, 1, 0 )

        SouthEast ->
            IntCubeHex ( 0, 1, -1 )


{-| With distance, we defined two functions: length works on one argument and distance works with two. The same is true with neighbors. The direction function is with one argument and the neighbor function is with two.

    IntCubeHex ( 2, 3, -5 )
        |> neighbor NorthWest
        |> similar (IntCubeHex ( 2, 4, -6 ))

-}
neighbor : Direction -> Hex -> Hex
neighbor dir hex =
    plus hex <| direction dir


{-| The immediate neighbors of a hex
-}
neighbors : Hex -> List Hex
neighbors hex =
    ring 1 hex



--
--
-- LAYOUT
--
--


{-| 2x2 matrix, by x and y coordinates
-}
type alias Square2Matrix =
    { f0 : Float
    , f1 : Float
    , f2 : Float
    , f3 : Float
    }


{-| Orientation helper type to store these: the 2×2 forward matrix, the 2×2 inverse matrix, and the starting angle
-}
type alias Orientation =
    { forward_matrix : Square2Matrix
    , inverse_matrix : Square2Matrix
    , start_angle : Float
    }


{-| Composite layout definition
-}
type alias Layout =
    { orientation : Orientation
    , size : ( Float, Float )
    , origin : ( Float, Float )
    }


{-| Round Float number to some division
-}
precision : Int -> Float -> Float
precision division number =
    let
        k =
            toFloat <| 10 ^ division
    in
    (toFloat << round) (number * k) / k


{-| Constant definition of pointy-top hexagon orientation
-}
pointyOrientation : Orientation
pointyOrientation =
    { forward_matrix =
        { f0 = sqrt 3.0
        , f1 = sqrt 3.0 / 2.0
        , f2 = 0.0
        , f3 = 3.0 / 2.0
        }
    , inverse_matrix =
        { f0 = sqrt 3.0 / 3.0
        , f1 = -1.0 / 3.0
        , f2 = 0.0
        , f3 = 2.0 / 3.0
        }
    , start_angle = 0.5
    }


{-| Constant definition for flat-top hexagon orientation
-}
flatOrientation : Orientation
flatOrientation =
    { forward_matrix =
        { f0 = 3.0 / 2.0
        , f1 = 0.0
        , f2 = sqrt 3.0 / 2
        , f3 = sqrt 3
        }
    , inverse_matrix =
        { f0 = 2.0 / 3.0
        , f1 = 0.0
        , f2 = -1.0 / 3.0
        , f3 = 1.0 / sqrt 3
        }
    , start_angle = 0
    }


{-| Turn Hex coordinates into a Point location on a Layout
-}
toPoint2d : Layout -> Hex -> Point2d Pixels coordinates
toPoint2d layout hex =
    let
        { f0, f1, f2, f3 } =
            layout.orientation.forward_matrix

        ( xl, yl ) =
            layout.size

        ( xo, yo ) =
            layout.origin

        q =
            toQ hex

        r =
            toR hex

        x =
            precision 2 <| (((f0 * q) + (f1 * r)) * xl) + xo

        y =
            precision 2 <| (((f2 * q) + (f3 * r)) * yl) + yo
    in
    Point2d.pixels x y


{-| Turn Point coordinates on a Layout into a Hex coordinates
-}
fromPoint2d : Layout -> Point2d Pixels coordinates -> Hex
fromPoint2d layout point =
    let
        { f0, f1, f2, f3 } =
            layout.orientation.inverse_matrix

        ( xl, yl ) =
            layout.size

        ( xo, yo ) =
            layout.origin

        { x, y } =
            Point2d.toPixels point

        x1 =
            (x - xo) / xl

        y1 =
            (y - yo) / yl

        q =
            (f0 * x1) + (f1 * y1)

        r =
            (f2 * x1) + (f3 * y1)
    in
    fromFloat ( q, r )


{-| Calculate corner offset from a center of the Hex
-}
hexCornerOffset : Layout -> Int -> ( Float, Float )
hexCornerOffset layout corner =
    let
        ( xl, yl ) =
            layout.size

        startAngle =
            layout.orientation.start_angle

        angle =
            ((2.0 * pi) * (toFloat corner + startAngle)) / 6

        x =
            precision 2 <| xl * cos angle

        y =
            precision 2 <| yl * sin angle
    in
    ( x, y )


{-| Once we know where the corners are relative to the center, we can calculate the corners in screen locations by adding the center to each corner, and putting the coordinates into a list.
-}
polygonCorners : Layout -> Hex -> List ( Float, Float )
polygonCorners layout hex =
    let
        ( x, y ) =
            toPointInternal layout hex

        offsetHex ( x__, y__ ) ( x_, y_ ) =
            ( precision 2 <| x__ + x_, precision 2 <| y__ + y_ )
    in
    List.range 0 5
        |> List.map (hexCornerOffset layout >> offsetHex ( x, y ))


toPointInternal : Layout -> Hex -> ( Float, Float )
toPointInternal layout hex =
    let
        { f0, f1, f2, f3 } =
            layout.orientation.forward_matrix

        ( xl, yl ) =
            layout.size

        ( xo, yo ) =
            layout.origin

        q =
            toQ hex

        r =
            toR hex

        x =
            precision 2 <| (((f0 * q) + (f1 * r)) * xl) + xo

        y =
            precision 2 <| (((f2 * q) + (f3 * r)) * yl) + yo
    in
    ( x, y )


{-| Linear interpolation of hexes
-}
hexLerp : Hex -> Hex -> Float -> Hex
hexLerp a b t =
    let
        a_ =
            toFloatCoordinates a

        b_ =
            toFloatCoordinates b

        q1 =
            toQ a_

        q2 =
            toQ b_

        r1 =
            toR a_

        r2 =
            toR b_

        q =
            q1 + ((q2 - q1) * t)

        r =
            r1 + ((r2 - r1) * t)
    in
    fromFloat ( q, r )


{-| Draw the line between hexes using the linear interpolation
-}
drawLine : Hex -> Hex -> List Hex
drawLine a b =
    let
        n =
            toFloat <| distance a b

        step =
            1.0 / max n 1.0

        aq =
            toQ a

        ar =
            toR a

        as_ =
            toS a

        bq =
            toQ b

        br =
            toR b

        bs =
            toS b

        a_nudge =
            FloatCubeHex ( aq + 1.0e-6, ar + 1.0e-6, as_ - 2.0e-6 )

        b_nudge =
            FloatCubeHex ( bq + 1.0e-6, br + 1.0e-6, bs - 2.0e-6 )

        float_steps =
            List.map toFloat (List.range 0 (truncate n))

        steps =
            List.map ((*) step) float_steps
    in
    List.map (toIntCoordinates << hexLerp a_nudge b_nudge) steps


{-| Draw the circular-ring of a defined redius with the hex in a center
-}
ring : Int -> Hex -> List Hex
ring radius hex =
    let
        calcHex q2 r2 =
            fromInt ( q2, r2 )
                |> plus hex

        calcRow q2 =
            let
                q1 =
                    max -radius (-q2 - radius)

                r1 =
                    min radius (-q2 + radius)
            in
            List.map (calcHex q2) (List.range q1 r1)
    in
    List.range -radius radius
        |> List.concatMap calcRow


{-| Draw the filled circle of a defined redius with the hex in a center
-}
circle : Int -> Hex -> List Hex
circle radius hex =
    List.range 1 radius
        |> List.concatMap (\r -> ring r hex)



--
--
-- MAP
--
--


{-| Dictionary storage to keep a map of hexes
-}
type alias Map a =
    Dict Key ( Hex, a )


{-| Key to access Map cell
-}
type alias Key =
    ( Int, Int, Int )


{-| Hash function to get a uniform token to address stored Hex
-}
toKey : Hex -> Key
toKey hex =
    let
        hex_ =
            toIntCoordinates hex

        q =
            toQInt hex_

        r =
            toRInt hex_

        s =
            toSInt hex_
    in
    ( q, r, s )


{-| Generate Map of rectangular shape given its height and width
-}
rectangularPointyTopMap : { width : Int, height : Int, initHex : Hex -> a } -> Map a
rectangularPointyTopMap { height, width, initHex } =
    let
        widthLine =
            List.range 0 width

        heightLine =
            List.range 0 height

        createHex : Int -> Int -> Hex
        createHex r q =
            fromInt ( q, r )

        offsetWidth : Int -> Int -> Int
        offsetWidth r q =
            q - (r // 2)

        widthRowLine : Int -> List Hex
        widthRowLine r =
            List.map (createHex r) <|
                List.map (offsetWidth r) widthLine

        allLines : List Hex
        allLines =
            List.concatMap widthRowLine heightLine

        makeDictRecord : Hex -> ( Key, ( Hex, a ) )
        makeDictRecord hex =
            ( toKey hex, ( hex, initHex hex ) )
    in
    allLines
        |> List.map makeDictRecord
        |> Dict.fromList


{-| Generate Map of rectangular shape given its height and width
-}
rectangularFlatTopMap : { width : Int, height : Int, initHex : Hex -> a } -> Map a
rectangularFlatTopMap { height, width, initHex } =
    let
        widthLine =
            List.range 0 width

        heightLine =
            List.range 0 height

        createHex : Int -> Int -> Hex
        createHex q r =
            fromInt ( q, r )

        offsetHeight : Int -> Int -> Int
        offsetHeight q r =
            r - (q // 2)

        widthColumnLine : Int -> List Hex
        widthColumnLine q =
            List.map (createHex q) <|
                List.map (offsetHeight q) heightLine

        allLines : List Hex
        allLines =
            List.concatMap widthColumnLine widthLine

        makeDictRecord : Hex -> ( Key, ( Hex, a ) )
        makeDictRecord hex =
            ( toKey hex, ( hex, initHex hex ) )
    in
    allLines
        |> List.map makeDictRecord
        |> Dict.fromList
