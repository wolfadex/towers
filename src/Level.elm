module Level exposing
    ( Level
    , init, default
    , encode, decode
    , addTile, removeTile
    , Prop(..)
    , addProp, removeProp
    , toMap
    )

{-|

@docs Level
@docs init, default
@docs encode, decode
@docs addTile, removeTile

@docs Prop
@docs addProp, removeProp

@docs toMap

-}

import Dict
import Hex exposing (Hex)
import Json.Decode
import Json.Encode


type alias Level =
    { tilemap : Hex.Map (Maybe Prop)
    }


encode : Level -> Json.Encode.Value
encode level =
    Json.Encode.object
        [ ( "tilemap"
          , level.tilemap
                |> Dict.toList
                |> Json.Encode.list
                    (\( key, prop ) ->
                        Json.Encode.object
                            [ ( "hex", key |> Hex.fromKey |> Hex.encode )
                            , ( "prop", encodeProp prop )
                            ]
                    )
          )
        ]


decode : Json.Decode.Decoder Level
decode =
    Json.Decode.map
        (\tilemap ->
            { tilemap = Dict.fromList tilemap
            }
        )
        (Json.Decode.field "tilemap"
            (Json.Decode.list
                (Json.Decode.map2
                    (\hex prop ->
                        ( Hex.toKey hex
                        , prop
                        )
                    )
                    (Json.Decode.field "hex" Hex.decode)
                    (Json.Decode.field "prop" decodeProp)
                )
            )
        )


type Prop
    = Target
    | Wall
    | Spawner


encodeProp : Maybe Prop -> Json.Encode.Value
encodeProp prop =
    case prop of
        Just Target ->
            Json.Encode.string "target"

        Just Wall ->
            Json.Encode.string "wall"

        Just Spawner ->
            Json.Encode.string "spawner"

        Nothing ->
            Json.Encode.null


decodeProp : Json.Decode.Decoder (Maybe Prop)
decodeProp =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\propName ->
                    case propName of
                        "target" ->
                            Json.Decode.succeed (Just Target)

                        "wall" ->
                            Json.Decode.succeed (Just Wall)

                        "spawner" ->
                            Json.Decode.succeed (Just Spawner)

                        _ ->
                            Json.Decode.fail ("Unknown prop: " ++ propName)
                )
        , Json.Decode.null Nothing
        ]


init : Level
init =
    { tilemap = Dict.empty
    }


default : Level
default =
    { tilemap =
        Hex.origin
            |> Hex.circle 6
            |> List.map (\hex -> ( Hex.toKey hex, Nothing ))
            |> Dict.fromList
    }
        |> addProp Hex.origin Target
        |> addProp
            (Hex.origin
                |> Hex.neighbor Hex.NorthEast
                |> Hex.neighbor Hex.NorthEast
                |> Hex.neighbor Hex.NorthEast
                |> Hex.neighbor Hex.NorthEast
                |> Hex.neighbor Hex.NorthEast
                |> Hex.neighbor Hex.NorthEast
            )
            Spawner
        |> addProp
            (Hex.origin
                |> Hex.neighbor Hex.NorthWest
                |> Hex.neighbor Hex.NorthWest
                |> Hex.neighbor Hex.NorthWest
                |> Hex.neighbor Hex.NorthWest
                |> Hex.neighbor Hex.NorthWest
                |> Hex.neighbor Hex.NorthWest
            )
            Spawner
        |> addProp
            (Hex.origin
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
            )
            Spawner
        |> addProp
            (Hex.origin
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
                |> Hex.neighbor Hex.SouthWest
            )
            Spawner


toMap : Level -> Hex.Map (Maybe Prop)
toMap level =
    level.tilemap


addTile : Hex -> Level -> Level
addTile hex level =
    { level
        | tilemap =
            Dict.update
                (Hex.toKey hex)
                (\maybeHex ->
                    case maybeHex of
                        Nothing ->
                            Just Nothing

                        Just _ ->
                            maybeHex
                )
                level.tilemap
    }


removeTile : Hex -> Level -> Level
removeTile hex level =
    { level
        | tilemap = Dict.remove (Hex.toKey hex) level.tilemap
    }


addProp : Hex -> Prop -> Level -> Level
addProp hex prop level =
    { level
        | tilemap =
            Dict.insert
                (Hex.toKey hex)
                (Just prop)
                level.tilemap
    }


removeProp : Hex -> Level -> Level
removeProp hex level =
    { level
        | tilemap =
            Dict.update
                (Hex.toKey hex)
                (Maybe.map (\_ -> Nothing))
                level.tilemap
    }
