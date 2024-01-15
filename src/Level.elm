module Level exposing (..)

import Dict exposing (Dict)
import Hex exposing (Hex)


type alias Level =
    { tilemap : Hex.Map (Maybe Prop)
    }


type Prop
    = Target
    | Wall
    | Spawner


init : Level
init =
    { tilemap = Dict.empty
    }


addTile : Hex -> Level -> Level
addTile hex level =
    { level
        | tilemap = Dict.insert (Hex.toKey hex) Nothing level.tilemap
    }


removeTile : Hex -> Level -> Level
removeTile hex level =
    { level
        | tilemap = Dict.remove (Hex.toKey hex) level.tilemap
    }
