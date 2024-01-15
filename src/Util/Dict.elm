module Util.Dict exposing (at)

import Dict exposing (Dict)


at : Dict comparable value -> comparable -> Maybe value
at dict key =
    Dict.get key dict
