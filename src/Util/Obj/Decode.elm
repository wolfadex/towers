module Util.Obj.Decode exposing (andMap)

import Obj.Decode


andMap : Obj.Decode.Decoder a -> Obj.Decode.Decoder (a -> b) -> Obj.Decode.Decoder b
andMap =
    Obj.Decode.map2 (|>)
