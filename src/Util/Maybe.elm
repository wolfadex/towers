module Util.Maybe exposing (apply)


apply : (b -> a -> a) -> Maybe b -> a -> a
apply fn maybeB a =
    case maybeB of
        Just value ->
            fn value a

        Nothing ->
            a
