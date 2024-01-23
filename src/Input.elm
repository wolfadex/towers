module Input exposing
    ( Key(..), Keyboard(..)
    , init
    , keyDown, keyUp
    , decodeKey
    , newKey
    , withAlt, withControl, withShift
    , isPressed
    )

{-|

@docs Key, Keyboard
@docs init
@docs keyDown, keyUp

@docs decodeKey

@docs newKey
@docs withAlt, withControl, withShift
@docs isPressed

-}

import Json.Decode
import Set exposing (Set)
import Time


type Key
    = Key
        { key : String
        , alt : Bool
        , control : Bool
        , shift : Bool
        }


type Keyboard
    = Keyboard
        { keysDown : Set String
        , alt : Bool
        , control : Bool
        , shift : Bool
        }


init : Keyboard
init =
    Keyboard
        { keysDown = Set.empty
        , alt = False
        , control = False
        , shift = False
        }


keyDown : Key -> Keyboard -> Keyboard
keyDown (Key key) (Keyboard keyboard) =
    Keyboard
        { keyboard
            | keysDown = Set.insert key.key keyboard.keysDown
            , alt = key.alt
            , control = key.control
            , shift = key.shift
        }


keyUp : Key -> Keyboard -> Keyboard
keyUp (Key key) (Keyboard keyboard) =
    Keyboard
        { keyboard
            | keysDown = Set.remove key.key keyboard.keysDown
            , alt = key.alt
            , control = key.control
            , shift = key.shift
        }


newKey : String -> Key
newKey str =
    Key
        { key = str
        , alt = False
        , control = False
        , shift = False
        }


withAlt : Key -> Key
withAlt (Key key) =
    Key
        { key
            | alt = True
        }


withControl : Key -> Key
withControl (Key key) =
    Key
        { key
            | control = True
        }


withShift : Key -> Key
withShift (Key key) =
    Key
        { key
            | shift = True
        }


isPressed : Keyboard -> Key -> Bool
isPressed (Keyboard keyboard) (Key key) =
    Set.member key.key keyboard.keysDown
        && (keyboard.alt == key.alt)
        && (keyboard.control == key.control)
        && (keyboard.shift == key.shift)


decodeKey : Json.Decode.Decoder ( Key, Time.Posix )
decodeKey =
    Json.Decode.map5
        (\key alt control shift timestamp ->
            ( Key
                { key = key
                , alt = alt
                , control = control
                , shift = shift
                }
            , Time.millisToPosix timestamp
            )
        )
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        (Json.Decode.field "__currentTime" Json.Decode.int)
