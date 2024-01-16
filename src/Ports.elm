port module Ports exposing
    ( closeModal
    , copyToClipboard
    , openModal
    )


port openModal : String -> Cmd msg


port closeModal : String -> Cmd msg


port copyToClipboard : String -> Cmd msg
