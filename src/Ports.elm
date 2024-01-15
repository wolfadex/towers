port module Ports exposing
    ( closeModal
    , openModal
    )


port openModal : String -> Cmd msg


port closeModal : String -> Cmd msg
