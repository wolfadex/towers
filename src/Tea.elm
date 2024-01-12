module Tea exposing
    ( Tea
    , save
    , withCmd, withReply
    , map, mapSub, mapView
    , toTuple
    )

{-|

@docs Tea

@docs save

@docs withCmd, withReply

@docs map, mapSub, mapView

@docs toTuple

-}

import Html exposing (Html)
import Task


type Tea model msg
    = Tea
        { model : model
        , commands : List (Cmd msg)
        , callbacks : List msg
        }


save : model -> Tea model msg
save model =
    Tea
        { model = model
        , commands = []
        , callbacks = []
        }


withCmd : Cmd msg -> Tea model msg -> Tea model msg
withCmd cmd (Tea tea) =
    Tea { tea | commands = cmd :: tea.commands }


withReply : msg -> Tea model msg -> Tea model msg
withReply msg (Tea tea) =
    Tea { tea | callbacks = msg :: tea.callbacks }


toTuple : Tea model msg -> ( model, Cmd msg )
toTuple (Tea tea) =
    ( tea.model
    , tea.callbacks
        |> List.map (Task.succeed >> Task.perform identity)
        |> (++) tea.commands
        |> Cmd.batch
    )


map : { a | toModel : modelA -> modelB, toMsg : msgA -> msgB } -> Tea modelA msgA -> Tea modelB msgB
map cfg (Tea tea) =
    Tea
        { model = cfg.toModel tea.model
        , commands = List.map (Cmd.map cfg.toMsg) tea.commands
        , callbacks = List.map cfg.toMsg tea.callbacks
        }


mapSub : { a | toMsg : msgA -> msgB } -> Sub msgA -> Sub msgB
mapSub cfg sub =
    Sub.map cfg.toMsg sub


mapView : { a | toMsg : msgA -> msgB } -> Html msgA -> Html msgB
mapView cfg view =
    Html.map cfg.toMsg view
