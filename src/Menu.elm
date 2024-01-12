module Menu exposing (Model, Msg(..), init, subscriptions, update, view)

import Css
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Tea exposing (Tea)


type alias Model =
    {}


init : { toMsg : Msg -> msg, toModel : Model -> model } -> Tea model msg
init cfg =
    {}
        |> Tea.save
        |> Tea.map cfg


subscriptions : { toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions cfg _ =
    Sub.none


type Msg
    = StartGame


update :
    { onGameStart : msg
    , toMsg : Msg -> msg
    , toModel : Model -> model
    }
    -> Msg
    -> Model
    -> Tea model msg
update cfg msg model =
    case msg of
        StartGame ->
            model
                |> Tea.save
                |> Tea.map cfg
                |> Tea.withReply cfg.onGameStart


view : { toMsg : Msg -> msg } -> Model -> List (Html msg)
view cfg model =
    List.map (Tea.mapView cfg)
        [ Html.div
            [ Css.title ]
            [ Html.text "Tower Game" ]
        , Html.div [ Css.mainMenu ]
            [ Html.div [ Css.menu ]
                [ Html.button
                    [ Html.Attributes.disabled True ]
                    [ Html.text "Load Game" ]
                , Html.button
                    [ Html.Events.onClick StartGame ]
                    [ Html.text "Start Game" ]
                , Html.button
                    [ Html.Attributes.disabled True ]
                    [ Html.text "About" ]
                ]
            ]
        ]
