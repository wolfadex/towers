module Menu exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Css
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Tea exposing (Tea)



-----------
-- MODEL --
-----------


type alias Model =
    {}



----------
-- INIT --
----------


init : { toMsg : Msg -> msg, toModel : Model -> model } -> Tea model msg
init cfg =
    {}
        |> Tea.save
        |> Tea.map cfg



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : { toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions _ _ =
    Sub.none



------------
-- UPDATE --
------------


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



----------
-- VIEW --
----------


view : { toMsg : Msg -> msg } -> Model -> List (Html msg)
view cfg _ =
    List.map (Tea.mapView cfg)
        [ Html.div
            [ Css.title ]
            [ Html.text "Tower Game" ]
        , Html.div [ Css.mainMenu ]
            [ Html.div [ Css.menu ]
                [ Html.button
                    [ Html.Events.onClick StartGame ]
                    [ Html.text "New Game" ]
                , Html.button
                    [ Html.Attributes.disabled True ]
                    [ Html.text "Load Custom Game" ]
                , Html.button
                    [ Html.Attributes.disabled True ]
                    [ Html.text "Level Editor" ]
                , Html.button
                    [ Html.Attributes.disabled True ]
                    [ Html.text "About" ]
                ]
            ]
        ]
