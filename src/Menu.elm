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
import Ports
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
    | EditLevel
    | ShowAbout
    | HideAbout


update :
    { toMsg : Msg -> msg
    , toModel : Model -> model
    , onGameStart : msg
    , onLevelEdit : msg
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

        EditLevel ->
            model
                |> Tea.save
                |> Tea.map cfg
                |> Tea.withReply cfg.onLevelEdit

        ShowAbout ->
            model
                |> Tea.save
                |> Tea.withCmd (Ports.openModal "about")
                |> Tea.map cfg

        HideAbout ->
            model
                |> Tea.save
                |> Tea.withCmd (Ports.closeModal "about")
                |> Tea.map cfg



----------
-- VIEW --
----------


view : { toMsg : Msg -> msg } -> Model -> List (Html msg)
view cfg _ =
    List.map (Tea.mapView cfg)
        [ viewAboutDialog
        , Html.div
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
                    [ Html.Events.onClick EditLevel ]
                    [ Html.text "Level Editor" ]
                , Html.button
                    [ Html.Events.onClick ShowAbout ]
                    [ Html.text "About" ]
                ]
            ]
        ]


viewAboutDialog : Html Msg
viewAboutDialog =
    Html.node "dialog"
        [ Html.Attributes.id "about"
        ]
        [ Html.p []
            [ Html.text "Towers is a built with "
            , Html.a [ Html.Attributes.href "https://elm-lang.org/" ] [ Html.text "Elm" ]
            , Html.text " by Wolfgang Schuster."
            , Html.br [] []
            , Html.text "The source code is available at "
            , Html.a
                [ Html.Attributes.href "https://github.com/wolfadex/towers" ]
                [ Html.text "on GitHub" ]
            , Html.text "."
            , Html.br [] []
            , Html.br [] []
            , Html.text "Thank you to the following for their packages & tools I used:"
            , [ { url = "https://github.com/avh4"
                , name = "Aaron VonderHaar"
                }
              , { url = "https://github.com/w0rm"
                , name = "Andrey Kuzmin"
                }
              , { url = "https://github.com/0ui"
                , name = "Drew Greene"
                }
              , { url = "https://github.com/evancz/"
                , name = "Evan Czaplicki"
                }
              , { url = "https://github.com/ianmackenzie/"
                , name = "Ian Mackenzie"
                }
              , { url = "https://github.com/jfmengels/"
                , name = "Jeroen Engels"
                }
              , { url = "https://github.com/krisajenkins"
                , name = "Kris Jenkins"
                }
              , { url = "https://github.com/ryannhg"
                , name = "Ryan Haskell-Glatz"
                }
              , { url = "https://github.com/lydell/"
                , name = "Simon Lydell"
                }
              , { url = "https://www.blender.org/"
                , name = "Blender"
                }
              ]
                |> List.map
                    (\person ->
                        Html.li []
                            [ Html.a
                                [ Html.Attributes.href person.url ]
                                [ Html.text person.name ]
                            ]
                    )
                |> Html.ul []
            ]
        , Html.button
            [ Html.Events.onClick HideAbout ]
            [ Html.text "Close" ]
        ]
