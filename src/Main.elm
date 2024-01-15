module Main exposing (Flags, Model, Msg, main)

import Browser
import Game
import Menu
import Tea


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    Int



-----------
-- MODEL --
-----------


type Model
    = Menu Menu.Model
    | Game Game.Model



----------
-- INIT --
----------


init : Flags -> ( Model, Cmd Msg )
init _ =
    Menu.init { toModel = Menu, toMsg = MenuMsg }
        |> Tea.toTuple



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Menu menuModel ->
            Menu.subscriptions { toMsg = MenuMsg } menuModel

        Game gameModel ->
            Game.subscriptions { toMsg = GameMsg } gameModel



------------
-- UPDATE --
------------


type Msg
    = MenuMsg Menu.Msg
    | GameMsg Game.Msg
      --
    | GameStart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Tea.toTuple <|
        case msg of
            MenuMsg menuMsg ->
                case model of
                    Menu menuModel ->
                        Menu.update
                            { toModel = Menu
                            , toMsg = MenuMsg
                            , onGameStart = GameStart
                            }
                            menuMsg
                            menuModel

                    _ ->
                        Tea.save model

            GameMsg gameMsg ->
                case model of
                    Game gameModel ->
                        Game.update { toModel = Game, toMsg = GameMsg }
                            gameMsg
                            gameModel

                    _ ->
                        Tea.save model

            --
            GameStart ->
                Game.init { toModel = Game, toMsg = GameMsg }



----------
-- VIEW --
----------


view : Model -> Browser.Document Msg
view model =
    { title = "Hex Defense"
    , body =
        case model of
            Menu menuModel ->
                Menu.view { toMsg = MenuMsg } menuModel

            Game gameModel ->
                Game.view { toMsg = GameMsg } gameModel
    }
