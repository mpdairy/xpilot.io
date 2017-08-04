--module Xpilot


module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr


-- import Html.App as App
--import Html.Events exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Random
import Time exposing (Time)
import AnimationFrame


--import Collage as G
--import Element
--import Color

import Dict exposing (Dict)
import String


--import Set exposing (Set)

import Keyboard exposing (KeyCode)


--import Array exposing (Array)

import GameEngine exposing (..)
import Vector exposing (..)


--import XpilotDisplay exposing (..)

import XpilotObjects exposing (..)
import XpilotTypes exposing (..)
import Shape exposing (Shape(..))
import Brains exposing (..)
import Window
import Task
import Platform.Cmd
import Http
import PageVisibility as PV


--import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias GameState =
    { players : Dict ObjectID Player
    , hud : Hud
    }



--


type alias Model =
    { selfID : Int
    , paused : Bool
    , statsID : Maybe String
    , offset : Vector
    , splash : Bool
    , donate : Bool
    , donateMsg : String
    , screenSize : Vector
    , screenZoom : Float
    , playSize : Vector
    , gameState : GameState
    , gameWorld : World XpilotMeta XpilotObject XpilotMsg
    }



--


type alias Hud =
    { energy : Float
    , timeToRespawn : Time
    , kills : Int
    , messages : List DisplayMsg
    }



--


type alias Stats =
    { kills : Int
    , deaths : Int
    , score : Float
    , lifeKills : Int
    , lifeScore : Float
    }



--


emptyStats : Stats
emptyStats =
    { kills = 0
    , deaths = 0
    , score = 0
    , lifeKills = 0
    , lifeScore = 0
    }



--


type Player
    = Player
        { id : ObjectID
        , name : String
        , dead : Bool
        , timeDead : Time
        , stats : Stats
        , create : List (GameMessage XpilotMeta XpilotObject XpilotMsg)
        }



--


type DisplayMsg
    = DisplayMsg
        { timeDisplayed : Time
        , message : String
        , color : String
        }


init : ( Model, Cmd Msg )
init =
    let
        meta =
            { keyboardControls =
                (Dict.fromList
                    [ ( 16, Thrust )
                    , ( 65, TurnRight )
                    , ( 83, TurnLeft )
                    , ( 13, Shoot )
                    , ( 32, Shoot )
                    , ( 37, TurnRight )
                    , ( 39, TurnLeft )
                    , ( 38, Thrust )
                    , ( 17, Shoot )
                    , ( 88, Shoot )
                    ]
                )
            , controlState = []
            , playerDeadTime = 3 * Time.second
            , shotCharge = 1000
            , shotChargeRate = 400 / Time.second
            , shotChargeCost = 200
            , shotRepeatRate = 10 / Time.second
            , shotLife = 5 * Time.second
            , shotVel = 220 / Time.second
            , shotRecoil = 1 / Time.second
            , shipMaxThrust = 0.4 / Time.second
            , shipMaxTurn = 6 / Time.second
            , shipEmberSpeed = 180 / Time.second
            , shipEmberRate = 30 / Time.second
            , shipExplosionSize = 50
            , emberReduceRate = 2.5 / Time.second
            }

        (World world) =
            initWorld meta xpilotMsgHandler

        --(GameObject player) = (newShip meta 7 0 (Vector 400 400) keyboardBrain)
        screenSize =
            (Vector 1200 700)

        screenZoom =
            1.3

        playSize =
            vectorDivScalar screenZoom screenSize

        gameState =
            { players =
                Dict.fromList
                    [ ( 7
                      , Player
                            { id = 7
                            , name = "Player"
                            , dead = True
                            , timeDead = meta.playerDeadTime
                            , stats = emptyStats
                            , create =
                                [ GameEngine.Game
                                    (Respawn
                                        (newShip meta
                                            7
                                            "Player"
                                            0
                                            (Vector 0 0)
                                            keyboardBrain
                                        )
                                        (Vector 150 150)
                                        (Vector 750 750)
                                    )
                                ]
                            }
                      )
                    , ( 8
                      , Player
                            { id = 8
                            , name = "Sid"
                            , dead = True
                            , timeDead = meta.playerDeadTime
                            , stats = emptyStats
                            , create =
                                [ GameEngine.Game
                                    (Respawn
                                        (newShip meta
                                            8
                                            "Sid"
                                            0
                                            (Vector 0 0)
                                            sid
                                        )
                                        (Vector 150 150)
                                        (Vector 750 750)
                                    )
                                ]
                            }
                      )
                    , ( 9
                      , Player
                            { id = 9
                            , name = "Mako"
                            , dead = True
                            , timeDead = meta.playerDeadTime
                            , stats = emptyStats
                            , create =
                                [ GameEngine.Game
                                    (Respawn
                                        (newShip meta
                                            9
                                            "Mako"
                                            0
                                            (Vector 0 0)
                                            sel
                                        )
                                        (Vector 150 150)
                                        (Vector 750 750)
                                    )
                                ]
                            }
                      )

                    {- , ( 12
                       , Player { id = 12
                                , name = "Spinoza"
                                , dead = True
                                , timeDead = meta.playerDeadTime
                                , score = 0
                                , create = [NewObjectKeepID (newShip meta 12 "Spinoza" 0 (Vector 100 800) defaultBrain)]})
                    -}
                    ]
            , hud =
                { energy = 1
                , timeToRespawn = 0
                , kills = 0
                , messages = []
                }
            }
    in
        ( { selfID = 7
          , statsID = Nothing
          , paused = False
          , offset = calcOffset playSize (Vector 400 400)
          , screenSize = screenSize
          , screenZoom = screenZoom
          , splash = True
          , donate = False
          , donateMsg = ""
          , playSize = playSize
          , gameState = gameState
          , gameWorld =
                (World
                    { world
                        | objects =
                            [ --(GameObject player)
                              --, (newShip meta 10 1.0 (Vector 10 200) sid)
                              --                           , (newShip meta 11 0.9 (Vector 400 600) defaultBrain)
                              --, (newShip meta 12 0.9 (Vector 100 800) defaultBrain)
                              (newWall (Vector 300 100) (Vector 1030 30))
                            , (newWall (Vector 300 900) (Vector 1030 30))
                            , (newWall (Vector (-200) 500) (Vector 30 830))
                            , (newWall (Vector 800 500) (Vector 30 830))
                            , (newWall (Vector 500 500) (Vector 30 120))
                            , (newWall (Vector 200 250) (Vector 90 60))
                            , (newWall (Vector 130 650) (Vector 45 45))
                            , (newWall (Vector 200 780) (Vector 45 45))
                            , (newWall (Vector 180 700) (Vector 40 40))
                            ]
                    }
                )
          }
        , initialSizeCmd
        )


calcOffset : Vector -> Vector -> Vector
calcOffset playSize playerPos =
    (vectorAdd (vectorMultScalar 0.5 playSize)
        (vectorMultScalar -1 playerPos)
    )



--


updateHud :
    XpilotMeta
    -> GameState
    -> Maybe Player
    -> Maybe (GameObject XpilotMeta XpilotObject XpilotMsg)
    -> Hud
    -> Hud
updateHud meta gs maybePlayer maybePlayerObj hud =
    case maybePlayer of
        Nothing ->
            hud

        Just (Player p) ->
            let
                energy =
                    case maybePlayerObj of
                        Nothing ->
                            hud.energy

                        Just (GameObject obj) ->
                            case obj.self of
                                Ship s ->
                                    s.shotCharge / meta.shotCharge

                                _ ->
                                    hud.energy

                timeToRespawn =
                    if p.dead then
                        (meta.playerDeadTime - p.timeDead) / Time.second
                    else
                        0
            in
                { hud
                    | energy = energy
                    , kills = p.stats.lifeKills
                    , timeToRespawn = timeToRespawn
                }


getPlayer :
    ObjectID
    -> List (GameObject XpilotMeta XpilotObject XpilotMsg)
    -> Maybe (GameObject XpilotMeta XpilotObject XpilotMsg)
getPlayer id objs =
    case List.filter (\(GameObject obj) -> obj.id == id) objs of
        [] ->
            Nothing

        p :: _ ->
            Just p



--


hudMessage : Hud -> String -> String -> Hud
hudMessage hud color s =
    { hud
        | messages =
            hud.messages
                ++ [ (DisplayMsg
                        { timeDisplayed = 0
                        , message = s
                        , color = color
                        }
                     )
                   ]
    }



--


gameStateMessageHandler :
    ObjectID
    -> GameState
    -> XpilotMeta
    -> GameMessage XpilotMeta XpilotObject XpilotMsg
    -> ( GameState, List (GameMessage XpilotMeta XpilotObject XpilotMsg) )
gameStateMessageHandler selfID gs meta msg =
    case msg of
        GameEngine.Game xpmsg ->
            case xpmsg of
                Killed killedID killerID killerObj ->
                    case killerObj of
                        Ship ship ->
                            ( { gs
                                | players =
                                    Dict.update killedID
                                        (\p ->
                                            case p of
                                                Nothing ->
                                                    Nothing

                                                Just (Player pl) ->
                                                    Just
                                                        (Player
                                                            (let
                                                                stats =
                                                                    pl.stats

                                                                ustats =
                                                                    { stats
                                                                        | deaths = stats.deaths + 1
                                                                        , score = stats.score - 2.0 + stats.lifeScore
                                                                        , kills = stats.kills + 1
                                                                        , lifeScore = 0
                                                                    }
                                                             in
                                                                { pl
                                                                    | stats = ustats
                                                                    , dead = True
                                                                    , timeDead = 0
                                                                }
                                                            )
                                                        )
                                        )
                                        gs.players
                                , hud =
                                    if killedID == selfID then
                                        case ( Dict.get killedID gs.players, Dict.get killerID gs.players ) of
                                            ( Just (Player killed), Just (Player killer) ) ->
                                                hudMessage gs.hud
                                                    "#aa0000"
                                                    (""
                                                        --++ (toString (killed.stats.lifeKills))
                                                        --++ "Collision with "
                                                        ++ killer.name
                                                     --++ "("
                                                     --++ (toString (killer.stats.lifeKills))
                                                     --++ ")"
                                                    )

                                            _ ->
                                                gs.hud
                                    else
                                        gs.hud
                              }
                            , []
                            )

                        Bullet bullet ->
                            let
                                hud =
                                    if (killerID == selfID || killedID == selfID) then
                                        case ( Dict.get killedID gs.players, Dict.get killerID gs.players ) of
                                            ( Just (Player killed), Just (Player killer) ) ->
                                                if killedID == killerID then
                                                    hudMessage gs.hud
                                                        "#aa0000"
                                                    <|
                                                        "Yourself"
                                                    --killed.name
                                                    --++ "("
                                                    --++ (toString killed.stats.lifeKills)
                                                    --++ ")"
                                                else if killedID == selfID then
                                                    hudMessage gs.hud
                                                        "#aaaa00"
                                                        (killer.name
                                                         --++ "("
                                                         --++ (toString (killer.stats.lifeKills + 1))
                                                         --++ ") "
                                                         --++ "("
                                                         --++ (toString (killed.stats.lifeKills))
                                                         --++ ")"
                                                        )
                                                else
                                                    hudMessage gs.hud
                                                        "green"
                                                        (killed.name
                                                         --++ "("
                                                         --++ (toString (killed.stats.lifeKills + 1))
                                                         --++ ")"
                                                        )

                                            _ ->
                                                gs.hud
                                    else
                                        gs.hud
                            in
                                ( { gs
                                    | players =
                                        (Dict.update killerID
                                            (\p ->
                                                if killerID /= killedID then
                                                    case p of
                                                        Nothing ->
                                                            Nothing

                                                        Just (Player pl) ->
                                                            Just
                                                                (Player
                                                                    (let
                                                                        stats =
                                                                            pl.stats

                                                                        ustats =
                                                                            { stats
                                                                                | lifeKills = stats.lifeKills + 1
                                                                                , lifeScore = stats.lifeScore + (toFloat stats.lifeKills) + 1
                                                                                , score = stats.score + (toFloat stats.lifeKills)
                                                                                , kills = stats.kills + 1
                                                                            }
                                                                     in
                                                                        { pl | stats = ustats }
                                                                    )
                                                                )
                                                else
                                                    p
                                            )
                                            (Dict.update killedID
                                                (\p ->
                                                    case p of
                                                        Nothing ->
                                                            Nothing

                                                        Just (Player pl) ->
                                                            Just
                                                                (Player
                                                                    (let
                                                                        stats =
                                                                            pl.stats

                                                                        ustats =
                                                                            { stats
                                                                                | deaths = stats.deaths + 1
                                                                                , score = stats.score - 2.0 + stats.lifeScore

                                                                                -- , lifeKills = 0
                                                                                , kills = stats.kills + stats.lifeKills
                                                                                , lifeScore = 0
                                                                            }
                                                                     in
                                                                        { pl
                                                                            | stats = ustats
                                                                            , dead = True
                                                                            , timeDead = 0
                                                                        }
                                                                    )
                                                                )
                                                )
                                                gs.players
                                            )
                                        )
                                    , hud = hud
                                  }
                                , []
                                )

                        Wall wall ->
                            ( { gs
                                | players =
                                    Dict.update killedID
                                        (\p ->
                                            case p of
                                                Nothing ->
                                                    Nothing

                                                Just (Player pl) ->
                                                    let
                                                        stats =
                                                            pl.stats
                                                    in
                                                        Just
                                                            (Player
                                                                { pl
                                                                    | dead = True
                                                                    , timeDead = 0
                                                                    , stats =
                                                                        { stats
                                                                            | deaths = stats.deaths + 1
                                                                            , score = stats.score - 2.0 + stats.lifeScore

                                                                            --                                                                       , lifeKills = 0
                                                                            , lifeScore = 0
                                                                        }
                                                                }
                                                            )
                                        )
                                        gs.players
                              }
                            , []
                            )

                        _ ->
                            ( gs, [] )

                _ ->
                    ( gs, [] )

        _ ->
            ( gs, [] )



--


updateGameState :
    Time
    -> ObjectID
    -> GameState
    -> XpilotMeta
    -> List (GameMessage XpilotMeta XpilotObject XpilotMsg)
    -> ( GameState, List (GameMessage XpilotMeta XpilotObject XpilotMsg) )
updateGameState diff selfID gs meta msgs =
    let
        ( ugs, umsgs ) =
            List.foldr
                (\msg ( fgs, fmsgs ) ->
                    let
                        ( rgs, rmsgs ) =
                            gameStateMessageHandler selfID fgs meta msg
                    in
                        ( rgs, rmsgs ++ fmsgs )
                )
                ( gs, [] )
                msgs

        ( tplayers, tmsgs ) =
            tickPlayers diff meta ugs.players

        deadplayers =
            Dict.map (\_ (Player p) -> p.dead) ugs.players

        hud =
            ugs.hud

        uhud =
            { hud | messages = tickHudMessages diff hud.messages }
    in
        ( { ugs
            | players = tplayers
            , hud = uhud
          }
        , tmsgs ++ umsgs
        )



--


tickHudMessages : Time -> List DisplayMsg -> List DisplayMsg
tickHudMessages diff msgs =
    List.foldr
        (\(DisplayMsg m) mx ->
            if m.timeDisplayed > 5 * Time.second then
                mx
            else
                (DisplayMsg { m | timeDisplayed = m.timeDisplayed + diff })
                    :: mx
        )
        []
        msgs



--


tickPlayers :
    Time
    -> XpilotMeta
    -> Dict ObjectID Player
    -> ( Dict ObjectID Player, List (GameMessage XpilotMeta XpilotObject XpilotMsg) )
tickPlayers diff meta players =
    Dict.foldr
        (\id (Player p) ( ps, msgs ) ->
            if p.dead then
                let
                    newTimeDead =
                        p.timeDead + diff

                    respawn =
                        newTimeDead >= meta.playerDeadTime

                    stats =
                        p.stats
                in
                    ( Dict.insert id
                        (Player
                            { p
                                | timeDead = newTimeDead
                                , dead = not respawn
                                , stats =
                                    if respawn then
                                        { stats | lifeKills = 0 }
                                    else
                                        stats
                            }
                        )
                        ps
                    , if respawn then
                        p.create ++ msgs
                      else
                        msgs
                    )
            else
                ( ps, msgs )
        )
        ( players, [] )
        players


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (Window True) Window.size



-- UPDATE


type Msg
    = Play
    | Error String
    | Window Bool Window.Size
    | Tick Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Game (List (GameMessage XpilotMeta XpilotObject XpilotMsg))
    | StatsNewGame String
    | StatsDeath String
    | Visible PV.Visibility
    | Donate ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( model, Cmd.none )

        Error _ ->
            ( model, Cmd.none )

        Window init { width, height } ->
            let
                screenSize =
                    Vector (toFloat width) (toFloat height)

                zoom =
                    if init then
                        screenSize.y / 630
                    else
                        model.screenZoom
            in
                ( { model
                    | screenSize = screenSize
                    , screenZoom = zoom
                    , playSize = vectorDivScalar zoom screenSize
                  }
                , Cmd.none
                )

        KeyDown k ->
            ( { model
                | gameWorld = handleKeyDown k model.gameWorld
                , splash = False
                , donate = False
              }
            , if model.splash then
                fetchNewGame
              else
                Cmd.none
            )

        KeyUp k ->
            ( { model | gameWorld = handleKeyUp k model.gameWorld }
            , Cmd.none
            )

        Tick tdiff ->
            if model.paused then
                model ! []
            else
                let
                    diff =
                        Basics.min tdiff (50 * Time.millisecond)

                    ( World world, gameMessages ) =
                        updateWorld diff model.gameWorld

                    ( gs, gsMsgs ) =
                        updateGameState diff model.selfID model.gameState world.meta gameMessages

                    (World uworld) =
                        handleGameMessages (gsMsgs ++ gameMessages) (World world)

                    maybePlayerObj =
                        getPlayer model.selfID world.objects

                    maybePlayer =
                        Dict.get model.selfID gs.players

                    offset =
                        case maybePlayerObj of
                            Nothing ->
                                model.offset

                            Just (GameObject p) ->
                                calcOffset model.playSize p.pos

                    hud =
                        updateHud uworld.meta gs maybePlayer maybePlayerObj gs.hud

                    hgs =
                        { gs | hud = hud }

                    deadFetchCmd =
                        case ( model.statsID, maybePlayer ) of
                            ( Nothing, _ ) ->
                                Cmd.none

                            ( _, Nothing ) ->
                                Cmd.none

                            ( Just statsID, Just (Player p) ) ->
                                if p.dead && p.timeDead <= diff then
                                    Cmd.batch [ fetchDeath statsID p.stats.lifeKills ]
                                else
                                    Cmd.none

                    maybeDonate =
                        Maybe.withDefault Cmd.none <|
                            Maybe.map
                                (\(Player p) ->
                                    if p.dead && p.timeDead <= diff then
                                        Random.generate
                                            Donate
                                            (Random.pair
                                                (Random.int 0 2)
                                                (Random.int 0 (List.length wants - 1))
                                            )
                                    else
                                        Cmd.none
                                )
                                maybePlayer
                in
                    ( { model
                        | gameWorld = (World uworld)
                        , gameState = hgs
                        , offset = offset
                      }
                    , Cmd.batch
                        [ deadFetchCmd

                        --, maybeDonate
                        ]
                    )

        Game gameMessages ->
            let
                world =
                    handleGameMessages gameMessages model.gameWorld
            in
                ( { model | gameWorld = world }
                , Cmd.none
                )

        StatsNewGame sid ->
            { model | statsID = Just sid } ! []

        StatsDeath _ ->
            model ! []

        Visible v ->
            case v of
                PV.Visible ->
                    { model | paused = False } ! []

                PV.Hidden ->
                    { model | paused = True } ! []

        Donate ( a, b ) ->
            if a == 0 then
                { model
                    | donate = True
                    , donateMsg =
                        Maybe.withDefault "" <|
                            List.head (List.drop b wants)
                }
                    ! []
            else
                model ! []



--      _ -> ( model, Cmd.none )
--


fetchNewGame : Cmd Msg
fetchNewGame =
    Http.getString "/newgame"
        |> Http.send processNewGameResponse


processNewGameResponse : Result Http.Error String -> Msg
processNewGameResponse result =
    case result of
        Ok sid ->
            StatsNewGame sid

        Err err ->
            Error (toString err)



--


fetchDeath : String -> Int -> Cmd Msg
fetchDeath statsID kills =
    Http.getString
        ("/death?vid=" ++ statsID ++ "&kills=" ++ toString kills)
        |> Http.send processDeathResponse


processDeathResponse : Result Http.Error String -> Msg
processDeathResponse result =
    case result of
        Ok sid ->
            StatsDeath sid

        Err err ->
            Error (toString err)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes (Window False)
        , PV.visibilityChanges Visible
        ]



-- VIEW


view : Model -> Html Msg
view model =
    -- drawScene model
    div
        [ Attr.style
            [ ( "height", toString (model.screenSize.y) )
            , ( "width", toString (model.screenSize.x) )
            , ( "overflow", "hidden" )
            ]
        ]
    <|
        List.concat
            [ [ drawScene model ]
            , if model.donate then
                [ drawDonateSplash model.screenSize model.donateMsg ]
              else
                []
            ]



--


drawScene : Model -> Html Msg
drawScene model =
    let
        (World world) =
            model.gameWorld
    in
        (Svg.svg
            [ width (toString (model.screenSize.x))
            , height (toString (model.screenSize.y))
            , viewBox
                ("0 0 "
                    ++ (toString model.playSize.x)
                    ++ " "
                    ++ (toString model.playSize.y)
                )
            ]
            (List.concat
                [ drawBackground model
                , List.concat
                    (List.map (drawObject model.selfID model.offset) world.objects)
                , drawHud model.playSize model.gameState.hud
                , drawRadar model
                , if model.gameState.hud.timeToRespawn > 0 then
                    --[drawScores model]
                    []
                  else
                    []
                , if model.splash then
                    [ welcomeSplash model.playSize ]
                  else
                    []
                ]
            )
        )


drawBackground : Model -> List (Html Msg)
drawBackground model =
    [ Svg.rect
        [ x "0"
        , y "0"
        , width (toString model.playSize.x)
        , height (toString model.playSize.y)
        , fill "black"
        ]
        []
    ]


drawObject : ObjectID -> Vector -> GameObject XpilotMeta XpilotObject XpilotMsg -> List (Html Msg)
drawObject selfID offset (GameObject object) =
    if object.destroy == False then
        case object.self of
            Ship _ ->
                drawShip selfID offset (GameObject object)

            Bullet _ ->
                drawBullet offset (GameObject object)

            Wall _ ->
                drawWall offset (GameObject object)

            Ember _ ->
                drawEmber offset (GameObject object)
    else
        []


drawText : Vector -> String -> Html Msg
drawText pos str =
    Svg.text_
        [ x (toString pos.x)
        , y (toString pos.y)
        , fill "white"
        ]
        [ Svg.text str ]


drawBullet : Vector -> GameObject XpilotMeta XpilotObject XpilotMsg -> List (Html Msg)
drawBullet offset (GameObject bullet) =
    [ Svg.rect
        [ x (toString (bullet.pos.x + offset.x))
        , y (toString (bullet.pos.y + offset.y))
        , width "2"
        , height "2"
        , fill "white"
        ]
        []
    ]


drawShip : ObjectID -> Vector -> GameObject XpilotMeta XpilotObject XpilotMsg -> List (Html Msg)
drawShip selfID offset (GameObject ship) =
    case ship.self of
        Ship s ->
            List.concat
                [ [ drawShape (vectorAdd offset (Vector ship.pos.x ship.pos.y))
                        [ stroke "white", fill "black" ]
                        ship.shape
                  ]
                , if selfID /= ship.id then
                    [ Svg.text_
                        [ fill "yellow"
                        , textAnchor "middle"
                        , fontSize "1.0em"
                        , fontStyle "italic"

                        --                        , fontWeight "Bold"
                        , fontFamily "monospace"
                        , x (toString (offset.x + ship.pos.x))
                        , y (toString (offset.y + ship.pos.y + 35))
                        ]
                        [ Svg.text s.name ]
                    ]
                  else
                    []
                ]

        _ ->
            []


drawWall : Vector -> GameObject XpilotMeta XpilotObject XpilotMsg -> List (Html Msg)
drawWall offset (GameObject wall) =
    [ drawShape (vectorAdd offset wall.pos)
        [ fill "#2222dd" ]
        wall.shape
    ]


drawEmber : Vector -> GameObject XpilotMeta XpilotObject XpilotMsg -> List (Html Msg)
drawEmber offset (GameObject ember) =
    [ Svg.rect
        [ x (toString (ember.pos.x + offset.x - 0.8))
        , y (toString (ember.pos.y + offset.y - 0.8))
        , width "1.6"
        , height "1.6"
        , fill "red"
        ]
        []
    ]


drawShape : Vector -> List (Svg.Attribute Msg) -> Shape -> Html Msg
drawShape offset styles shape =
    case shape of
        Polygon pointlist ->
            drawPolygon offset styles pointlist

        _ ->
            drawPoint offset (Vector 0 0)


drawPoint : Vector -> Vector -> Html Msg
drawPoint offset point =
    Svg.rect
        [ x (toString (point.x + offset.x))
        , y (toString (point.y + offset.y))
        , width "2"
        , height "2"
        , fill "white"
        ]
        []


drawPolygon : Vector -> List (Svg.Attribute Msg) -> List Vector -> Html Msg
drawPolygon offset styles pointlist =
    let
        pointstring =
            (List.foldl
                (\point s ->
                    s
                        ++ (toString (offset.x + point.x)
                                ++ ", "
                                ++ toString (offset.y + point.y)
                                ++ " "
                           )
                )
                ""
                pointlist
            )
    in
        Svg.polygon ((points pointstring) :: styles)
            []



--


hudWidth : Float
hudWidth =
    300



--


hudHeight : Float
hudHeight =
    150



--


hudBarHeight : Float
hudBarHeight =
    4



--


drawHud : Vector -> Hud -> List (Html Msg)
drawHud screensize hud =
    let
        middle =
            vectorMultScalar 0.5 screensize
    in
        List.concat
            [ [ Svg.rect
                    [ x (toString (middle.x - hudWidth / 2))
                    , y (toString (middle.y - hudHeight / 2))
                    , width (toString hudWidth)
                    , height (toString hudHeight)
                    , stroke "green"
                    , fill "none"
                    , strokeDasharray "0, 10"
                    ]
                    []
              ]
            , [ Svg.rect
                    [ x (toString (middle.x + hudWidth / 2))
                    , y (toString (middle.y - hudHeight / 2))
                    , width (toString hudBarHeight)
                    , height (toString hudHeight)
                    , fill "none"
                    , stroke "green"
                    ]
                    []

              -- hud energy bar right
              , Svg.rect
                    [ x (toString (middle.x + hudWidth / 2))
                    , y (toString ((middle.y - hudHeight / 2) + (hudHeight * (1 - hud.energy))))
                    , width (toString hudBarHeight)
                    , height (toString (hudHeight * hud.energy))
                    , fill "green"
                    ]
                    []
              ]
            , if hud.timeToRespawn > 0 then
                [ Svg.text_
                    [ fill "green"
                    , textAnchor "middle"
                    , fontSize "1.2em"
                    , fontStyle "italic"
                    , fontWeight "Bold"
                    , fontFamily "monospace"
                    , x (toString middle.x)
                    , y (toString (middle.y + 8))
                    ]
                    [ Svg.text (String.left 3 (toString hud.timeToRespawn)) ]
                ]
              else
                []
            , List.indexedMap (printHudMsg middle) hud.messages
            , [ Svg.text_
                    [ fill "green"
                    , textAnchor "right"
                    , fontSize "1.0em"
                    , fontStyle "italic"
                    , fontWeight "Bold"
                    , fontFamily "monospace"
                    , x (toString (middle.x + hudWidth / 2 - 20))
                    , y (toString (middle.y + hudHeight / 2))
                    ]
                    [ Svg.text (String.left 3 (toString hud.kills)) ]
              ]
            ]



--
--


printHudMsg : Vector -> Int -> DisplayMsg -> Html Msg
printHudMsg middle i (DisplayMsg { message, color }) =
    Svg.text_
        [ fill color
        , textAnchor "left"
        , fontSize "0.9em"
        , fontStyle "italic"
        , fontWeight "Bold"
        , fontFamily "monospace"
        , x (toString (middle.x - (hudWidth / 2)))
        , y (toString (middle.y + (hudHeight / 2) - 15 * (toFloat i)))
        ]
        [ Svg.text message ]


radarWidth : Float
radarWidth =
    160


radarHeight : Float
radarHeight =
    160


drawRadar : Model -> List (Html Msg)
drawRadar model =
    let
        (World world) =
            model.gameWorld

        rx =
            model.playSize.x - radarWidth - 1

        ry =
            model.playSize.y - radarHeight - 1
    in
        [ Svg.svg
            [ x (toString rx)
            , y (toString ry)
            , width (toString radarWidth)
            , height (toString radarHeight)
            , viewBox
                ((toString (1 * model.playSize.x / 2 - 700))
                    ++ " "
                    ++ (toString (1 * model.playSize.y / 2 - 700))
                    ++ " "
                    ++ (toString 1400)
                    ++ " "
                    ++ (toString 1400)
                )

            --                  , viewBox "0 0 1200 1200"
            ]
            ([ Svg.rect
                [ x (toString (1 * model.playSize.x / 2 - 700))
                , y (toString (1 * model.playSize.y / 2 - 700))
                , width "1400"
                , height "1400"
                , fill "black"
                , stroke "green"
                , strokeWidth "13"
                ]
                []
             ]
                ++ (List.foldr
                        (\x xs ->
                            case
                                drawRadarObject
                                    (Vector rx ry)
                                    (Vector radarWidth radarHeight)
                                    model.screenZoom
                                    model.offset
                                    x
                            of
                                Nothing ->
                                    xs

                                Just svg ->
                                    svg :: xs
                        )
                        []
                        world.objects
                   )
            )
        ]


drawRadarObject :
    Vector
    -> Vector
    -> Float
    -> Vector
    -> GameObject XpilotMeta XpilotObject XpilotMsg
    -> Maybe (Html Msg)
drawRadarObject rpos rsize rzoom offset (GameObject obj) =
    case obj.self of
        Ship _ ->
            Just
                (Svg.rect
                    [ x (toString (obj.pos.x + offset.x))
                    , y (toString (obj.pos.y + offset.y))
                    , width "15"
                    , height "15"
                    , fill "white"
                    ]
                    []
                )

        Wall _ ->
            Just
                (drawShape (vectorAdd offset obj.pos)
                    [ fill "#2222dd" ]
                    obj.shape
                )

        _ ->
            Nothing



--


scoresWidth : Float
scoresWidth =
    170



--


scoresHeight : Float
scoresHeight =
    200



--


drawScores : Model -> Html Msg
drawScores model =
    let
        gs =
            model.gameState

        players =
            Dict.foldr (\_ p ps -> p :: ps)
                []
                gs.players

        splayers =
            List.sortBy (\(Player p) -> (p.stats.kills - p.stats.deaths) * -1) players
    in
        Svg.svg
            [ x <| toString 10
            , y <| toString 10
            , width <| toString scoresWidth
            , height <| toString scoresHeight
            , viewBox <|
                "0 0 "
                    ++ (toString scoresWidth)
                    ++ " "
                    ++ (toString scoresHeight)
            ]
            ([ Svg.rect
                [ x "0"
                , y "0"
                , width (toString scoresWidth)
                , height (toString scoresHeight)
                , fill "#000000"
                , fillOpacity "0.4"
                , stroke "green"
                , strokeWidth "1"
                ]
                []
             , Svg.text_
                [ fill "yellow"
                , textAnchor "left"
                , fontSize "0.9em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString 10
                , y <| toString <| 20
                ]
                [ Svg.text "Name" ]
             , Svg.text_
                [ fill "yellow"
                , textAnchor "left"
                , fontSize "0.9em"
                , fontStyle "normal"
                , fontWeight "Bold"
                , fontFamily "monospace"
                , x <| toString 80
                , y <| toString <| 20
                ]
                [ Svg.text <| "Kill Ratio" ]
             ]
                ++ List.indexedMap (printPlayerScore (Vector 10 30)) splayers
            )



--


printPlayerScore : Vector -> Int -> Player -> Html Msg
printPlayerScore offset i (Player p) =
    Svg.svg
        [ x <| toString offset.x
        , y <| toString <| offset.y + 15 * (toFloat i)
        , width "300"
        , height "15"

        --, viewBox "0 0 300 15"
        ]
        [ Svg.text_
            [ fill "#eee"
            , textAnchor "left"
            , fontSize "0.9em"
            , fontStyle "normal"
            , fontWeight "normal"
            , fontFamily "monospace"
            , x <| toString 0
            , y <| toString <| 15
            ]
            [ Svg.text p.name ]
        , Svg.text_
            [ fill "#eee"
            , textAnchor "left"
            , fontSize "0.9em"
            , fontStyle "normal"
            , fontWeight "Bold"
            , fontFamily "monospace"
            , x <| toString 100
            , y <| toString <| 15
            ]
            [ Svg.text <|
                (String.left 3 <|
                    toString
                        ((toFloat p.stats.kills)
                            / (toFloat (Basics.max 1 p.stats.deaths))
                        )
                )
            ]
        ]



--


splashWidth : Float
splashWidth =
    400



--


splashHeight : Float
splashHeight =
    320



--


welcomeSplash : Vector -> Html Msg
welcomeSplash screensize =
    let
        middle =
            vectorMultScalar 0.5 screensize
    in
        Svg.svg
            [ x <| toString <| middle.x - splashWidth / 2
            , y <| toString <| middle.y - splashHeight / 2
            , width <| toString splashWidth
            , height <| toString splashHeight
            , viewBox <| "0 0 " ++ toString splashWidth ++ " " ++ toString splashHeight
            ]
            [ Svg.rect
                [ x "0"
                , y "0"
                , width <| toString splashWidth
                , height <| toString splashHeight
                , fill "#000"
                , fillOpacity "0.7"
                , stroke "#000066"
                , rx "10"
                , ry "10"
                , strokeWidth "2"
                ]
                []
            , Svg.text_
                [ fill "#bbbb00"
                , textAnchor "middle"
                , fontSize "40pt"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 80
                ]
                [ Svg.text "Xpilot.IO" ]

            --
            , Svg.text_
                [ fill "#aaa"
                , textAnchor "middle"
                , fontSize "0.9em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 100
                ]
                [ Svg.text "Early Gameplay Demo" ]

            -- 'a' key
            , Svg.rect
                [ x "55"
                , y "120"
                , width <| toString 30
                , height <| toString 30
                , fill "#000"
                , stroke "#bbb"
                , rx "2"
                , ry "2"
                , strokeWidth "1"
                ]
                []
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70
                , y <| toString <| 140
                ]
                [ Svg.text "A" ]
            , Svg.rect
                [ x <| toString <| 55 + 40
                , y "120"
                , width <| toString 30
                , height <| toString 30
                , fill "#000"
                , stroke "#bbb"
                , rx "2"
                , ry "2"
                , strokeWidth "1"
                ]
                []
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70 + 40
                , y <| toString <| 140
                ]
                [ Svg.text "S" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "left"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70 + 40 + 40
                , y <| toString <| 140
                ]
                [ Svg.text "Turn left/right" ]
            , Svg.rect
                [ x <| toString <| 55 + 190
                , y <| toString <| 120 + 50
                , width <| toString 100
                , height <| toString 30
                , fill "#000"
                , stroke "#bbb"
                , rx "2"
                , ry "2"
                , strokeWidth "1"
                ]
                []
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "left"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70 + 13 + 190
                , y <| toString <| 140 + 50
                ]
                [ Svg.text "Enter" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "left"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70
                , y <| toString <| 140 + 50
                ]
                [ Svg.text "Shoot" ]
            , Svg.rect
                [ x <| toString <| 55 + 190
                , y <| toString <| 120 + 50 + 40
                , width <| toString 100
                , height <| toString 30
                , fill "#000"
                , stroke "#bbb"
                , rx "2"
                , ry "2"
                , strokeWidth "1"
                ]
                []
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "left"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70 + 13 + 190
                , y <| toString <| 140 + 50 + 40
                ]
                [ Svg.text "Shift" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "left"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| 70
                , y <| toString <| 140 + 50 + 40
                ]
                [ Svg.text "Thrust" ]
            , Svg.text_
                [ fill "#fff"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 140 + 50 + 40 + 50
                ]
                [ Svg.text "Or use arrow keys" ]
            , Svg.text_
                [ fill "#fff"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 140 + 50 + 40 + 50 + 20
                ]
                [ Svg.text "and Ctrl, Space, or X to shoot" ]
            , Svg.text_
                [ fill "#fff"
                , textAnchor "middle"
                , fontSize "1.0em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 140 + 50 + 40 + 50 + 60
                ]
                [ Svg.text "Press any key to begin." ]
            ]


donateSplash : Vector -> Html Msg
donateSplash screensize =
    let
        middle =
            vectorMultScalar 0.5 screensize
    in
        Svg.svg
            [ x <| toString <| middle.x - splashWidth / 2
            , y <| toString <| middle.y - splashHeight / 2
            , width <| toString splashWidth
            , height <| toString splashHeight
            , viewBox <| "0 0 " ++ toString splashWidth ++ " " ++ toString splashHeight
            ]
            [ Svg.rect
                [ x "0"
                , y "0"
                , width <| toString splashWidth
                , height <| toString splashHeight
                , fill "#000"
                , fillOpacity "0.7"
                , stroke "#000066"
                , rx "10"
                , ry "10"
                , strokeWidth "2"
                ]
                []
            , Svg.text_
                [ fill "#bbbb00"
                , textAnchor "middle"
                , fontSize "28pt"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 50
                ]
                [ Svg.text "Support Xpilot.IO" ]

            --
            , Svg.text_
                [ fill "#aaa"
                , textAnchor "middle"
                , fontSize "0.9em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 70
                ]
                [ Svg.text "Help fund further development:" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 100
                ]
                [ Svg.text "Multiplayer" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 125
                ]
                [ Svg.text "Maps" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 150
                ]
                [ Svg.text "Game Modes" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 175
                ]
                [ Svg.text "Make your own AI bots" ]
            , Svg.text_
                [ fill "#ddd"
                , textAnchor "middle"
                , fontSize "1.1em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 200
                ]
                [ Svg.text "Open Source" ]
            , Svg.text_
                [ fill "#bbbb00"
                , textAnchor "middle"
                , fontSize "20pt"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 260
                ]
                [ Svg.text "Our Kickstarter" ]
            , Svg.text_
                [ fill "#fff"
                , textAnchor "middle"
                , fontSize "1.0em"
                , fontStyle "normal"
                , fontWeight "normal"
                , fontFamily "monospace"
                , x <| toString <| splashWidth / 2
                , y <| toString <| 300
                ]
                [ Svg.text "Press any key to continue playing." ]
            ]



--


px : String -> String
px s =
    s ++ "px"



--


wants : List String
wants =
    [ "Want to fight against human players instead of bots?"
    , "Want bigger maps with better walls?"
    , "Want Capture the Flag, Zombie, king-of-the-hill, game modes?"
    , "Do you want to program you own AI bots and send them out to fight against real players?"
    , "Want to program your own wingman, or secret-service AI who will take a bullet for you?"
    , "Please support Open Source software!"
    , "Want to play against dozens of real players?"
    , "Want an action-strategy game mode with resource collection, turrets, weapons and armor upgrades?"
    , "Support a great Artificial Intelligence research platform!"
    ]



--
--
--


drawDonateSplash : Vector -> String -> Html Msg
drawDonateSplash screensize msg =
    let
        middle =
            vectorMultScalar 0.5 screensize
    in
        div
            [ Attr.style
                [ ( "color", "#eee" )
                , ( "position", "absolute" )
                , ( "top", px <| toString <| middle.y - splashHeight / 2 )
                , ( "left", px <| toString <| middle.x - splashWidth / 2 )
                , ( "width", px <| toString <| splashWidth )
                , ( "height", px <| toString <| splashHeight )
                ]
            ]
            [ div
                [ Attr.style
                    [ ( "color", "#eee" )
                    , ( "width", "100%" )
                    , ( "text-align", "center" )
                    , ( "font-size", "16pt" )
                    , ( "font-family", "monospace" )
                    , ( "line-height", "28px" )
                    ]
                ]
                [ text msg ]
            , div
                [ Attr.style
                    [ ( "color", "yellow" )
                    , ( "width", px <| toString <| splashWidth / 1.5 )
                    , ( "margin", "10px auto" )
                    , ( "height", px <| toString <| splashHeight / 4 )
                    , ( "border", "1px solid yellow" )
                    , ( "border-radius", "10px" )
                    ]
                ]
                [ a
                    [ Attr.href "https://www.kickstarter.com/projects/196267783/xpilotio"
                    , Attr.style [ ( "text-decoration", "none" ) ]
                    ]
                    [ div
                        [ Attr.style
                            [ ( "color", "yellow" )
                            , ( "height", "100%" )
                            , ( "width", "100%" )
                            , ( "text-align", "center" )
                            , ( "font-size", "20pt" )
                            , ( "font-family", "monospace" )
                            , ( "text-decoration", "none" )
                            , ( "line-height", "29px" )
                            , ( "margin-top", px <| toString <| 10 )
                            ]
                        ]
                        [ text "Support On Kickstarter" ]
                    ]
                ]
            ]
