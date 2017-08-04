module XpilotTypes exposing (..)

import GameEngine exposing (..)
import Dict exposing (Dict)
import Keyboard exposing (KeyCode)
import Time exposing (Time)
import Vector exposing (..)
import Shape exposing (..)


type alias XpilotMeta =
    { controlState : List ControlAction
    , keyboardControls : ControlMapping
    , playerDeadTime : Time
    , shotCharge : Float
    , shotChargeRate : Float
    , shotChargeCost : Float
    , shotRepeatRate : Float
    , shotLife : Time
    , shotVel : Float
    , shotRecoil : Float
    , shipMaxThrust : Float
    , shipMaxTurn : Float
    , shipEmberSpeed : Float
    , shipEmberRate : Float
    , shipExplosionSize : Int -- number of embers
    , emberReduceRate : Float
    }


type XpilotObject
    = Bullet
        { shooter : ObjectID
        , shotLife : Time
        }
    | Ship
        { dir : Float

        --                         , id : Int
        , shape : List Vector
        , gunPos : Vector
        , thrusterPos : Vector
        , shotCharge : Float
        , timeSinceLastShot : Time
        , name : String
        , brain : ShipBrain
        }
    | Wall {}
    | Ember
        { strength : Float
        , age : Time
        , owner : ObjectID
        }


type XpilotMsg
    = Explosion Vector Vector (List Float)
    | StrayBullet Vector Vector (List Float)
    | MaybeThrustEmber Time ObjectID Vector Vector Float (List Float)
    | ThrustEmber ObjectID Vector Vector Float (List Float)
    | Killed ObjectID ObjectID XpilotObject
    | Respawn (GameObject XpilotMeta XpilotObject XpilotMsg) Vector Vector
    | RespawnAt (GameObject XpilotMeta XpilotObject XpilotMsg) Vector Vector (List Float)


type ControlAction
    = Thrust
    | TurnRight
    | TurnLeft
    | Shoot


type alias ControlMapping =
    Dict KeyCode ControlAction


type ShipControls
    = ShipControls
        { turn : Float
        , thrust : Float
        , shoot : Bool
        }


type alias ShipBrain =
    World XpilotMeta XpilotObject XpilotMsg
    -> GameObject XpilotMeta XpilotObject XpilotMsg
    -> ShipControls
