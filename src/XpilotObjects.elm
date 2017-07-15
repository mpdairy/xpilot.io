module XpilotObjects exposing (..)

import Dict exposing (Dict)
import GameEngine exposing (..)
import Time exposing (Time)
import Vector exposing (..)
import Shape exposing (..)
import Collision exposing (..)
import XpilotTypes exposing (..)
import Keyboard exposing (KeyCode)
--import Brains exposing (..)
--import Debug


---------Message Handler--------

mapBy3 : (a-> a -> a -> b) -> List a -> List b
mapBy3 f ls = case ls of
                  [] -> []
                  [a] -> []
                  [a,b] -> []
                  (a::b::c::xs) -> (f a b c) :: (mapBy3 f xs)

xpilotMsgHandler : GameMsgHandler XpilotMeta XpilotObject XpilotMsg
xpilotMsgHandler msg world =
    case msg of
        Respawn ship topright bottomleft ->
            ( world,
              [ Randoms 2 (RespawnAt ship topright bottomleft) ] )
        RespawnAt (GameObject ship) tr bl randoms ->
            case randoms of
                (r1::r2::_) ->
                    ( world,
                          [ NewObjectKeepID
                                ( GameObject
                                      { ship
                                          | pos = Vector
                                                  (tr.x + r1 * (bl.x - tr.x))
                                                  (tr.y + r2 * (bl.x - tr.x)) } )
                                ])
                _ -> (world, [])
        Explosion pos vel rands ->
            (world
            , mapBy3 (explodeEmber pos vel) rands)
        MaybeThrustEmber diff ownerID pos vel strength rand ->
            let (World {meta}) = world
            in
                (world
                , case rand of
                      [rand] -> if rand < diff * meta.shipEmberRate then
                                    [ Randoms 2 (ThrustEmber ownerID pos vel strength) ]
                                else []
                      _ -> [] )
        ThrustEmber ownerID pos vel strength rands ->
            (world
            , case rands of
                  (rdir::rvel::_) ->
                      [ NewObject (newEmber
                                       ownerID
                                       { pos
                                           | x = pos.x
                                           , y = pos.y }
                                       { vel
                                           | x = vel.x + 0.2 * (rdir-0.5)
                                           , y = vel.y + 0.2 * (rvel-0.5)}
                                       strength ) ]
                  _ ->
                      [])
        StrayBullet pos vel rand ->
            ( world
            , case rand of
                  [speed] ->
                      [ NewObject (newBullet 0 pos
                                       {vel
                                           | x = vel.x * speed
                                           , y = vel.y * speed }) ]
                  _ ->
                      [])
        _ -> (world, [])

---------BULLETS------------
bulletCollision : CollisionReaction XpilotMeta XpilotObject XpilotMsg
bulletCollision (World world) (GameObject bullet) (GameObject other) =
    (GameObject
         {bullet
             | destroy = True }
         , [])

bulletCollideWith : XpilotObject -> Bool
bulletCollideWith x =
    case x of
        Ship _ -> True
        Wall _ -> True
        _ -> False


bulletControl : Time
              -> World XpilotMeta XpilotObject XpilotMsg
              -> GameObject XpilotMeta XpilotObject XpilotMsg
              -> ( GameObject XpilotMeta XpilotObject XpilotMsg
                 , List (GameMessage XpilotMeta XpilotObject XpilotMsg) )
bulletControl diff (World {meta}) (GameObject object) =
    case object.self of
        Bullet bullet ->
            let newShotLife = bullet.shotLife + diff
            in
                if newShotLife > meta.shotLife then
                    (GameObject { object | destroy = True }, [])
                else
                    let newBullet = Bullet { bullet | shotLife = newShotLife }
                    in
                        (GameObject { object | self = newBullet }, [])
        _ -> ( GameObject object, [] )


newBullet : Int -> Vector -> Vector -> GameObject XpilotMeta XpilotObject XpilotMsg
newBullet shooter pos vel =
    let
        (GameObject obj) = newGameObject ( Bullet { shooter = shooter
                                                  , shotLife = 0 } )
        collisions = CollisionHandler
                     { collisionReaction = bulletCollision
                     , canCollideWith = bulletCollideWith }
    in
        GameObject
        { obj
            | pos = pos
            , vel = vel
            , shape = Point
            , collisionHandler = collisions
            , controlHandler = ControlHandler bulletControl
        }


-----------SHIPS----------------

shipCollision : CollisionReaction XpilotMeta XpilotObject XpilotMsg
shipCollision (World world) (GameObject ship) (GameObject other) =
    case other.self of
        (Bullet shot) ->
            if ship.destroy then
                ( GameObject ship, [] )
            else
                ( GameObject { ship | destroy = True }
                , [ newExplosion ship.pos ship.vel world.meta.shipExplosionSize
                  , Game (Killed ship.id shot.shooter other.self)] )
        (Ship _) ->
            ( GameObject { ship | destroy = True }
            , [ newExplosion ship.pos ship.vel world.meta.shipExplosionSize
              , Game (Killed ship.id other.id other.self)
              ] )
        (Ember ember) ->
            ( GameObject
                  { ship |
                        forces = if ember.owner /= ship.id then
                                     (vectorMultScalar other.mass other.vel)
                                     :: ship.forces
                                 else
                                     ship.forces
                  }
            , []  )
        (Wall _) ->
            let xLap = spanOverlap ship.boundingBox.xSpan other.boundingBox.xSpan
                yLap = spanOverlap ship.boundingBox.ySpan other.boundingBox.ySpan
                newVel = if xLap > yLap then
                             (Vector ship.vel.x (-0.4 * ship.vel.y))
                         else
                             (Vector (-0.4 * ship.vel.x) ship.vel.y)
                newPos = if xLap > yLap then
                             (Vector
                                  ship.pos.x
                                  (if ship.pos.y < other.pos.y then
                                       ship.pos.y - yLap
                                   else
                                       ship.pos.y + yLap))
                         else
                             (Vector
                                  (if ship.pos.x < other.pos.x then
                                          ship.pos.x - xLap
                                      else
                                          ship.pos.x + xLap)
                                  ship.pos.y)
                lethal = ((ship.vel.x^2 + ship.vel.y^2)^0.5) > 0.8
            in
            ( GameObject
                  { ship
                      | vel = newVel
                      , pos = newPos
                      , destroy = lethal
                  }
            , if lethal then
                  [ Game (Killed ship.id other.id other.self)
                  , newExplosion newPos newVel world.meta.shipExplosionSize]
            else
                [] )


shipControl : Time -> World XpilotMeta XpilotObject XpilotMsg
            -> GameObject XpilotMeta XpilotObject XpilotMsg
            -> ( GameObject XpilotMeta XpilotObject XpilotMsg
               , List (GameMessage XpilotMeta XpilotObject XpilotMsg))
shipControl diff (World world) (GameObject ship) =
    case ship.self of
        Ship self ->
            let
                meta = world.meta
                (ShipControls control) = self.brain (World world) (GameObject ship)
                newdir = within2pi(self.dir + (clamp ((-1) * world.meta.shipMaxTurn)
                                                   (world.meta.shipMaxTurn)
                                                   control.turn)
                                       * diff )
                rMatrix = rotationMatrix newdir
                newShotCharge = min meta.shotCharge
                                    (self.shotCharge + (meta.shotChargeRate * diff))
                newTimeSinceLastShot = self.timeSinceLastShot + diff
                shooting = control.shoot
                         && (newTimeSinceLastShot >= (1 / meta.shotRepeatRate))
                         && (newShotCharge >= meta.shotChargeCost)
                newshape = if newdir == self.dir then
                               ship.shape
                           else
                               Polygon (rotatePointsWithMatrix rMatrix self.shape)
                newself = Ship { self
                                   | dir = newdir
                                   , timeSinceLastShot =
                                     if shooting then
                                         0
                                     else
                                         newTimeSinceLastShot
                                   , shotCharge =
                                     if shooting then
                                         newShotCharge - meta.shotChargeCost
                                     else
                                         newShotCharge
                               }
                thrust = if (control.thrust == 0) then
                             []
                         else
                             [vectorMultScalar
                                  -1.0
                                  (rotatePointWithMatrix rMatrix
                                       (Vector 0
                                            (clamp ((-1) * world.meta.shipMaxThrust)
                                                   (world.meta.shipMaxThrust)
                                                   control.thrust)))]
                recoil = if shooting then
                             [(vectorMultScalar 1
                              (rotatePointWithMatrix rMatrix
                                   (Vector 0 meta.shotRecoil)))]
                         else
                             []
            in
                ( GameObject { ship
                                     | shape = newshape
                                     , self = newself
                                     , forces = List.concat
                                        [ thrust
                                        , recoil
                                        , ship.forces]}
                , List.concat
                    [ (if shooting then
                           let rgunPos = rotatePointWithMatrix rMatrix self.gunPos
                               shotVel = vectorAdd ship.vel
                                         (rotatePointWithMatrix rMatrix
                                              (Vector 0 (-1 * meta.shotVel)))
                               shotPos = (vectorAdd (vectorAdd ship.pos rgunPos)
                                              shotVel)
                           in
                               [ NewObject (newBullet ship.id shotPos shotVel) ]
                       else
                           [])
                    , (if control.thrust > 0 then
                           let rthrustPos = rotatePointWithMatrix rMatrix self.thrusterPos
                               thrustPos = vectorAdd ship.pos rthrustPos
                               thrustVel = vectorAdd (vectorMultScalar 0.5 ship.vel)
                                           (vectorMultScalar -1.0
                                                (rotatePointWithMatrix rMatrix
                                                     (Vector 0 (-1 * meta.shipEmberSpeed))))
                           in
                               [Randoms 1 (MaybeThrustEmber diff ship.id thrustPos thrustVel 1)]
                       else
                           [])
                    , [] ] -- can add more messages later
                )
        _ -> (GameObject ship, [])

shipCollideWith : XpilotObject -> Bool
shipCollideWith x =
    case x of
        Ship _ -> True
        Wall _ -> True
        Bullet _ -> True
        Ember _ -> True
--        _ -> False

newShip : XpilotMeta -> Int -> String -> Float -> Vector -> ShipBrain
        -> GameObject XpilotMeta XpilotObject XpilotMsg
newShip meta id name dir pos brain =
    let shipshape = [(Vector -7 6), (Vector 7 6), (Vector 0 -15)]
        collisions =  CollisionHandler
                      { collisionReaction = shipCollision
                      , canCollideWith = shipCollideWith }
        (GameObject obj) = newGameObject ( Ship { dir = dir
                                                --, id = id
                                                , name = name
                                                , shape = shipshape
                                                , gunPos = Vector 0 -16
                                                , thrusterPos = Vector 0 7
                                                , shotCharge = meta.shotCharge
                                                , timeSinceLastShot = (0 * Time.second)
                                                , brain = brain
                                                } )
    in
        GameObject
        { obj
            | pos = pos
            , vel = Vector 0 0
            , id = id
            , shape = Polygon shipshape
            , collisionHandler = collisions
            , controlHandler = ControlHandler shipControl}



---player keyboard---


keyboardBrain : ShipBrain
keyboardBrain (World {meta}) _ =
    let cs = meta.controlState
    in
        ShipControls
        { thrust = (if List.member Thrust cs then meta.shipMaxThrust else 0)
        , turn = (if List.member TurnRight cs then (-1 * meta.shipMaxTurn) else 0)
                 + (if List.member TurnLeft cs then meta.shipMaxTurn else 0)
        , shoot = List.member Shoot cs }

addControlAction : ControlAction -> XpilotMeta -> XpilotMeta
addControlAction action meta =
    { meta | controlState = action :: meta.controlState }

rmControlAction : ControlAction -> XpilotMeta -> XpilotMeta
rmControlAction action meta =
    { meta | controlState = List.filter (\ a -> action /= a) meta.controlState }

handleKeyDown : KeyCode -> World XpilotMeta a b -> World XpilotMeta a b
handleKeyDown = handleKey addControlAction

handleKeyUp : KeyCode -> World XpilotMeta a b -> World XpilotMeta a b
handleKeyUp = handleKey rmControlAction

handleKey : (ControlAction -> XpilotMeta -> XpilotMeta)
          -> KeyCode -> World XpilotMeta a b
          -> World XpilotMeta a b
handleKey f k (World world) =
    let maybeAction = (Dict.get k world.meta.keyboardControls)
    in
        World
        (case maybeAction of
             Nothing ->
                 world
             Just action ->
                 { world | meta = f action world.meta })






---------EMBERS------------
emberCollision : CollisionReaction XpilotMeta XpilotObject XpilotMsg
emberCollision (World world) (GameObject ember) (GameObject other) =
    case other.self of
        {-Wall _ -> (GameObject { ember
                                     | vel = vectorMultScalar -1 ember.vel
                                     , id = -1 }, [])-}
        Ship _ -> if other.id /= ember.id then
                      (GameObject ember, [])
                  else
                      (GameObject { ember | destroy = True }, [])
        _ -> (GameObject { ember | destroy = True }, []) 

emberCollideWith : XpilotObject -> Bool
emberCollideWith x =
    case x of
        Ship _ -> True
        Wall _ -> True
        _ -> False


emberControl : Time -> World XpilotMeta XpilotObject XpilotMsg
             -> GameObject XpilotMeta XpilotObject XpilotMsg
             -> ( GameObject XpilotMeta XpilotObject XpilotMsg
                , List (GameMessage XpilotMeta XpilotObject XpilotMsg) )
emberControl diff (World {meta}) (GameObject object) =
    case object.self of
        Ember ember ->
            let newStrength = ember.strength - meta.emberReduceRate * diff
            in
                if newStrength < 0 then
                    (GameObject { object | destroy = True }, [])
                else
                    let updatedEmber = Ember { ember | strength = newStrength
                                             , age = ember.age + diff}
                    in
                        (GameObject
                         { object
                             | self = updatedEmber
                             , vel = vectorMultScalar ember.strength object.vel
                             , vel = object.vel }
                        , [])
        _ -> ( GameObject object, [] )

newEmber : ObjectID -> Vector -> Vector -> Float
         -> GameObject XpilotMeta XpilotObject XpilotMsg
newEmber ownerID pos vel strength =
    let
        (GameObject obj) = newGameObject ( Ember { strength = strength
                                                 , age = 0.0
                                                 , owner = ownerID } )
        collisions = CollisionHandler
                     { collisionReaction = emberCollision
                     , canCollideWith = emberCollideWith }
    in
        GameObject
        { obj
            | pos = pos
            , vel = vel
            , mass = 0.005
            , shape = Point
            , collisionHandler = collisions
            , controlHandler = ControlHandler emberControl
        }


------- EXPLOSION ---------

newExplosion : Vector -> Vector -> Int
             -> GameMessage XpilotMeta XpilotObject XpilotMsg
newExplosion pos vel numEmbers =
    Randoms (numEmbers * 3) (Explosion pos vel)

explodeEmber : Vector -> Vector -> Float -> Float -> Float
             -> GameMessage XpilotMeta XpilotObject XpilotMsg
explodeEmber pos vel hr vr sr =
    let c = vr * hr in
    NewObject (newEmber
                   -1
                   { pos
                       | x = pos.x
                       , y = pos.y }
                   { vel
                       | x = 0.4 * (vel.x + 0.5 * (hr - 0.5)) / c
                       , y = 0.4 * (vel.y + 0.5 * (vr - 0.5)) / c}
                    (sr * 4.0))



---------- WALLS --------------

wallCollision : CollisionReaction XpilotMeta XpilotObject XpilotMsg
wallCollision (World world) (GameObject wall) (GameObject other) =
    ((GameObject wall), []) 

wallCollideWith : XpilotObject -> Bool
wallCollideWith x = False

newWall : Vector -> Vector -> GameObject XpilotMeta XpilotObject XpilotMsg
newWall pos wh =
    let
        (GameObject obj) = newGameObject ( Wall { } )
        collisions = CollisionHandler
                     { collisionReaction = wallCollision
                     , canCollideWith = wallCollideWith }
        wh2 = vectorMultScalar 0.5 wh
    in
        GameObject
        { obj
            | pos = pos
            , shape = Polygon [ (Vector (wh2.x * -1) (wh2.y * -1))
                              , (Vector (wh2.x * -1) (wh2.y * 1))
                              , (Vector (wh2.x * 1) (wh2.y * 1))
                              , (Vector (wh2.x * 1) (wh2.y * -1))]
            , collisionHandler = collisions
            , boundingBox = shapeToBoundingBox pos
                            (Polygon [(Vector (wh2.x * -1) (wh2.y * -1))
                              , (Vector (wh2.x * -1) (wh2.y * 1))
                              , (Vector (wh2.x * 1) (wh2.y * 1))
                              , (Vector (wh2.x * 1) (wh2.y * -1))])
            , controlHandler = NoControl
        }
