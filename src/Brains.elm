module Brains exposing (..)

import GameEngine exposing (..)
import Vector exposing (..)
--import Shape exposing (..)
--import Collision exposing (..)
import XpilotTypes exposing (..)

----------------Util---------------

-- distance without the ^0.5 for comparisons
distanceComp : Vector -> Vector -> Float
distanceComp v1 v2 = (v1.x - v2.x)^2 + (v1.y - v2.y)^2
--

nearestObj : GameObject a b c -> List (GameObject a b c) -> Maybe (GameObject a b c)
nearestObj (GameObject self) objects =
    case
        (List.foldr (\ (GameObject obj) closest ->
                         if obj.id == self.id then
                             closest
                         else
                             let newd = distanceComp self.pos obj.pos in
                             case closest of
                                 Nothing -> Just (newd, GameObject obj)
                                 (Just (d, GameObject cobj)) ->
                                     if newd < d then
                                         Just (newd, GameObject obj)
                                     else
                                         closest)
            Nothing
            objects)
    of
        Nothing -> Nothing
        (Just (_, closestObj)) -> Just closestObj
--


bulletDanger : Vector -> Vector -> Vector -> Vector -> Float
bulletDanger spos svel bpos bvel =
    let tx = -1 * (bpos.x - spos.x) / (bvel.x - svel.x)
        ty = -1 * (bpos.y - spos.y) / (bvel.y - svel.y)
    in
        if tx < 0 || ty < 0 then 0
        else 1000 * (1 / (max 0.001 (abs (tx - ty))))

mostDangerousBullet : GameObject a XpilotObject c -> List (GameObject a XpilotObject c)
                    -> Maybe (Float, GameObject a XpilotObject c)
mostDangerousBullet (GameObject self) bullets =
    List.foldr (\ (GameObject b) mdb-> let nd = bulletDanger self.pos self.vel b.pos b.vel
                                       in
                                           case mdb of
                                               Nothing -> Just (nd, GameObject b)
                                               Just (d, db) ->
                                                   if nd > d then Just (nd, GameObject b)
                                                   else Just (d, db))
        Nothing bullets


--

allShips : List (GameObject a XpilotObject c) -> List (GameObject a XpilotObject c)
allShips = List.filter (\ (GameObject obj) -> case obj.self of
                                           (Ship _) -> True
                                           _ -> False )
--
allBullets : List (GameObject a XpilotObject c) -> List (GameObject a XpilotObject c)
allBullets = List.filter (\ (GameObject obj) -> case obj.self of
                                           (Bullet _) -> True
                                           _ -> False )
defaultBrain : ShipBrain
defaultBrain (World {meta}) ship =
    ShipControls { thrust = meta.shipMaxThrust
                 , shoot = True
                 , turn = meta.shipMaxTurn }
--
doNothing : ShipControls
doNothing = ShipControls { thrust = 0
                         , shoot = False
                         , turn = 0}
--

sidAttack : ShipBrain
sidAttack (World {meta, objects}) (GameObject ship) =
    case ship.self of
        (Ship self) ->
            let ships = allShips objects
                cShip = nearestObj (GameObject ship) ships
            in
                case cShip of
                    Nothing ->
                        ShipControls { thrust = 0
                                     , shoot = False
                                     , turn = 4}
                    Just (GameObject s) ->
                        let aimdir = aimDir s.pos ship.pos
                        in
                            if (abs (aimdir - self.dir)) < 0.1
                                && self.shotCharge > (meta.shotCharge / 3) then
                                ShipControls { thrust = 1.00
                                             , shoot = True
                                             , turn = angleDiff aimdir  self.dir }
                            else
                                ShipControls { thrust = 0
                                             , shoot = False
                                             , turn = angleDiff aimdir  self.dir }
        _ -> doNothing
--
sidEvadeBullet : GameObject a XpilotObject b -> Float -> GameObject a XpilotObject b
               -> ShipControls
sidEvadeBullet (GameObject ship) danger (GameObject bullet) =
    let rm = rotationMatrix (aimDir (Vector 0 0) ship.vel)
        bpos = rotatePointWithMatrix rm (vectorSub bullet.pos ship.pos)
        bvel = rotatePointWithMatrix rm (vectorSub bullet.vel ship.vel)
        t = bpos.y / bvel.y
        bx = bpos.x + bvel.x * t
    in
    ShipControls { thrust = 1.0
                 , shoot = False
                 , turn = if bx > 0 then -9 else 9 }

sid : ShipBrain
sid (World w) (GameObject ship) =
    case ship.self of
        (Ship self) ->
            case mostDangerousBullet (GameObject ship) <| allBullets w.objects of
                Nothing ->
                    sidAttack (World w) (GameObject ship)
                    --doNothing
                Just (d, b) -> if d > 1 then
                                   sidEvadeBullet (GameObject ship) d b
                               else
                                   --doNothing
                                   sidAttack (World w) (GameObject ship)

        _ -> doNothing
--
aimAtVel : Float -> Vector -> Vector -> Maybe Float
aimAtVel bv p v =
    if (p.x == 0 || p.y == 0) then
        Nothing
    else
        Just
        ( asin (
                ((v.y / bv) - ((p.y * v.x) / (p.x * bv)))
                    / (1 + (p.x / p.y)^2)
               - (atan2 (p.x / p.y) 1))
        )

aimAtVel2 : Float -> Vector -> Vector -> Maybe Float
aimAtVel2 bv p v =
    if p.x == 0 || p.y == 0 then
        Nothing
    else
        Just
        ( let angle = (asin (
                             ((v.y / bv) - ((p.y * v.x) / (p.x * bv))))
                      - (atan2 (p.x / p.y) 1))
          in
              within2pi
              (if p.y > 0 then
                   if p.x > 0 then
                       pi - angle * -1
                   else
                       angle * 1 + pi / 1
               else
                   if p.x > 0 then
                       angle
                   else
                       -1 * angle - pi / 2)
        )
--
aimAtShip : Float
          -> GameObject XpilotMeta XpilotObject XpilotMsg
          -> GameObject XpilotMeta XpilotObject XpilotMsg
          -> Maybe Float
aimAtShip bvel (GameObject ego) (GameObject other) =
    (aimAtVel2 bvel (vectorSub other.pos ego.pos)
        (vectorSub other.vel ego.vel))

--

selAttack : ShipBrain
selAttack (World {meta, objects}) (GameObject ship) =
    case ship.self of
        (Ship self) ->
            let ships = allShips objects
                cShip = nearestObj (GameObject ship) ships
            in
                case cShip of
                    Nothing ->
                        ShipControls { thrust = 0
                                     , shoot = False
                                     , turn = 4}
                    Just (GameObject s) ->
                        case aimAtShip meta.shotVel (GameObject ship) (GameObject s) of
                            Nothing -> doNothing
                            Just aimdir ->
                                if isNaN aimdir then
                                    (let aimdir = aimDir s.pos ship.pos
                                     in
                                         if (abs (aimdir - self.dir)) < 0.1
                                         && self.shotCharge > (meta.shotCharge / 3) then
                                             ShipControls { thrust = 1.00
                                                          , shoot = True
                                                          , turn = angleDiff aimdir  self.dir }
                                         else
                                             ShipControls { thrust = 0
                                                          , shoot = False
                                                          , turn = angleDiff aimdir  self.dir })
                                else
                                    if (abs (aimdir - self.dir)) < 0.01
                                        && self.shotCharge > (meta.shotCharge / 3) then
                                        ShipControls { thrust = 0
                                                     , shoot = True
                                                     , turn = angleDiff aimdir  self.dir }
                                    else
                                        ShipControls { thrust = if abs (ship.vel.y + ship.vel.x) < 0.1 then 1.0 else 0
                                                     , shoot = False
                                                     , turn = angleDiff aimdir  self.dir }
        _ -> doNothing
--

sel : ShipBrain
sel (World w) (GameObject ship) =
    case ship.self of
        (Ship self) ->
            case mostDangerousBullet (GameObject ship) <| allBullets w.objects of
                Nothing ->
                    selAttack (World w) (GameObject ship)
                    --doNothing
                Just (d, b) -> if d > 2 then
                                   sidEvadeBullet (GameObject ship) d b
                                   --doNothing
                               else
                                   --doNothing
                                   selAttack (World w) (GameObject ship)

        _ -> doNothing
