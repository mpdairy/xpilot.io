module GameEngine
    exposing
        ( CollisionHandler(NoCollision, CollisionHandler)
        , CollisionReaction
        , ObjectID
        , GameMessage(..)
        , GameMsgHandler
        , ControlHandler(NoControl, ControlHandler)
        , DisplayHandler(NoDisplay, DisplayHandler)
          --  , Shape (Point, Circle, Rectangle, Polygon)
        , GameObject(GameObject)
        , World(World)
        , initWorld
        , updateWorld
        , newGameObject
        , handleGameMessage
        , handleGameMessages
        )

{-| A little 2-D game engine that does.


# Types

@docs CollisionHandler, ControlHandler, DisplayHandler, GameObject, updateWorld, newGameObject, CollisionReaction, World, GameMessage, ObjectID, handleGameMessage, handleGameMessages, GameMsgHandler, initWorld

-}

import Shape exposing (..)
import Vector exposing (..)
import Time exposing (Time)
import Collision exposing (..)
import Random


--import Debug exposing (..)
--import Dict exposing (Dict)


{-| Standard game object with physics attributes.
-}
type World m a msg
    = World
        { meta : m
        , currentObjectID : ObjectID
        , randomSeed : Random.Seed
        , msgHandler : GameMsgHandler m a msg
        , debug : String
        , objects : List (GameObject m a msg)
        }


{-| ok
-}
type alias GameMsgHandler m a msg =
    msg
    -> World m a msg
    -> ( World m a msg, List (GameMessage m a msg) )


{-| ok
-}
initWorld :
    m
    -> GameMsgHandler m a msg
    -> World m a msg
initWorld m msgHandler =
    World
        { meta = m
        , currentObjectID = 10000
        , randomSeed = Random.initialSeed 434
        , msgHandler = msgHandler
        , debug = ""
        , objects = []
        }


{-| Standard game object with physics attributes.
-}
type GameObject m a msg
    = GameObject
        { self : a
        , id : ObjectID
        , pos : Vector
        , vel : Vector
        , acc : Vector
        , mass : Float
        , forces : List Vector
        , dir : Float
        , destroy : Bool
        , shape : Shape
        , rotatedShape : Shape
        , boundingBox : BoundingBox
        , needsRebounding : Bool
        , collisionHandler : CollisionHandler m a msg
        , controlHandler : ControlHandler m a msg
        }


{-| ok
-}
type GameMessage m a msg
    = NewObject (GameObject m a msg)
    | NewObjectKeepID (GameObject m a msg)
    | RemoveObject ObjectID
    | AddForce ObjectID Vector
    | Randoms Int (List Float -> msg)
    | Debug String
    | Game msg



{- Represents various shapes that the GameEngine can handle -}
{- type Shape = Point
   | Circle Float           --  radius
   | Rectangle Float Float  -- width, height
   | Polygon (List Vector)  -- List of points around 0, 0 center.
-}
--type BoundingBox = BoundingBox
--findBoundingBox : Shape -> BoundingBox
--findBoundingBox shape =


{-| Either it has as mass value, is fixed in place, or floats around without mass.
-}
type Mass
    = Mass Float
    | Fixed
    | Floating


{-| ok elm
-}
type alias CollisionReaction m a msg =
    World m a msg
    -> GameObject m a msg
    -> GameObject m a msg
    -> ( GameObject m a msg, List (GameMessage m a msg) )


{-| Whenever two objects collide, self object -> foreign object -> [new objects].
You can get rid of any object by simply not including it in the list.
-}
type CollisionHandler m a msg
    = NoCollision
    | CollisionHandler
        { canCollideWith : a -> Bool
        , collisionReaction : CollisionReaction m a msg
        }


{-| Called every control frame. Gets the world and itself.
You can add i.e. bullets to the list of Objects to return.
-}
type ControlHandler m a msg
    = NoControl
    | ControlHandler
        (Time
         -> World m a msg
         -> GameObject m a msg
         -> ( GameObject m a msg, List (GameMessage m a msg) )
        )


{-| Called every animation frame to draw the thing
-}
type DisplayHandler m a msg b
    = NoDisplay
    | DisplayHandler (World m a msg -> GameObject m a msg -> b)


{-| ok
-}
type alias ObjectID =
    Int


{-| ok
-}
handleGameMessage :
    GameMessage m a msg
    -> World m a msg
    -> ( World m a msg, List (GameMessage m a msg) )
handleGameMessage message (World world) =
    case message of
        Debug str ->
            ( World { world | debug = str }, [] )

        NewObjectKeepID obj ->
            ( World { world | objects = obj :: world.objects }, [] )

        NewObject (GameObject obj) ->
            let
                newID =
                    world.currentObjectID + 1

                objWithID =
                    GameObject { obj | id = newID }
            in
                ( World
                    { world
                        | currentObjectID = world.currentObjectID + 1
                        , objects = objWithID :: world.objects
                    }
                , []
                )

        Randoms n constructor ->
            let
                ( rx, seed ) =
                    randoms n world.randomSeed []
            in
                world.msgHandler
                    (constructor rx)
                    (World { world | randomSeed = seed })

        Game msg ->
            world.msgHandler msg (World world)

        _ ->
            ( World world, [] )


randoms : Int -> Random.Seed -> List Float -> ( List Float, Random.Seed )
randoms n seed xs =
    if (n == 0) then
        ( xs, seed )
    else
        let
            ( x, nextSeed ) =
                Random.step (Random.float 0 1) seed
        in
            randoms (n - 1) nextSeed (x :: xs)


{-| ok
-}
handleGameMessages : List (GameMessage m a msg) -> World m a msg -> World m a msg
handleGameMessages messages world =
    case messages of
        [] ->
            world

        m :: mx ->
            let
                ( newWorld, newMessages ) =
                    handleGameMessage m world
            in
                handleGameMessages (newMessages ++ mx) newWorld


{-| creates empty game object
-}
newGameObject : a -> GameObject m a msg
newGameObject obj =
    GameObject
        { self = obj
        , id = 0
        , pos = Vector 0 0
        , vel = Vector 0 0
        , acc = Vector 0 0
        , mass = 1.0
        , forces = []
        , dir = pi / 4
        , destroy = False
        , shape = Point
        , rotatedShape = Point
        , boundingBox = Collision.shapeToBoundingBox (Vector 0 0) Point
        , needsRebounding = True
        , collisionHandler = NoCollision
        , controlHandler = NoControl
        }


{-| gets the list of points from a Shape
-}
shapePoints : Shape -> List Vector
shapePoints shape =
    case shape of
        Point ->
            [ Vector 0 0 ]

        Circle _ ->
            [ Vector 0 0 ]

        Polygon points ->
            points


{-| Update all the objects in the game ok
-}
updateWorld : Time -> World m a msg -> ( World m a msg, List (GameMessage m a msg) )
updateWorld diff world =
    let
        ( controlledWorld, controlMessages ) =
            updatePhysics diff world
                |> resetPhysics
                |> updateControls diff

        ( collidedWorld, collidedMessages ) =
            updateCollisions diff controlledWorld
    in
        ( clearDestroyed collidedWorld, controlMessages ++ collidedMessages )


updateControls : Time -> World m a msg -> ( World m a msg, List (GameMessage m a msg) )
updateControls diff (World world) =
    let
        ( objects, messages ) =
            List.foldr
                (\(GameObject object) ( objects, messages ) ->
                    if object.destroy then
                        ( objects, messages )
                    else
                        (case object.controlHandler of
                            NoControl ->
                                ( (GameObject object) :: objects, messages )

                            ControlHandler f ->
                                let
                                    ( obj, msgs ) =
                                        (f diff
                                            (World world)
                                            (GameObject object)
                                        )
                                in
                                    ( obj :: objects, msgs ++ messages )
                        )
                )
                ( [], [] )
                world.objects
    in
        ( World { world | objects = objects }, messages )


isZeroVector : Vector -> Bool
isZeroVector v =
    v.x == 0 && v.y == 0


clearDestroyed : World m a msg -> World m a msg
clearDestroyed (World world) =
    World
        { world
            | objects =
                (List.foldr
                    (\(GameObject obj) objects ->
                        if obj.destroy then
                            objects
                        else
                            (GameObject obj) :: objects
                    )
                    []
                    world.objects
                )
        }


resetPhysics : World m a msg -> World m a msg
resetPhysics (World world) =
    let
        updatedObjects =
            List.map
                (\(GameObject obj) ->
                    GameObject
                        { obj | forces = [] }
                )
                world.objects
    in
        World { world | objects = updatedObjects }


updatePhysics : Time -> World m a msg -> World m a msg
updatePhysics diff (World world) =
    let
        updatedObjects =
            List.map (\obj -> (updateObjectPhysics diff obj)) world.objects
    in
        World { world | objects = updatedObjects }


updateObjectPhysics : Time -> GameObject m a msg -> GameObject m a msg
updateObjectPhysics diff (GameObject object) =
    let
        sumForces =
            List.foldr vectorAdd (Vector 0 0) object.forces

        acc =
            (vectorDivScalar object.mass sumForces)

        vel =
            vectorAdd (vectorMultScalar diff (aveVec object.acc acc)) object.vel

        pos =
            vectorAdd (vectorMultScalar diff (aveVec object.vel vel)) object.pos
    in
        GameObject
            { object
                | acc = acc
                , vel = vel
                , pos = pos
                , needsRebounding = pos /= object.pos
            }


aveVec : Vector -> Vector -> Vector
aveVec v1 v2 =
    vectorAdd v1 v2
        |> vectorDivScalar 2


makeBounded : GameObject m a msg -> Bounded (GameObject m a msg)
makeBounded (GameObject obj) =
    { pos = obj.pos
    , shape = obj.shape
    , object = GameObject obj
    , boundingBox = shapeToBoundingBox obj.pos obj.shape
    }


maybeUpdateBoundingBox : GameObject m a msg -> GameObject m a msg
maybeUpdateBoundingBox (GameObject obj) =
    if obj.needsRebounding then
        GameObject
            { obj
                | boundingBox = shapeToBoundingBox obj.pos obj.shape
                , needsRebounding = False
            }
    else
        GameObject obj


updateCollisions : Time -> World m a msg -> ( World m a msg, List (GameMessage m a msg) )
updateCollisions diff (World world) =
    case world.objects of
        [] ->
            ( World world, [] )

        _ ->
            let
                collidables =
                    List.foldr
                        (\(GameObject obj) objects ->
                            case obj.collisionHandler of
                                NoCollision ->
                                    objects

                                CollisionHandler _ ->
                                    (GameObject obj) :: objects
                        )
                        []
                        world.objects
            in
                case (List.map maybeUpdateBoundingBox collidables) of
                    [] ->
                        ( World world, [] )

                    x :: xs ->
                        let
                            ( updatedObjects, msgs ) =
                                checkCollisions diff
                                    (World world)
                                    []
                                    []
                                    x
                                    xs
                        in
                            ( World { world | objects = updatedObjects }
                            , msgs
                            )


checkCollisions :
    Time
    -> World m a msg
    -> List (GameMessage m a msg)
    -> List (GameObject m a msg)
    -> GameObject m a msg
    -> List (GameObject m a msg)
    -> ( List (GameObject m a msg), List (GameMessage m a msg) )
checkCollisions diff world msgs checked current others =
    case others of
        [] ->
            ( current :: checked, msgs )

        _ ->
            let
                ( updatedCurrent, updatedOthers, updatedMsgs ) =
                    (List.foldr
                        (\(GameObject oth) ( GameObject cur, uOthers, uMsgs ) ->
                            let
                                noCollision =
                                    ( GameObject cur
                                    , (GameObject oth) :: uOthers
                                    , uMsgs
                                    )
                            in
                                case ( cur.collisionHandler, oth.collisionHandler ) of
                                    ( NoCollision, _ ) ->
                                        noCollision

                                    ( _, NoCollision ) ->
                                        noCollision

                                    ( CollisionHandler cHandler, CollisionHandler oHandler ) ->
                                        if
                                            (cHandler.canCollideWith oth.self
                                                || oHandler.canCollideWith cur.self
                                            )
                                        then
                                            if areColliding (GameObject cur) (GameObject oth) then
                                                -- later should check further, ok
                                                let
                                                    ( uCurrent, msgsCurrent ) =
                                                        cHandler.collisionReaction
                                                            world
                                                            (GameObject cur)
                                                            (GameObject oth)

                                                    ( uOther, msgsOther ) =
                                                        oHandler.collisionReaction
                                                            world
                                                            (GameObject oth)
                                                            (GameObject cur)
                                                in
                                                    ( uCurrent
                                                    , uOther :: uOthers
                                                    , msgsCurrent ++ msgsOther ++ uMsgs
                                                    )
                                            else
                                                noCollision
                                        else
                                            noCollision
                        )
                        ( current, [], [] )
                        others
                    )
            in
                case updatedOthers of
                    [] ->
                        ( updatedCurrent :: checked, updatedMsgs )

                    --should never happen
                    oth :: oths ->
                        checkCollisions
                            diff
                            world
                            (msgs ++ updatedMsgs)
                            (updatedCurrent :: checked)
                            oth
                            oths


areColliding : GameObject m a msg -> GameObject m a msg -> Bool
areColliding (GameObject o1) (GameObject o2) =
    --colliding (makeBounded (GameObject o1)) (makeBounded (GameObject
    --o2))
    case ( o1.shape, o2.shape ) of
        ( Point, Point ) ->
            False

        ( Point, Circle r ) ->
            (o1.pos.x - o2.pos.x) ^ 2 + (o1.pos.y - o2.pos.x) ^ 2 < r ^ 2

        ( Point, Polygon points ) ->
            o1.pos.x
                > o2.boundingBox.xSpan.low
                && o1.pos.x
                < o2.boundingBox.xSpan.high
                && o1.pos.y
                > o2.boundingBox.ySpan.low
                && o1.pos.y
                < o2.boundingBox.ySpan.high

        ( Circle _, Point ) ->
            areColliding (GameObject o2) (GameObject o1)

        ( Circle r1, Circle r2 ) ->
            (o1.pos.x - o2.pos.x)
                ^ 2
                + (o1.pos.y - o2.pos.x)
                ^ 2
                < (r1 + r2)
                ^ 2

        ( Circle r1, Polygon points ) ->
            False

        ( Polygon points1, Polygon points2 ) ->
            boundingBoxCollision o1.boundingBox o2.boundingBox

        ( Polygon _, Point ) ->
            areColliding (GameObject o2) (GameObject o1)

        ( Polygon _, Circle _ ) ->
            areColliding (GameObject o2) (GameObject o1)



--    if abs (o1.pos.x - o2.pos.x) < 2
--        && abs (o1.pos.y - o2.pos.y) < 2 then True else False
{- if (boundingBoxCollision
           cur.boundingBox
           oth.boundingBox) then
       -- later should check further, ok
       let (uCurrent, msgsCurrent) =
               cHandler.collisionReaction
                   world
                   (GameObject cur)
                   (GameObject oth)
           (uOther, msgsOther) =
               oHandler.collisionReaction
                   world
                   (GameObject oth)
                   (GameObject cur)
       in
           (uCurrent
           , uOther :: uOthers
           , msgsCurrent ++ msgsOther ++ uMsgs)
   else
       noCollision
-}
