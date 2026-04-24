module Collision
    exposing
        ( BoundingBox
        , Bounded
        , boundingBoxCollision
        , shapeToBoundingBox
        , colliding
        , spanOverlap
        )

import Shape exposing (..)
import Vector exposing (..)


--import Debug exposing (..)


type alias Span =
    { low : Float
    , high : Float
    }


type alias BoundingBox =
    { xSpan : Span
    , ySpan : Span
    }


type alias Bounded a =
    { object : a
    , boundingBox : BoundingBox
    , pos : Vector
    , shape : Shape
    }


spanOverlap : Span -> Span -> Float
spanOverlap s1 s2 =
    max 0.0 ((min s1.high s2.high) - (max s1.low s2.low))


spanCollision : Span -> Span -> Bool
spanCollision s1 s2 =
    if s1.low < s2.low then
        s2.low < s1.high
    else
        s1.low < s2.high


xSpan : Vector -> Shape -> Span
xSpan pos shape =
    case shape of
        Point ->
            { low = pos.x, high = pos.x }

        Circle radius ->
            { low = pos.x - radius, high = pos.x + radius }

        Polygon points ->
            case points of
                [] ->
                    { low = pos.x, high = pos.x }

                firstP :: _ ->
                    List.foldr
                        (\p span ->
                            { low = min (p.x + pos.x) span.low
                            , high = max (p.x + pos.x) span.high
                            }
                        )
                        { low = firstP.x, high = firstP.x }
                        points


boundingBoxCollision : BoundingBox -> BoundingBox -> Bool
boundingBoxCollision box1 box2 =
    (spanCollision box1.xSpan box2.xSpan)
        && (spanCollision box1.ySpan box2.ySpan)


shapeToBoundingBox : Vector -> Shape -> BoundingBox
shapeToBoundingBox pos shape =
    case shape of
        Point ->
            { xSpan = { low = pos.x, high = pos.x }
            , ySpan = { low = pos.y, high = pos.y }
            }

        Circle radius ->
            { xSpan = { low = pos.x - radius, high = pos.x + radius }
            , ySpan = { low = pos.y - radius, high = pos.y + radius }
            }

        Polygon points ->
            case points of
                [] ->
                    { xSpan = { low = pos.x, high = pos.x }
                    , ySpan = { low = pos.y, high = pos.y }
                    }

                p :: _ ->
                    let
                        bounds =
                            List.foldr
                                (\point bounding ->
                                    { xSpan =
                                        { low = min bounding.xSpan.low point.x
                                        , high = max bounding.xSpan.high point.x
                                        }
                                    , ySpan =
                                        { low = min bounding.ySpan.low point.y
                                        , high = max bounding.ySpan.high point.y
                                        }
                                    }
                                )
                                { xSpan = { low = p.x, high = p.x }
                                , ySpan = { low = p.y, high = p.y }
                                }
                                points
                    in
                        { xSpan =
                            { low = pos.x + bounds.xSpan.low
                            , high = pos.x + bounds.xSpan.high
                            }
                        , ySpan =
                            { low = pos.y + bounds.ySpan.low
                            , high = pos.y + bounds.ySpan.high
                            }
                        }


segmentAngle : Vector -> Vector -> Float
segmentAngle p1 p2 =
    atan2 (p2.y - p1.y) (p2.x - p1.x)


colliding : Bounded a -> Bounded a -> Bool
colliding b1 b2 =
    if boundingBoxCollision b1.boundingBox b2.boundingBox then
        True
    else
        False
