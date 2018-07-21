module Vector exposing (..)


type alias Vector =
    { x : Float
    , y : Float
    }


type alias AngleMag =
    { angle : Float
    , mag : Float
    }


type alias RotationMatrix =
    { xAxis : Vector
    , yAxis : Vector
    }


vectorMultScalar : Float -> Vector -> Vector
vectorMultScalar scalar v =
    Vector (v.x * scalar) (v.y * scalar)


vectorDivScalar : Float -> Vector -> Vector
vectorDivScalar scalar v =
    Vector (v.x / scalar) (v.y / scalar)


vectorAdd : Vector -> Vector -> Vector
vectorAdd v1 v2 =
    Vector (v1.x + v2.x) (v1.y + v2.y)


vectorSub : Vector -> Vector -> Vector
vectorSub v1 v2 =
    Vector (v1.x - v2.x) (v1.y - v2.y)


angleMagToVector : AngleMag -> Vector
angleMagToVector { angle, mag } =
    Vector ((cos angle) * mag) ((sin angle) * mag)


rotatePoint : Float -> Vector -> Vector
rotatePoint angle point =
    let
        cosAngle =
            cos (angle)

        sinAngle =
            sin (angle)
    in
        Vector (point.x * cosAngle - point.y * sinAngle)
            (point.x * sinAngle + point.y * cosAngle)


rotationMatrix : Float -> RotationMatrix
rotationMatrix angle =
    let
        cosAngle =
            cos (angle)

        sinAngle =
            sin (angle)
    in
        RotationMatrix
            (Vector cosAngle sinAngle)
            (Vector (-1 * sinAngle) cosAngle)


rotatePointWithMatrix : RotationMatrix -> Vector -> Vector
rotatePointWithMatrix rmatrix point =
    vectorAdd (vectorMultScalar point.x rmatrix.xAxis)
        (vectorMultScalar point.y rmatrix.yAxis)


rotatePointsWithMatrix : RotationMatrix -> List Vector -> List Vector
rotatePointsWithMatrix rmatrix =
    List.map (rotatePointWithMatrix rmatrix)


rotatePoints : Float -> List Vector -> List Vector
rotatePoints angle =
    rotatePointsWithMatrix (rotationMatrix angle)


within2pi : Float -> Float
within2pi x =
    let
        w =
            x - (2 * pi * (toFloat (floor (x / (2.0 * pi)))))
    in
        if w >= 0 then
            w
        else
            -1 * w


distance : Vector -> Vector -> Float
distance v1 v2 =
    ((v1.x - v2.x) ^ 2 + (v1.y - v2.y) ^ 2) ^ 0.5


aimDir : Vector -> Vector -> Float
aimDir v1 v2 =
    within2pi (-1 * atan2 (v1.x - v2.x) (v1.y - v2.y) - pi)



-- returns shortest angle to get from one to another


angleDiff : Float -> Float -> Float
angleDiff a1 a2 =
    let
        adiff =
            a1 - a2
    in
        if adiff > pi then
            within2pi (2 * pi - adiff)
        else
            adiff
