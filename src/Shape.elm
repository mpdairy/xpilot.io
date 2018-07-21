module Shape exposing (Shape(..))

import Vector exposing (..)


type Shape
    = Point
    | Circle Float --  radius
    | Polygon (List Vector) -- List of points around 0, 0 center.
