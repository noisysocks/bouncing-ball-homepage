module Geometry exposing (..)


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Rectangle =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }


clamp : comparable -> comparable -> comparable -> comparable
clamp min max k =
    if k < min then
        min
    else if k > max then
        max
    else
        k


vadd : Vector -> Vector -> Vector
vadd a b =
    Vector (a.x + b.x) (a.y + b.y)


vsubtract : Vector -> Vector -> Vector
vsubtract a b =
    Vector (a.x - b.x) (a.y - b.y)


vscale : Float -> Vector -> Vector
vscale k { x, y } =
    Vector (x * k) (y * k)


vlength : Vector -> Float
vlength { x, y } =
    sqrt (x * x + y * y)


vclamp : Rectangle -> Vector -> Vector
vclamp bounds { x, y } =
    Vector (clamp bounds.left bounds.right x) (clamp bounds.top bounds.bottom y)
