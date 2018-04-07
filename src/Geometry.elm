module Geometry exposing (..)


type alias Vector =
    ( Float, Float )


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
vadd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vscale : Float -> Vector -> Vector
vscale k ( x, y ) =
    ( x * k, y * k )


vlength : Vector -> Float
vlength ( x, y ) =
    sqrt (x * x + y * y)


vclamp : Rectangle -> Vector -> Vector
vclamp bounds ( x, y ) =
    ( clamp bounds.left bounds.right x, clamp bounds.top bounds.bottom y )
