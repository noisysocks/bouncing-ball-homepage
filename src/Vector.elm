module Vector exposing (Vector, Rectangle, add, subtract, scale, length, clamp)


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


add : Vector -> Vector -> Vector
add a b =
    Vector (a.x + b.x) (a.y + b.y)


subtract : Vector -> Vector -> Vector
subtract a b =
    Vector (a.x - b.x) (a.y - b.y)


scale : Float -> Vector -> Vector
scale k { x, y } =
    Vector (x * k) (y * k)


length : Vector -> Float
length { x, y } =
    sqrt (x * x + y * y)


clamp : Rectangle -> Vector -> Vector
clamp bounds { x, y } =
    Vector (Basics.clamp bounds.left bounds.right x) (Basics.clamp bounds.top bounds.bottom y)
