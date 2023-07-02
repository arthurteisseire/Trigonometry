module Vector2 exposing (..)


type alias Vector2 number =
    { x : number
    , y : number
    }


identity : Vector2 number
identity =
    { x = 0, y = 0 }


eq : Vector2 number -> Vector2 number -> Bool
eq left right =
    left.x == right.x && left.y == right.y


isNull : Vector2 number -> Bool
isNull vector =
    eq vector identity


add : Vector2 number -> Vector2 number -> Vector2 number
add =
    apply (+)


sub : Vector2 number -> Vector2 number -> Vector2 number
sub =
    apply (-)


divFloat : Vector2 Float -> Vector2 Float -> Vector2 Float
divFloat =
    apply (/)


divInt : Vector2 Int -> Vector2 Int -> Vector2 Int
divInt =
    apply (//)


mul : Vector2 number -> Vector2 number -> Vector2 number
mul =
    apply (*)


approxEq : Vector2 Float -> Vector2 Float -> Float -> Bool
approxEq left right epsilon =
    length (sub left right) < epsilon


length : Vector2 Float -> Float
length v =
    sqrt ((v.x * v.x) + (v.y * v.y))


apply : (number -> number -> number) -> Vector2 number -> Vector2 number -> Vector2 number
apply func left right =
    { x = func left.x right.x
    , y = func left.y right.y
    }


vectorFloatToString : Vector2 Float -> String
vectorFloatToString =
    toString String.fromFloat


vectorIntToString : Vector2 Int -> String
vectorIntToString =
    toString String.fromInt


toString : (number -> String) -> Vector2 number -> String
toString func vector =
    "V2(x=" ++ func vector.x ++ ", y=" ++ func vector.y ++ ")"
