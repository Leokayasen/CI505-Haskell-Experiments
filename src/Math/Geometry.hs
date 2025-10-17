module Math.Geometry (
    areaCircle,
    perimeterCircle
) where

areaCircle :: Floating a => a -> a
areaCircle r = pi * r * r

perimeterCircle :: Floating a => a -> a
perimeterCircle r = 2 * pi * r