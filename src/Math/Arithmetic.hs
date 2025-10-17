module Math.Arithmetic (
    safeDiv,
    custRound
) where

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

custRound :: (RealFrac a, Integral b) => a -> b
custRound = round