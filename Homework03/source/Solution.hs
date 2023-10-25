{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-identities #-}
module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where
import Control.Concurrent (yield)


unique :: Eq a => [a] -> Bool
unique [] = True
unique l
    | (head l) `elem` (tail l) = False
    | otherwise = unique (tail l)



pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(b, a, c) | c<-[1..100], a<-[1..100], b<-[1..a],  a^2 + b^2 == c^2]


primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(b, a, c) | c<-[1..100], a<-[1..100], b<-[1..a],  a^2 + b^2 == c^2 && b<a && gcd a b ==1]



perfectNumbers :: Integral a => [a]
perfectNumbers = [x | x<-[1..8128], x==sum [ d | d<-[1..x-1], x `mod` d==0]]


cantorPairs :: Integral a => [(a, a)]
cantorPairs = gen1 0 0
    where gen1 x y = if x == 0 then (0, y): gen1 (y + 1) 0 else (x, y): gen1 (x - 1) (y + 1)


distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
ans l = minimum [distance (head l) (p)| p<- tail l]

minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = min (ans l) (minimalDistance (tail l))


