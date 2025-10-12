-- a module with general utility functions
module Util (insertIndex, mapFst, breakIndexDrop, tailSafe, breakIndexSpec, clampIndex, flipTuple, mapSnd) where

insertIndex :: Int -> a -> [a] -> [a]
insertIndex i a xs = take i xs ++ [a] ++ drop i xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

-- not inclusive to i
-- ex: breakIndex 1 [0, 1, 3] == ([0], [3])
breakIndexDrop :: Int -> [a] -> ([a], [a])
breakIndexDrop i xs = (take i xs, drop (i + 1) xs)

breakIndexSpec :: Int -> [a] -> ([a], a, [a])
breakIndexSpec i xs = (take i xs, xs !! i, drop (i + 1) xs)

-- tail [] = error
-- tailSafe [] = []
-- tailSafe [x] = []
-- tailSafe [x, y, z] = [y, z]
-- functionally identical to tail, exception being when input is [], returns []
tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_ : x) = x

clampIndex :: [a] -> Int -> Int
clampIndex xs b
  | b < 0 = 0
  | b > l = l
  | otherwise = b
 where
  l = length xs - 1
