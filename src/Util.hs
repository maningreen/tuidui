-- a module with general utility functions
module Util (insertIndex, mapFst, breakIndexDrop, tailSafe, breakIndexSpec, clampIndex, flipTuple, mapSnd, dropIndex, applyIndex, initSafe, replace) where

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

breakIndexSpec :: Int -> [a] -> ([a], [a], [a])
breakIndexSpec i xs = (take i xs, [xs !! i], drop (i + 1) xs)

-- tail [] = error
-- tailSafe [] = []
-- tailSafe [x] = []
-- tailSafe [x, y, z] = [y, z]
-- functionally identical to tail, exception being when input is [], returns []
tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_ : x) = x

initSafe :: [a] -> [a]
initSafe [] = []
initSafe x = init x

clampIndex :: [a] -> Int -> Int
clampIndex [] _ = 0
clampIndex xs a
  | a < 0 = 0
  | a > l = l
  | otherwise = a
 where
  l = length xs - 1

dropIndex :: [a] -> Int -> [a]
dropIndex xs i = take i xs ++ drop (i + 1) xs

applyIndex :: (a -> a) -> [a] -> Int -> [a]
applyIndex _ [] _ = []
applyIndex f xs i = a ++ (map f b) ++ c
 where
  (a, b, c) = breakIndexSpec i xs

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace a b c = do
  item <- c
  if item == a 
  then b
  else return item
