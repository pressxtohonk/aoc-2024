module PressXToGrids where

import qualified Data.List as List

type Grid a = [[a]]

-- Constructs a valid grid from a list of lists
fromLists :: [[a]] -> Grid a
fromLists [] = []
fromLists (h:t) | all ((length h ==) . length) t = h:t
                | otherwise = error "sublists have inconsistent width"

(!) :: Grid a -> (Int, Int) -> a
grid ! (r, c) = case grid ? (r, c) of
  Nothing -> error $ "grid index " ++ show (r, c) ++ " out of bounds"
  Just x -> x

(?) :: Grid a -> (Int, Int) -> Maybe a
grid ? (r, c)
  | (0 > r) || (r >= nrow grid) = Nothing
  | (0 > c) || (c >= ncol grid) = Nothing
  | otherwise = Just $ grid !! r !! c

take :: Int -> Int -> Grid a -> Grid a
take r c grid = List.take c <$> List.take r grid

empty :: Grid a
empty = []

-- Grid transformations
r1 :: Grid a -> Grid a
r1 = List.transpose . tx

r2 :: Grid a -> Grid a
r2 = tx . ty

r3 :: Grid a -> Grid a
r3 = List.transpose . ty

tx :: Grid a -> Grid a
tx = reverse

ty :: Grid a -> Grid a
ty = fmap reverse

nrow :: Grid a -> Int
nrow = length

ncol :: Grid a -> Int
ncol xs | nrow xs > 0 = length (head xs)
        | otherwise   = 0

rowHead :: Grid a -> Grid a
rowHead xs | nrow xs > 0 = [head xs]
           | otherwise   = error "no head for empty grid rows"

colHead :: Grid a -> Grid a
colHead xs | ncol xs > 0 = [ [head row] | row <- xs ]
           | otherwise   = error "no head for empty grid cols"

rowTail :: Grid a -> Grid a
rowTail xs | nrow xs > 0 = tail xs
           | otherwise   = error "no tail for empty grid rows"

colTail :: Grid a -> Grid a
colTail xs | ncol xs > 0 = [ tail row | row <- xs ]
           | otherwise   = error "no tail for empty grid cols"

-- list of all subgrids ending at the bottom right
tails :: Grid a -> [Grid a]
tails grid = rowTails grid >>= colTails

-- list of subgrids, each starting one row down
rowTails :: Grid a -> [Grid a]
rowTails xs | nrow xs > 0 = xs : rowTails (rowTail xs)
            | otherwise   = []

-- list of subgrids, each starting one col down
colTails :: Grid a -> [Grid a]
colTails xs | ncol xs > 0 = xs : colTails (colTail xs)
            | otherwise   = []

