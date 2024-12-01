import PressXToParse
import PressXToSolve

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

input :: Parser (Pair [Int])
input = unzip <$> linesOf (pair int)

difference :: Pair [Int] -> Int
difference (xs, ys) = sum [abs (x-y) | (x, y) <- zip (sort xs) (sort ys)]

similarity :: Pair [Int] -> Int
similarity (xs, ys) = f 0 (sort xs) (sort ys)
  where
    f acc [] _ = acc
    f acc _ [] = acc
    f acc (x:xs) (y:ys)
      | x < y     = f acc xs (y:ys)
      | x > y     = f acc (x:xs) ys
      | otherwise = f (acc + x * length x' * length y') xs' ys'
        where
          (x', xs') = span (==x) (x:xs)
          (y', ys') = span (==y) (y:ys)

-- alternate map based solution
similarity' :: Pair [Int] -> Int
similarity' (xs, ys) = foldl update 0 xs
  where
    counts = Map.fromListWith (+) (map (, 1) ys)
    get x = Map.findWithDefault 0 x counts
    update acc x = acc + x * get x

main :: IO ()
main = runCLI solve1 solve2

solve1 :: Solver
solve1 = show . difference . mustParse input

solve2 :: Solver
solve2 = show . similarity . mustParse input

