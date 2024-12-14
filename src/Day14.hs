module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

xy :: Parser (Pair Int)
xy = do
  x <- int
  char ','
  y <- int
  return (x ,y)

robot :: Parser (Pair (Pair Int))
robot = do
  p <- string "p=" *> xy
  space
  v <- string "v=" *> xy
  return (p, v)

extrapolate :: Int -> Pair (Pair Int) -> Pair Int
extrapolate n ((x, y), (dx, dy)) = (n*dx+x, n*dy+y)

clip :: Int -> Int -> Pair Int -> Pair Int
clip nrow ncol (x, y) = (posMod x ncol, posMod y nrow)
  where
    posMod x b = ((x `mod` b) + b) `mod` b

quadrants :: Int -> Int -> [Pair Int] -> Int
quadrants nrow ncol points =  a * b * c * d
  where
    x' = ncol `div` 2
    y' = nrow `div` 2
    clipped = clip nrow ncol <$> points
    (a, b, c, d) = foldr update (0, 0, 0, 0) clipped
    update p@(x, y) (a, b, c, d)
      | (x < x') && (y < y') = (a+1, b, c, d)
      | (x > x') && (y < y') = (a, b+1, c, d)
      | (x < x') && (y > y') = (a, b, c+1, d)
      | (x > x') && (y > y') = (a, b, c, d+1)
      | otherwise = (a, b, c, d)

plot :: Int -> Int -> Int -> [Pair Int] -> String
plot nrow ncol i points = show i ++ ":\n" ++ render ++ "\n"
  where
    render = unlines [[symbol (x, y) | x <- [0..ncol-1]] | y <- [0..nrow-1]]
    symbol pos | pos `elem` points = 'x'
               | otherwise = '.'

solve1 :: Solver
solve1 = show 
       . quadrants nrow ncol
       . map (extrapolate nsteps) 
       . mustParse (linesOf robot)
  where
    nrow = 103
    ncol = 101
    nsteps = 100

solve2 :: Solver
solve2 = unlines
       . zipWith (plot nrow ncol) [1..nsteps]
       . history
       . mustParse (linesOf robot)
  where
    nrow = 103
    ncol = 101
    nsteps = 10000
    history robots = [map (clip nrow ncol . extrapolate n) robots | n <- [1..nsteps]]

main :: IO ()
main = runCLI solve1 solve2
