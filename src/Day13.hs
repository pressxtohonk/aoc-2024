module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Data.Maybe (mapMaybe)

data Game = Game {a :: XY Int, b :: XY Int, prize :: XY Int}
  deriving (Show)

newtype XY a = XY (Pair a)
  deriving (Show, Eq)

instance Ord a => Ord (XY a) where
  compare (XY (dx1, dy1)) (XY (dx2, dy2))
    | (dx1 > dx2) || (dy1 > dy2) = GT
    | (dx1 < dx2) && (dy1 < dy2) = LT
    | otherwise = EQ

instance Num a => Semigroup (XY a) where
  (XY (dx1, dy1)) <> (XY (dx2, dy2)) = XY (dx1+dx2, dy1+dy2)

instance Num a => Monoid (XY a) where
  mempty = XY (0, 0)

instance Functor XY where 
  fmap f (XY (x, y)) = XY (f x, f y)

dxdy :: String -> Parser (XY Int)
dxdy label = do
  string label
  string ": X" >> oneOf "+="
  dx <- int
  string ", Y" >> oneOf "+="
  dy <- int
  eol
  return $ XY (dx, dy)

clawMachine :: Parser Game
clawMachine = do
  a <- dxdy "Button A"
  b <- dxdy "Button B"
  p <- dxdy "Prize"
  return $ Game a b p

tryMin :: Maybe Int -> Maybe Int -> Maybe Int
tryMin Nothing a = a
tryMin a Nothing = a
tryMin a b = min <$> a <*> b

minToWin :: Game -> Maybe Int
minToWin (Game a b prize) = go 100 0 Nothing
  where
    go :: Int -> Int -> Maybe Int -> Maybe Int
    go na nb acc
      | na < 0 || nb > 100 = acc
      | value == prize = go (na-1) (nb+1) (tryMin acc cost)
      | value >= prize = go (na-1) nb acc
      | value <= prize = go na (nb+1) acc
      where
        a' = (na *) <$> a
        b' = (nb *) <$> b
        value = a' <> b'
        cost = Just (3 * na + nb)

type Mat = Pair Vec
type Vec = Pair Int

mmul :: Mat -> Vec -> Vec
mmul m@((m11, m12), (m21, m22)) a@(a1, a2) =
  (m11 * a1 + m12 * a2, m21 * a1 + m22 * a2)

inv2x2 :: Mat -> Mat
inv2x2 ((a, b), (c, d)) = ((d, -b), (-c, a))

invDenom :: Mat -> Int
invDenom ((a, b), (c, d)) = a * d - b * c

-- try solve : x | M.x = a , int x
intSoln :: Mat -> Vec -> Maybe Vec
intSoln m@((m11, m12), (m21, m22)) a@(a1, a2)
  | isIntSoln = Just (na, nb)
  | otherwise = Nothing
  where
    -- invert the matrix
    m' = inv2x2 m -- inverse without determinant scaling
    d = invDenom m -- determinant scaling factor
    -- solve for x.D
    (na', nb') = mmul m' a
    -- check integer constraints
    (na, ra) = divMod na' d
    (nb, rb) = divMod nb' d
    isIntSoln = (ra == 0) && (rb == 0)

-- lin alg hack, assumes systems of eqns have exactly 1 soln
minToWin' :: Game -> Maybe Int
minToWin' (Game (XY (a, c)) (XY (b, d)) (XY (x, y))) = do
  let big = 10000000000000
  (na, nb) <- intSoln ((a, b), (c, d)) (big+x, big+y)
  return (3 * na + nb)

solve1 :: Solver
solve1 = show . sum . mapMaybe minToWin . mustParse (clawMachine `sepEndBy` eol)

solve2 :: Solver
solve2 = show . sum . mapMaybe minToWin' . mustParse (clawMachine `sepEndBy` eol)

main :: IO ()
main = runCLI solve1 solve2
