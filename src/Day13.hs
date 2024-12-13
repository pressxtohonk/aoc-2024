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

solve1 :: Solver
solve1 = show . sum . mapMaybe minToWin . mustParse (clawMachine `sepEndBy` eol)

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
