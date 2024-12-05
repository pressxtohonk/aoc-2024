module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Rule = Pair Int
type Update = [Int]

rule :: Parser Rule
rule = (,) <$> int <* char '|' <*> int

update :: Parser Update
update = int `sepBy1` char ','

inputs :: Parser ([Rule], [Update])
inputs = (,) <$> linesOf rule <* eol <*> linesOf update

follows :: [Rule] -> Update -> Bool
follows rules update = all adhered rules
  where
    idx = Map.fromList $ zip update [0..]
    adhered (lower, upper) = True `fromMaybe` isOrdered -- true if any lookup fails
      where isOrdered = (<) <$> Map.lookup lower idx
                            <*> Map.lookup upper idx

fixWith :: [Rule] -> Update -> Update
fixWith rules = addTo [] -- insert elems into a new list at correct positions
  where
    pageSuccessors = Map.fromListWith (++) [(lower, [upper]) | (lower, upper) <- rules]
    addTo fixed [] = fixed
    addTo fixed (page:rest) = addTo (combine page fixed) rest
      where
        combine = (:) `fromMaybe` do
          successors <- Map.lookup page pageSuccessors   -- O[log n] for tree lookup
          return $ insertBeforeFirst (`elem` successors) -- O[n] amortized (:

insertBeforeFirst :: (a -> Bool) -> a -> [a] -> [a]
insertBeforeFirst pred x xs =
  let (preds, succs) = List.partition (not . pred) xs
  in preds ++ x : succs

mid :: [a] -> a
mid xs = slowfast xs xs
  where
    slowfast _ [] = error "list has even number of elements"
    slowfast (m:_) [_] = m
    slowfast (_:slow) (_:_:fast) = slowfast slow fast

solve1 :: Solver
solve1 input = show $ sum [ mid u | u <- updates, follows rules u ]
  where
    (rules, updates) = mustParse inputs input

solve2 :: Solver
solve2 input = show $ sum [ mid (fixWith rules u) | u <- updates, not (follows rules u) ]
  where
    (rules, updates) = mustParse inputs input

main :: IO ()
main = runCLI solve1 solve2
