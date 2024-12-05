module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type Rule = Pair Int
type Update = [Int]

rule :: Parser Rule
rule = (,) <$> int <* char '|' <*> int

update :: Parser Update
update = int `sepBy1` char ','

inputs :: Parser ([Rule], [Update])
inputs = do
  blks <- blocks
  case blks of
    [rulesIn, updatesIn] ->
      let 
        rules = mustParse (linesOf rule) (unlines rulesIn)
        updates = mustParse (linesOf update) (unlines updatesIn)
      in
        return (rules, updates)
    xs -> error $ "expected 2 sections in input, got " ++ show (length xs)

-- O(mn) to walk over input, O(mnlgn)

summarize :: [Rule] -> Map Int (Set Int)
summarize rules = 
  let errors = Map.fromListWith (++) [(k, [v]) | (k, v) <- rules]
  in Map.map Set.fromList errors

follows :: Map Int (Set Int) -> Update -> Bool
follows errors = f Set.empty
  where
    f :: Set Int -> Update -> Bool
    f _ [] = True
    f seen (curr:rest) = 
      let
        okay = case Map.lookup curr errors of
          Nothing   -> True
          Just errs -> Set.disjoint seen errs
        seen' = Set.insert curr seen
      in
        okay && f seen' rest

correct :: Map Int (Set Int) -> Update -> Update
correct errors = f Set.empty []
  where
    f :: Set Int -> Update -> Update -> Update
    f _ acc [] = reverse acc
    f seen acc (curr:rest) = 
      let
        seen' = Set.insert curr seen
        acc' = case Map.lookup curr errors of
          Just errs | not (Set.disjoint seen errs) -> 
            reverse . insertBefore errs curr . reverse $ acc
          _ -> curr:acc
      in
        f seen' acc' rest

insertBefore :: Set Int -> Int -> Update -> Update
insertBefore set val xs =
  let (heads, tail) = List.partition (`Set.notMember` set) xs
  in heads ++ val:tail

mid :: [a] -> a
mid xs = slowfast xs xs
  where
    slowfast _ [] = error "list has even number of elements"
    slowfast (m:_) [_] = m
    slowfast (_:slow) (_:_:fast) = slowfast slow fast

solve1 :: Solver
solve1 input = show $ sum [ mid update | update <- updates, isOkay update ]
  where
    (rules, updates) = mustParse inputs input
    isOkay = follows (summarize rules)

solve2 :: Solver
solve2 input = show $ sum [ mid (fix xs) | xs <- updates, not (isOkay xs) ]
  where
    (rules, updates) = mustParse inputs input
    errors = summarize rules
    isOkay = follows errors
    fix = correct errors

main :: IO ()
main = runCLI solve1 solve2
