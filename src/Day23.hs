module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List (intercalate, isPrefixOf)
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import qualified Data.Set as Set
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (alphaNum, char, eof, manyTill)

connP :: Parser (String, String)
connP = (,) <$> alphaNum `manyTill` char '-' <*> alphaNum `manyTill` eol

connMap :: (Ord a) => [(a, a)] -> Map.Map a (Set.Set a)
connMap = foldl' update Map.empty
  where
    update :: (Ord a) => Map.Map a (Set.Set a) -> (a, a) -> Map.Map a (Set.Set a)
    update acc (u, v) = updateU . updateV $ acc
      where
        updateU = Map.insertWith Set.union u (Set.singleton v)
        updateV = Map.insertWith Set.union v (Set.singleton u)

findTrios :: (Ord a) => [(a, a)] -> [(a, a, a)]
findTrios pairs = foldl' (connectWith $ connMap pairs) [] pairs
  where
    connectWith conns acc (u, v) = case Set.intersection <$> conns !? u <*> conns !? v of
      Just xs -> [(u, v, x) | x <- Set.toList xs, x /= u, x /= v] ++ acc
      _ -> acc

hasAny :: (a -> Bool) -> (a, a, a) -> Bool
hasAny p (a, b, c) = any p [a, b, c]

-- Given an edge list of a graph, find all fully connected components (FCCs)
-- estimation fail for: [(10, 20), (10, 30), (20, 30), (10, 1), (20, 2), (30, 3)]
-- [{1, 10}, {2, 20}, {3, 30}] will be estimated instead of [{10, 20, 30}]
-- this occurs as nodes {1, 2, 3} are greedily added before {10, 20, 30}
-- In practice, may be solvable by growing multiple times with shuffled node orders
estimateLargestFCC :: (Ord a) => [(a, a)] -> Set.Set a
estimateLargestFCC pairs = maxBy Set.size (grow <$> nodes)
  where
    -- Create an adjacency list and a list of nodes
    peers = connMap pairs
    nodes = Map.keys peers
    -- Given a starting node, greedily add nodes to form a FCC containing that node
    grow seed = foldl' addTo (Set.singleton seed) nodes
      where
        addTo component node
          | component `Set.isSubsetOf` (peers ! node) = Set.insert node component
          | otherwise = component

maxBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
maxBy f = snd . maximum . map (\s -> (f s, s))

passwordFromParty :: Set.Set String -> String
passwordFromParty = intercalate "," . Set.elems

solve1 :: Solver
solve1 =
  show
    . (`div` 3) -- hack to account for symmetries of a 3-tuple
    . length
    . filter (hasAny ("t" `isPrefixOf`))
    . findTrios
    . mustParse (connP `manyTill` eof)

solve2 :: Solver
solve2 =
  passwordFromParty
    . estimateLargestFCC
    . mustParse (connP `manyTill` eof)

main :: IO ()
main = runCLI solve1 solve2
