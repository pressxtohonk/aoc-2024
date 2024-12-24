module Main where

import Data.List (isPrefixOf)
import Data.Map ((!))
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (alphaNum, string, manyTill, space, many)

data Bit a
  = Lit a
  | And (Bit a) (Bit a)
  | Or (Bit a) (Bit a)
  | Xor (Bit a) (Bit a)
  deriving (Eq, Show)

instance Functor Bit where
  fmap f (Lit a) = Lit (f a)
  fmap f (And a b) = And (fmap f a) (fmap f b)
  fmap f (Or a b) = Or (fmap f a) (fmap f b)
  fmap f (Xor a b) = Xor (fmap f a) (fmap f b)

-- If we can build the tree, evaluating is easy
eval :: Bit Bool -> Bool
eval (Lit x) = x
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Xor a b) = eval a /= eval b

type Circuit = Map.Map String (Bit Bool)

valueP :: Parser (String, Bit Bool)
valueP = (,) <$> label <*> value <* end
  where
    label = alphaNum `manyTill` string ": "
    value = Lit . (==1) <$> int

gateP :: Parser (String, Bit String)
gateP = do
  a <- alphaNum `manyTill` space
  gate <- anyOf
    [ And <$ string "AND",
      Or <$ string "OR",
      Xor <$ string "XOR"
    ]
  space
  b <- alphaNum `manyTill` string " -> "
  c <- alphaNum `manyTill` end
  return (c, Lit a `gate` Lit b)

circuitP :: Parser (Map.Map String (Bit Bool), Map.Map String (Bit String))
circuitP = do
  values <- Map.fromList <$> many valueP
  eol
  gates <- Map.fromList <$> many gateP
  return (values, gates)

resolveWith :: Map.Map String (Bit Bool) -> Map.Map String (Bit String) -> String -> Bit Bool
resolveWith leaves branches = go
  where
    go :: String -> Bit Bool
    go label
      | Map.member label leaves = leaves ! label
      | Map.member label branches = case branches ! label of
          And (Lit a) (Lit b) -> And (go a) (go b)
          Or (Lit a) (Lit b) -> Or (go a) (go b)
          Xor (Lit a) (Lit b) -> Xor (go a) (go b)
          bit -> error $ "cannot expand " ++ show bit

toGraph :: (Map.Map String (Bit Bool), Map.Map String (Bit String)) -> Map.Map String (Bit Bool)
toGraph (leaves, branches) = Map.union leaves branches'
  where
    resolve = resolveWith leaves branches
    branches' = Map.mapWithKey (\k _ -> resolve k) branches

getOutput :: Map.Map String Bool -> Int
getOutput bits = sum [if bit then 2 ^ n else 0 | (n, bit) <- zip [0..]  output]
  where
    output = Map.elems $ Map.filterWithKey (\k _ -> "z" `isPrefixOf` k) bits

solve1 :: Solver
solve1 = show . getOutput . Map.map eval . toGraph . mustParse circuitP

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
