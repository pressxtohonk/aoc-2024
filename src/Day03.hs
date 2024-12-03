module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Control.Monad (void)

mul :: Parser (Pair Int)
mul = do
  string "mul("
  a <- int
  string ","
  b <- int
  string ")"
  return (a, b)

doP :: Parser String
doP = string "do()"

dontP :: Parser String
dontP = string "don't()"

-- mustParse enabled "mul(1,2)" = [(1,2)]
enabled :: Parser [Pair Int]
enabled = do
  pair <- mul
  return [pair]

-- mustParse disabled "don't()...do()"  = []
-- mustParse disabled "don't()...<EOF>" = []
disabled :: Parser [Pair Int]
disabled = do
  dontP >> (anyChar `manyTill` anyOf [void doP, eof])
  return []

-- mustParse gap "...mul(a,b)" = "..."
-- mustParse gap "...<EOF>"    = "..."
gap :: Parser String
gap = anyChar `manyTill` anyOf
  [ void (lookAhead mul)
  , eof
  ]

-- mustParse gap' "...mul(a,b)" = "..."
-- mustParse gap' "...don't()"  = "..."
-- mustParse gap' "...<EOF>"    = "..."
gap' :: Parser String
gap' = anyChar `manyTill` anyOf
  [ void (lookAhead enabled)
  , void (lookAhead disabled)
  , eof
  ]

prog :: Parser [Pair Int]
prog = gap >> (mul `sepEndBy` gap)

prog' :: Parser [Pair Int]
prog' = do
  pairs <- gap' >> (anyOf [enabled, disabled] `sepEndBy` gap')
  return $ concat pairs

eval :: [Pair Int] -> Int
eval = sum . map (uncurry (*))

solve1 :: Solver
solve1 = show . eval . mustParse prog

solve2 :: Solver
solve2 = show . eval . mustParse prog'

main :: IO ()
main = runCLI solve1 solve2
