module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Control.Monad (void)

data Cmd
  = Mul Int Int
  | Do
  | Dont
  deriving (Show, Eq)

cmdMul :: Parser Cmd
cmdMul = do
  string "mul("
  a <- int
  string ","
  b <- int
  string ")"
  return $ Mul a b

cmdDo :: Parser Cmd
cmdDo = Do <$ string "do()"

cmdDont :: Parser Cmd
cmdDont = Dont <$ string "don't()"

cmd :: Parser Cmd
cmd = skipTill $ anyOf [cmdMul, cmdDo, cmdDont]

prog :: Parser [Cmd]
prog = many $ try cmd

eval :: [Cmd] -> Int
eval = enabled 0 -- start in enabled state with result=0
  where
    enabled acc [] = acc
    enabled acc (x:xs) = case x of
      Mul a b -> enabled (a*b+acc) xs
      Dont    -> disabled acc xs
      _       -> enabled acc xs
    disabled acc [] = acc
    disabled acc (x:xs) = case x of
      Do -> enabled acc xs
      _  -> disabled acc xs

solve1 :: Solver
solve1 = show . eval . mustParse prog

solve2 :: Solver
solve2 = show . eval . mustParse prog

main :: IO ()
main = runCLI solve1 solve2
