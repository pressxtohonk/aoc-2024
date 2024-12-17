module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, eof, optional, sepBy, string)

data Operand
  = Lit Int
  | A
  | B
  | C
  | Reserved
  deriving (Show, Eq)

data Instruction
  = Adv Operand
  | Bdv Operand
  | Cdv Operand
  | Bxl Operand
  | Bst Operand
  | Jnz Operand
  | Bxc
  | Out Operand
  deriving (Show, Eq)

data Computer = Computer {p :: Int, a :: Int, b :: Int, c :: Int} deriving (Show)

type Program = [Instruction]

literal :: Parser Operand
literal = Lit <$> int

combo :: Parser Operand
combo =
  anyOf
    [ Lit 0 <$ char '0',
      Lit 1 <$ char '1',
      Lit 2 <$ char '2',
      Lit 3 <$ char '3',
      A <$ char '4',
      B <$ char '5',
      C <$ char '6',
      Reserved <$ char '7'
    ]

opcode :: Parser Instruction
opcode =
  anyOf
    [ string "0," >> Adv <$> combo,
      string "1," >> Bxl <$> literal,
      string "2," >> Bst <$> literal,
      string "3," >> Jnz <$> combo,
      string "4," >> Bxc <$ literal,
      string "5," >> Out <$> combo,
      string "6," >> Bdv <$> combo,
      string "7," >> Cdv <$> combo
    ]

computer :: Parser Computer
computer = do
  regA <- string "Register A: " *> int <* eol
  regB <- string "Register B: " *> int <* eol
  regC <- string "Register C: " *> int <* eol
  return $ Computer 0 regA regB regC

program :: Parser Program
program = string "Program: " *> opcode `sepBy` char ',' <* optional eol

debugger :: Parser (Computer, Program)
debugger = do
  c <- computer <* eol
  p <- program <* optional eol
  eof
  return (c, p)

solve1 :: Solver
solve1 = show . mustParse debugger

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
