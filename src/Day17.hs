module Main where

import Control.Monad.Writer
import Data.Bits
import Data.List (intercalate)
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, eof, lookAhead, many, optional, sepBy, string, try)

data Operand
  = Lit Int
  | A
  | B
  | C
  | Reserved
  deriving (Show, Eq)

data Instruction
  = Adv Operand
  | Bxl Operand
  | Bst Operand
  | Jnz Operand
  | Bxc
  | Out Operand
  | Bdv Operand
  | Cdv Operand
  deriving (Show, Eq)

data Computer = Computer {p :: Int, a :: Int, b :: Int, c :: Int} deriving (Show)

type Program = [Instruction]

literal :: Parser Operand
literal = Lit <$> int

combo :: Parser Operand
combo =
  anyOf
    [ char '0' >> return (Lit 0),
      char '1' >> return (Lit 1),
      char '2' >> return (Lit 2),
      char '3' >> return (Lit 3),
      char '4' >> return A,
      char '5' >> return B,
      char '6' >> return C,
      char '7' >> return Reserved
    ]

opcode :: Parser Instruction
opcode =
  anyOf
    [ string "0," >> Adv <$> lookAhead combo,
      string "1," >> Bxl <$> lookAhead literal,
      string "2," >> Bst <$> lookAhead combo,
      string "3," >> Jnz <$> lookAhead literal,
      string "4," >> Bxc <$ lookAhead literal,
      string "5," >> Out <$> lookAhead combo,
      string "6," >> Bdv <$> lookAhead combo,
      string "7," >> Cdv <$> lookAhead combo
    ]

computer :: Parser Computer
computer = do
  regA <- string "Register A: " *> int <* eol
  regB <- string "Register B: " *> int <* eol
  regC <- string "Register C: " *> int <* eol
  return $ Computer 0 regA regB regC

program :: Parser Program
program = string "Program: " *> many (try opcode) <* int <* optional eol

debugger :: Parser (Computer, Program)
debugger = do
  c <- computer <* eol
  p <- program <* optional eol
  eof
  return (c, p)

type OutputW = Writer [Int]

val :: Operand -> Computer -> Int
val (Lit x) = const x
val A = a
val B = b
val C = c
val Reserved = error "cannot get value for reserved operand"

incr :: Computer -> Computer
incr comp = comp {p = p comp + 2}

adv :: Operand -> Computer -> OutputW Computer
adv op comp = return $ incr comp {a = a comp `div` (0b1 `shift` val op comp)}

bxl :: Operand -> Computer -> OutputW Computer
bxl op comp = return $ incr comp {b = b comp `xor` val op comp}

bst :: Operand -> Computer -> OutputW Computer
bst op comp = return $ incr comp {b = val op comp .&. 0b111}

jnz :: Operand -> Computer -> OutputW Computer
jnz op comp
  | a comp == 0 = return $ incr comp
  | otherwise = return $ comp {p = val op comp}

bxc :: Computer -> OutputW Computer
bxc comp = return $ incr comp {b = b comp `xor` c comp}

out :: Operand -> Computer -> OutputW Computer
out op comp = writer (incr comp, [val op comp .&. 0b111])

bdv :: Operand -> Computer -> OutputW Computer
bdv op comp = return $ incr comp {b = a comp `div` (0b1 `shift` val op comp)}

cdv :: Operand -> Computer -> OutputW Computer
cdv op comp = return $ incr comp {c = a comp `div` (0b1 `shift` val op comp)}

run :: Program -> Computer -> OutputW Computer
run prog comp
  | p comp >= length prog = return comp
  | otherwise = update comp >>= run prog
  where
    update = case prog !! p comp of
      Adv op -> adv op
      Bxl op -> bxl op
      Bst op -> bst op
      Jnz op -> jnz op
      Bxc -> bxc
      Out op -> out op
      Bdv op -> bdv op
      Cdv op -> cdv op

solve1 :: Solver
solve1 input = unlines ["", show comp, show prog, output]
  where
    (comp, prog) = mustParse debugger input
    output = intercalate "," . map show $ execWriter (run prog comp)

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
