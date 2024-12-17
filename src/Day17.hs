module Main where

import Control.Applicative (asum)
import Control.Monad.State
import Data.Bits
import Data.List (intercalate)
import Data.Maybe (fromJust)
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

opCodes :: Parser [Int]
opCodes = string "Program: " *> int `sepBy` char ','

debugger :: Parser (Computer, Program)
debugger = do
  c <- computer <* eol
  p <- program <* optional eol
  eof
  return (c, p)

type WithOutput = State [Int]

val :: Operand -> Computer -> Int
val (Lit x) = const x
val A = a
val B = b
val C = c
val Reserved = error "cannot get value for reserved operand"

incr :: Computer -> Computer
incr comp = comp {p = p comp + 2}

adv :: Operand -> Computer -> WithOutput Computer
adv op comp = return $ incr comp {a = a comp `div` (0b1 `shift` val op comp)}

bxl :: Operand -> Computer -> WithOutput Computer
bxl op comp = return $ incr comp {b = b comp `xor` val op comp}

bst :: Operand -> Computer -> WithOutput Computer
bst op comp = return $ incr comp {b = val op comp .&. 0b111}

jnz :: Operand -> Computer -> WithOutput Computer
jnz op comp
  | a comp == 0 = return $ incr comp
  | otherwise = return $ comp {p = val op comp}

bxc :: Computer -> WithOutput Computer
bxc comp = return $ incr comp {b = b comp `xor` c comp}

out :: Operand -> Computer -> WithOutput Computer
out op comp = do
  modify (val op comp .&. 0b111 :)
  return $ incr comp

bdv :: Operand -> Computer -> WithOutput Computer
bdv op comp = return $ incr comp {b = a comp `div` (0b1 `shift` val op comp)}

cdv :: Operand -> Computer -> WithOutput Computer
cdv op comp = return $ incr comp {c = a comp `div` (0b1 `shift` val op comp)}

runInstruction :: Instruction -> Computer -> WithOutput Computer
runInstruction ix = case ix of
  Adv op -> adv op
  Bxl op -> bxl op
  Bst op -> bst op
  Jnz op -> jnz op
  Bxc -> bxc
  Out op -> out op
  Bdv op -> bdv op
  Cdv op -> cdv op

run :: Program -> Computer -> WithOutput Computer
run prog comp
  | p comp < length prog = update comp >>= run prog
  | otherwise = return comp
  where
    update = runInstruction (prog !! p comp)

format :: [Int] -> String
format = intercalate "," . map show . reverse

findSeed :: (Int -> Int) -> [Int] -> Maybe Int
findSeed next output = asum (go output' 0 <$> [1..7])
  where
    output' = reverse output
    go :: [Int] -> Int -> Int -> Maybe Int
    go [] acc _ = Just acc
    go (x:xs) acc n
      | next acc' == x = asum (go xs acc' <$> [0..7])
      | otherwise = Nothing
      where
        acc' = 8 * acc + n

nextOutput :: Program -> Int -> Int
nextOutput prog a0 = head $ execState s []
  where
    comp = Computer 0 a0 0 0
    s = foldl (>>=) (return comp) (runInstruction <$> prog)

alternate :: [a] -> [a]
alternate (x:_:xs) = x : alternate xs
alternate xs = xs

solve1 :: Solver
solve1 input = format $ execState (run prog comp) []
  where
    (comp, prog) = mustParse debugger input

solve2 :: Solver
solve2 input = show $ fromJust (seed target)
  where
    (_, prog) = mustParse debugger input
    next = nextOutput (alternate prog)
    seed = findSeed next
    target = mustParse (block >> eol >> opCodes) input

main :: IO ()
main = runCLI solve1 solve2
