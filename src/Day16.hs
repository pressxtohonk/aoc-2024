module Main where

import PressXToBoard (Board, (?))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, manyTill)

data Tile
  = Start
  | Floor
  | Wall
  | End
  deriving (Show, Eq)

tile :: Parser Tile
tile = anyOf
  [ Start <$ char 'S'
  , Floor <$ char '.'
  , Wall <$ char '#'
  , End <$ char 'E'
  ]

maze :: Parser (Board Tile)
maze = Board.fromLists <$> ((tile `manyTill` eol) `manyTill` eol)

solve1 :: Solver
solve1 = show . mustParse maze

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
