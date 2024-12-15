module Main where

import PressXToBoard (Board, Dir, Pos, Move)
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, many)

data Block
  = Robot
  | Box
  | Wall
  | Empty
  deriving (Show, Eq)


moveP :: Parser Dir
moveP = anyOf 
  [ Board.U <$ char '^'
  , Board.D <$ char 'v'
  , Board.L <$ char '<'
  , Board.R <$ char '>'
  ]

blockP :: Parser Block
blockP = anyOf
  [ Robot <$ char '@'
  , Box <$ char 'O'
  , Wall <$ char '#'
  , Empty <$ char '.'
  ]

puzzleP :: Parser (Board Block, [Dir])
puzzleP = do
  sections <- blocks
  case sections of
    [boardTxt, movesTxt] -> return (board, moves)
      where
        cells = mustParse (many blockP) <$> boardTxt
        moves = mustParse (many moveP) (concat movesTxt)
        board = Board.fromLists cells
    _ -> error $ "error parsing puzzle, expected 2 sections but got " ++ show (length sections)

solve1 :: Solver
solve1 input = show (board, moves)
  where
    (board, moves) = mustParse puzzleP input


solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
