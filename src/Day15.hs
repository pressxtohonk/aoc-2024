module Main where

import qualified Data.Map as Map
import PressXToBoard (Board, Dir, Pos, Move, (!), (?))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, many)
import Data.Maybe (fromMaybe)

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

push :: Move -> Board Block -> Maybe (Board Block)
push move@(pos, _) board = case board ? pos of
  Just Wall -> Nothing
  Just Empty -> Just board
  Just Robot -> update Robot <$> push move' board
  Just Box -> update Box <$> push move' board
  result -> error $ "push got unexpected grid result " ++ show result
  where
    move'@(pos', _) = Board.step move
    update block board = Board.fill (Board.fill board pos Empty) pos' block

pushRobot :: Dir -> Board Block -> Board Block
pushRobot dir board = fromMaybe board (push (pos, dir) board)
  where
    pos = head . Map.keys $ Map.filter (==Robot) (Board.filled board)

gpsCoordSum :: Board Block -> Int
gpsCoordSum = Map.foldrWithKey combine 0 . Board.filled
  where
    combine (r, c) Box acc = 100 * r + c + acc
    combine _ _ acc = acc

solve1 :: Solver
solve1 input = show $ gpsCoordSum board'
  where
    (board, moves) = mustParse puzzleP input
    board' = foldr pushRobot board (reverse moves)

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
