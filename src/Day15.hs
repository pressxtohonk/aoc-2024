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
  | BoxL
  | BoxR
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

blockP' :: Parser [Block]
blockP' = anyOf
  [ [Robot, Empty] <$ char '@'
  , [BoxL, BoxR] <$ char 'O'
  , [Wall, Wall] <$ char '#'
  , [Empty, Empty] <$ char '.'
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

puzzleP' :: Parser (Board Block, [Dir])
puzzleP' = do
  sections <- blocks
  case sections of
    [boardTxt, movesTxt] -> return (board, moves)
      where
        cells = concat . mustParse (many blockP') <$> boardTxt
        moves = mustParse (many moveP) (concat movesTxt)
        board = Board.fromLists cells
    _ -> error $ "error parsing puzzle, expected 2 sections but got " ++ show (length sections)

-- Tries to move the specified block /and/ all other affected blocks forward
-- Return Nothing if any wall is hit
nudge :: Move -> Board Block -> Maybe (Board Block)
nudge move@(pos, dir) board = case board ? pos of
  Just Wall -> Nothing
  Just Empty -> Just board
  Just Robot -> Just board >>= go move
  Just Box -> Just board >>= go move
  Just BoxL 
    | dir == Board.U -> Just board >>= go move >>= go (stepR move)
    | dir == Board.D -> Just board >>= go move >>= go (stepL move)
    | otherwise -> Just board >>= go move
  Just BoxR
    | dir == Board.U -> Just board >>= go move >>= go (stepL move)
    | dir == Board.D -> Just board >>= go move >>= go (stepR move)
    | otherwise -> Just board >>= go move
  result -> error $ "nudge got unexpected grid result " ++ show result
  where
    -- returns a function that shoves a cell forward if nudging the next cell succeeds
    go move = fmap (shove move) . nudge (Board.step move)
    turnL = Board.turn . Board.turn . Board.turn
    turnR = Board.turn
    stepL = turnR . Board.step . turnL
    stepR = turnL . Board.step . turnR

-- Moves the specified block one step forward and replaces it with `Empty`.
shove :: Move -> Board Block -> Board Block
shove move@(pos, _) board = board2
  where
    block = board ! pos
    (pos', _) = Board.step move
    board1 = Board.fill board pos Empty
    board2 = Board.fill board1 pos' block

pushRobot :: Dir -> Board Block -> Board Block
pushRobot dir board = fromMaybe board (nudge (pos, dir) board)
  where
    pos = head . Map.keys $ Map.filter (==Robot) (Board.filled board)

gpsCoordSum :: Board Block -> Int
gpsCoordSum = Map.foldrWithKey combine 0 . Board.filled
  where
    combine (r, c) Box acc = 100 * r + c + acc
    combine (r, c) BoxL acc = 100 * r + c + acc
    combine _ _ acc = acc

solve1 :: Solver
solve1 input = show $ gpsCoordSum board'
  where
    (board, moves) = mustParse puzzleP input
    board' = foldr pushRobot board (reverse moves)

solve2 :: Solver
solve2 input = show $ gpsCoordSum board'
  where
    (board, moves) = mustParse puzzleP' input
    board' = foldr pushRobot board (reverse moves)

main :: IO ()
main = runCLI solve1 solve2
