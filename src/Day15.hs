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

push :: Move -> Board Block -> Maybe (Board Block)
push move@(pos, dir) board = case board ? pos of
  Just Wall -> Nothing
  Just Empty -> Just board
  Just Robot -> update move Robot <$> push move' board
  Just Box -> update move Box <$> push move' board
  Just BoxL 
    | dir == Board.U -> 
        update move BoxL . update (stepR move) BoxR
        <$> (push move' board >>= push (Board.step $ stepR move))
    | dir == Board.D -> 
        update move BoxL . update (stepL move) BoxR
        <$> (push move' board >>= push (Board.step $ stepL move))
    | otherwise -> update move BoxL <$> push move' board
  Just BoxR
    | dir == Board.U -> 
        update move BoxR . update (stepL move) BoxL
        <$> (push (Board.step move) board >>= push (Board.step $ stepL move))
    | dir == Board.D -> 
        update move BoxR . update (stepR move) BoxL
        <$> (push (Board.step move) board >>= push (Board.step $ stepR move))
    | otherwise -> update move BoxR <$> push move' board
  result -> error $ "push got unexpected grid result " ++ show result
  where
    move'@(pos', _) = Board.step move
    update move block board = Board.fill (Board.fill board pos Empty) pos' block
      where
        (pos, _) = move
        (pos', _) = Board.step move
    turnL = Board.turn . Board.turn . Board.turn
    turnR = Board.turn
    stepL = turnR . Board.step . turnL
    stepR = turnL . Board.step . turnR

pushRobot :: Dir -> Board Block -> Board Block
pushRobot dir board = fromMaybe board (push (pos, dir) board)
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
