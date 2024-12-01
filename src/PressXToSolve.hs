module PressXToSolve where

import System.Environment (getArgs)

type Solver = String -> String

runTest :: String -> String -> Solver -> ()
runTest xs ys solve =
  case solve xs of
    ys' | ys' == ys -> ()
    ys' -> error $ unlines [ "expected: " ++ ys
                           , "got     : " ++ ys' ]


runCLI :: Solver -> Solver -> IO ()
runCLI solve1 solve2 = do
  args <- getArgs
  case args of
    ["1", filename] -> readFile filename >>= putStrLn . solve1
    ["2", filename] -> readFile filename >>= putStrLn . solve2
    _ -> print "usage: ./XXX [1|2] [filename]"
