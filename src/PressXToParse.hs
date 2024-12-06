module PressXToParse where

import Control.Monad (void)
import Text.Parsec

type Pair a = (a, a)
type Trio a = (a, a, a)

type Parser = Parsec String ()

mustParse :: Parser a -> String -> a
mustParse parser input = case parse parser "" input of
  Left err -> error (show err)
  Right x -> x

-- Token parsers
int :: Parser Int
int = do
  sign <- option "" (string "-")
  nums <- many1 digit
  return $ read (sign ++ nums)

eol :: Parser ()
eol = void endOfLine

end :: Parser ()
end = eol <|> eof

-- Schema parsers
line :: Parser String
line = nonEmpty $ anyChar `manyTill` lookAhead end

block :: Parser [String]
block = linesOf line

blocks :: Parser [[String]]
blocks = linesOf block

-- coordinate combinators
coord :: Parser (Pair Int)
coord = do
  pos <- getPosition
  let r = sourceLine pos
  let c = sourceColumn pos
  return (r, c)

coordOf :: Parser a -> Parser (Pair Int)
coordOf item = do
  skipTill item
  (r, c) <- coord
  return (r, c-1)

rangeOf :: Parser a -> Parser (Pair (Pair Int))
rangeOf item = do
  skipTill (lookAhead item)
  lb <- coord
  ub <- coordOf item
  return (lb, ub)

-- custom combinators
nonEmpty :: Foldable f => Parser (f a) -> Parser (f a)
nonEmpty parser = do
  token <- parser
  if null token
    then fail "expected non-empty parse result"
    else return token

skipTill :: Parser a -> Parser a
skipTill item = do
  anyChar `manyTill` try (lookAhead item)
  item

linesOf :: Parser a -> Parser [a]
linesOf item = item `endBy1` end

anyOf :: [Parser a] -> Parser a
anyOf = choice . map try

pair :: Parser a -> Parser (Pair a)
pair item = do
  a <- item
  spaces
  b <- item
  return (a, b)

trio :: Parser a -> Parser (Trio a)
trio item = do
  a <- item
  spaces
  b <- item
  spaces
  c <- item
  return (a, b, c)

