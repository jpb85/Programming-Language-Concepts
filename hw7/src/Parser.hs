module Parser (
    Parser,
    parse,
    (+++),
    (<|>),
    eof,
    anyChar,
    satisfy,
    oneOf,
    notOneOf,
    char,
    digit,
    lower,
    upper,
    letter,
    alphaNum,
    string,
    many,
    many1,
    space,
    parens
  ) where

import Control.Applicative (Alternative, empty, (<|>),
                            Applicative, pure, (<*>))
import Control.Monad
import Data.Char (isDigit, isLower, isUpper, isAlpha, isAlphaNum, isSpace)

newtype Parser a = P { runP :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance Monad Parser where
    return x = P $ \inp -> Just (x, inp)
    m >>= f  = P $ \inp -> case runP m inp of
                             Nothing        -> Nothing
                             Just (x, inp') -> runP (f x) inp'
    m >>  n  = P $ \inp -> case runP m inp of
                             Nothing        -> Nothing
                             Just (_, inp') -> runP n inp'
    fail _   = P $ \_ -> Nothing

instance MonadPlus Parser where
    mzero       = fail "mzero"
    m `mplus` n = P $ \inp -> case runP m inp of
                                Nothing -> runP n inp
                                Just x  -> Just x

parse :: Parser a -> String -> Maybe a
parse p s = case runP p s of
              Nothing    -> Nothing
              Just (x,_) -> Just x

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

eof :: Parser ()
eof = P $ \inp -> case inp of
                    [] -> Just ((), [])
                    _  -> Nothing

anyChar :: Parser Char
anyChar = P $ \inp -> case inp of
                        []     -> Nothing
                        (c:cs) -> Just (c,cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do x <- anyChar
               if p x then return x else fail "satisfy"

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

notOneOf :: [Char] -> Parser Char
notOneOf cs = satisfy (`notElem` cs)

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

string :: String -> Parser String
string []     =  return []
string (c:cs) =  do _ <- char c
                    _ <- string cs
                    return (c:cs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p =  do x  <- p
              xs <- many p
              return (x:xs)

space :: Parser ()
space = do _ <- many (satisfy isSpace)
           return ()

parens :: Parser a -> Parser a
parens p = do _ <- char '('
              x <- p
              _ <- char ')'
              return x
