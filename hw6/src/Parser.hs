module Parser (
    Parser,
    parse,

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

    chainl,
    chainl1,
    chainr,
    chainr1,

    space,
    parens,
    ident,
    nat,

    token,
    keyword,
    symbol,
    identifier,
    natural,
    integer
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
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p =  do x  <- p
              xs <- many p
              return (x:xs)

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@, and returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are zero occurrences of @p@, the value @x@ is returned.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

-- | @chainl1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@, and returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. This parser can, for example, be used to eliminate left recursion which
-- typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do x <- p
                  rest x
  where
    rest x =  do f <- op
                 y <- p
                 rest (f x y)
          <|> return x

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@, and returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are no occurrences of @p@, the value @x@ is returned.
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> return x

-- | @chainr1 p op x@ parses /one/ or more occurrences of |p|,
-- separated by @op@, and returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by @p@.
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op =
    scan
  where
    scan = do x <- p
              rest x

    rest x =  do f <- op
                 y <- scan
                 return (f x y)
          <|> return x

space :: Parser ()
space = do _ <- many (satisfy isSpace)
           return ()

parens :: Parser a -> Parser a
parens p = do _ <- char '('
              x <- p
              _ <- char ')'
              return x

ident :: Parser String
ident = do x <- lower
           xs <- many alphaNum
           return (x:xs)

nat :: Parser Integer
nat = do xs <- many1 digit
         return (read xs)

int :: Parser Integer
int =  do _ <- char '-'
          n <- nat
          return (-n)
   <|> nat

-- The function below ignore spacing

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

keyword :: String -> Parser ()
keyword xs = do _ <- token (string xs)
                return ()

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

integer :: Parser Integer
integer = token int
