module RegExp (
    RegExp(..),

    parseRegExp
  ) where

import Parser

data RegExp = Empty
            | Epsilon
            | Lit Char
            | Cat RegExp RegExp
            | Alt RegExp RegExp
            | Star RegExp
  deriving (Eq, Ord, Show, Read)

regExpP :: Parser RegExp
regExpP = altP <|> catP <|> return Epsilon
  where
    litP :: Parser RegExp
    litP = do c <- notOneOf "()*|"
              return (Lit c)

    atomP :: Parser RegExp
    atomP = parens regExpP <|> litP

    starP :: Parser RegExp
    starP = do r <- atomP
               _ <- char '*'
               return (Star r)

    noncatP :: Parser RegExp
    noncatP = starP <|> atomP

    catP :: Parser RegExp
    catP  =  do r <- noncatP
                s <- catP
                return (Cat r s)
         <|> noncatP

    altP :: Parser RegExp
    altP  =  do r <- catP
                _ <- char '|'
                s <- altP
                return (Alt r s)
         <|> catP

parseRegExp :: String -> Maybe RegExp
parseRegExp s = parse p s
  where
    p :: Parser RegExp
    p = do r <- regExpP
           eof
           return r
