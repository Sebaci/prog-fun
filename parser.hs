-- Sebastian Cielemęcki

module Main where

import Control.Monad
import Data.Char

-- | The main entry point.
main :: IO ()
main = do
	putStr "> "
	lispExpr <- getLine
	print $ parseExpr lispExpr
	main

newtype Parser a = Parser (String -> [(a, String)])
parse (Parser p) = p

instance Monad Parser where
    return a = Parser (\s -> [(a, s)])
    p >>= f = Parser (\s -> concat [parse (f a) s' | (a, s') <- parse p s] )

-- choice operator
instance MonadPlus Parser where
    mzero = Parser (\s -> [])
    mplus p q = Parser (\s -> parse p s ++ parse q s)

-- deterministic choice operator
bmplus :: Parser a -> Parser a -> Parser a
bmplus p q = Parser (\s -> case parse (p `mplus` q) s of
    [] -> []
    (c:s) -> [c])

-- eat one char
item :: Parser Char
item = Parser item' where
    item' [] = []
    item' (c:s) = [(c, s)]

-- conditional eating using predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c
    else mzero

isChr :: Char -> Parser Char
isChr c = sat (c ==)

isSym :: Char -> Bool
isSym c = c `elem` "!#$%&|*+-/:<=>?@^_~"

-- many applications
many :: Parser a -> Parser [a]
many p = many1 p `bmplus` return []

many1 :: Parser a -> Parser [a]
many1 p = do
    a <- p
    s <- many p
    return (a:s)

-- many applications with separator
sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = (p `sepby1` sep) `bmplus` return [] where
    sepby1 p sep = do
        a <- p
        s <- many (do 
            sep; p)
        return (a:s)


-- now, grammar and lexical combinators --
data Expr = Atom String
            | Number Int
            | Str String
            | List [Expr] -- lisp list is represented as a list of expressions that it contains
    deriving Show

space :: Parser String
space = many (sat isSpace)

qt :: Parser Char
qt = isChr '"'

lpar :: Parser Char
lpar = isChr '('

rpar :: Parser Char
rpar = isChr ')'

digit :: Parser Char
digit = sat isDigit

sym :: Parser Char
sym = sat (\n -> isLetter n || isSym n)

number :: Parser Int
number = do
    ds <- many1 digit
    
    return (read ds :: Int)

-- sequence of letters and digits (possibly Atom or Str)
sq :: Parser String
sq = do
    sq <- many1 sym
    return sq

-- Num recognision
numTerm :: Parser Expr
numTerm = do
    n <- number
    return (Number n)

-- Atom recognision
atomTerm :: Parser Expr
atomTerm = do
    a <- sq
    return (Atom a)

-- Str recognision
strTerm :: Parser Expr
strTerm = do
    qt
    s <- sq
    qt
    return (Str s)

-- List recognision
listTerm :: Parser Expr
listTerm = do
    lpar
    space
    cs <- sepby expr space
    space
    rpar
    return (List cs)

-- Expression recognision
expr :: Parser Expr
expr = numTerm `mplus` atomTerm `mplus` strTerm `mplus` listTerm

parseExpr :: String -> Expr
parseExpr = fst . head . parse expr
