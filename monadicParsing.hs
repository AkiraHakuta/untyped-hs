module MonadicParsing where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad (guard)
import Data.Char

-- Monadic Parsing
type Parser a  =  StateT String Maybe a
runParser = runStateT


anyChar :: Parser Char
anyChar = StateT $ \s -> case s of
    []     -> empty
    (c:cs) -> pure (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    c <- anyChar
    guard $ pred c
    pure c

char :: Char -> Parser Char
char = satisfy . (==)

notChar :: Char -> Parser Char
notChar x = satisfy  (/= x)

string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where 
        rest a = (do
            f <- op
            b <- p
            rest (f a b)) <|> pure a

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainr1` op) <|> pure a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
    where
        scan   = p >>= rest
        rest a = (do
            f <- op
            b <- scan
            rest (f a b)) <|> pure a

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

junk :: Parser String
junk = do sepBy space (lineComment <|> blockComment)
          return []

space :: Parser String
space = many (satisfy isSpace)

token :: Parser a -> Parser a
token p = do junk
             v <- p
             junk 
             return v

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: Parser String
identifier = do junk
                x  <- (lower <|> upper)
                xs <- many (alphanum <|> char '\'')
                junk
                return (x:xs)


-- Commenting
lineComment :: Parser String
lineComment = 
    do string "--"
       many (notChar '\n')
       char '\n'
       return []

blockComment :: Parser String
blockComment = 
    do string "{-"
       seachMinusCloseBrace1

seachMinusCloseBrace1 :: Parser String
seachMinusCloseBrace1 =
    do c1 <- anyChar
       if c1 /= '-' then seachMinusCloseBrace1
            else do c2 <- anyChar
                    if c2 == '}' then return []
                         else if c2 =='-' then  seachMinusCloseBrace2
                             else seachMinusCloseBrace1

seachMinusCloseBrace2 :: Parser String
seachMinusCloseBrace2 =   
    do c <- anyChar
       if c == '}' then return []
            else if c == '-' then seachMinusCloseBrace2
                else seachMinusCloseBrace1



            
{-
main :: IO()
main = do    
    print $ runParser anyChar ("abc")
    print $ runParser (char 'a') ("abc")
    print $ runParser (char 'a') ("bcd")
    print $ runParser (symbol "\\") ("  \\x.x")
    print $ runParser identifier ("{- block comment -}  aA1'-- line comment\n:= \\x.x")
-}