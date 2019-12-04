module Parse where
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                        [] -> []
                        (x:xs) -> [(x, xs)])

                        
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                            [] -> []
                            [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- <|> :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item 
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x: xs)

-- "itermediate" parsers.
ident :: Parser String
ident = do x <- lower -- at least one lower case character
           xs <- many alphanum
           return (x: xs)

nat :: Parser Int
nat = do x <- some digit
         return (read x)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

-- Parsers that handle spaces
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Actual useful parsers
identifier :: Parser String -- foo bar p ab2C1d; first lower
identifier = token ident

natural :: Parser Int
natural = token nat

integer:: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs) -- match the xs symbol

opIdentifier :: Parser String
opIdentifier = do s <- symbol "=="
                  return s
                  <|> do s <- symbol "~="
                         return s
                  <|> do s <- symbol ">"
                         return s
                  <|> do s <- symbol ">="
                         return s
                  <|> do s <- symbol "<"
                         return s
                  <|> do s <- symbol "<="
                         return s
            

