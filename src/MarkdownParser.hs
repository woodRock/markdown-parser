{-# LANGUAGE TupleSections #-}

module MarkdownParser where

import Data.Char
import Control.Applicative
import Data.Maybe

data Value
  = MString String
  | Newline Char
  | MNumber Int
  | Bold String
  | Italics String
  | Codeblock (String, String)
  | Blockquote String
  | Inlinecode String
  | Header (Int, String)
  | List [Value]
  | OrderedList [(Int, Value)]
  | File [Value]
  | Horozontalrule String
  deriving (Show,Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input',x)<- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

match :: Char -> Parser Char
match x = Parser f
  where
    f [] = Nothing
    f y
      | head y == x = Just (y, x)
      | otherwise = Nothing

charP :: Char -> Parser Char
charP x = Parser f
  where
    f [] = Nothing
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
      in Just (rest, token)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input',xs)

number :: Parser Int
number = f <$> notNull (spanP isDigit)
  where f = read

stringLiteral :: Parser String
stringLiteral = spanP (/='\n')

surround :: String -> Parser String
surround s  = stringP s *> spanP (/= head s) <* stringP s

newline :: Parser Value
newline = Newline <$> match '\n'

bold :: Parser Value
bold = Bold <$> surround "**"

italics :: Parser Value
italics = Italics <$> surround "_"

inlineCode :: Parser Value
inlineCode = Inlinecode <$> surround "`"

mstring :: Parser Value
mstring = MString <$> options
  where
    options = notNull stringLiteral

blockQuote :: Parser Value
blockQuote = Blockquote <$> (charP '>' *> charP ' ' *> stringLiteral)

codeBlock :: Parser Value
codeBlock =  Codeblock <$> (stringP "```" *> code <* stringP "```")
  where
  code = syntax <|> noSyntax
  syntax = (\key value -> (key,value)) <$> stringLiteral <* charP '\n' <*> spanP (/= '`')
  noSyntax = ("",) <$> (charP '\n' *> spanP (/= '`'))

header :: Parser Value
header =  Header <$> pair
  where
  pair = (\key value -> (length key, value)) <$> headers <* charP ' ' <*> stringLiteral
  headers =   stringP "######" <|> stringP "#####" <|> stringP "####" <|> stringP "###" <|> stringP "##" <|> stringP "#"

horizontalRule :: Parser Value
horizontalRule = Horozontalrule <$> (stringP "---" <|> stringP "___" <|> stringP "***")

list :: Parser Value
list = List <$> (listChar *> charP ' ' *> elements)
  where
  elements = sepBy (charP '\n' *> listChar <* charP ' ') mstring
  listChar = charP '-' <|> charP '+' <|> charP '*'

orderedList :: Parser Value
orderedList = OrderedList <$> (match '1' *> sepBy (charP '\n') pair)
  where
    pair =
      (\key value -> (key, value)) <$> number <* charP '.' <* ws <*> mstring

value :: Parser Value
value = newline
  <|> header
  <|> orderedList
  <|> codeBlock
  <|> list
  <|> blockQuote
  <|> horizontalRule
  <|> italics
  <|> bold
  <|> inlineCode
  <|> mstring

file :: Parser Value
file = File <$> lines
  where
    lines = sepBy (charP '\n') value

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)
