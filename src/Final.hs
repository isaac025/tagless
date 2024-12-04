{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Final where

import Data.Text.Lazy (Text)
import Text.Parsec (
    char,
    digit,
    many1,
    parse,
    spaces,
    string,
    try,
    (<|>),
 )
import Text.Parsec.Text.Lazy (Parser)
import Prelude hiding (and, or)

class IntArith expr where
    int :: Int -> expr
    add :: expr -> expr -> expr
    sub :: expr -> expr -> expr
    mult :: expr -> expr -> expr

instance IntArith Int where
    int n = n
    add n1 n2 = n1 + n2
    sub n1 n2 = n1 - n2
    mult n1 n2 = n1 * n2

class Logic expr where
    bool :: Bool -> expr
    and :: expr -> expr -> expr
    or :: expr -> expr -> expr

instance Logic Bool where
    bool b = b
    and b1 b2 = b1 && b2
    or b1 b2 = b1 || b2

-- a boolean is a string true or false
parseBool :: (Logic expr) => Parser expr
parseBool = bool <$> ((True <$ string "true") <|> (False <$ string "false"))

parseAnd :: (Logic expr) => Parser expr
parseAnd = do
    b <- parseBool
    f <- and <$ (spaces *> string "/\\" <* spaces)
    f b <$> parseBool

parseOr :: (Logic expr) => Parser expr
parseOr = do
    b <- try parseAnd <|> parseBool
    f <- or <$ (spaces *> string "\\/" <* spaces)
    f b <$> (try parseAnd <|> parseBool)

parseLogic :: (Logic expr) => Parser expr
parseLogic = try parseOr <|> parseAnd

-- an integer is simple, one or more digits
parseInt :: (IntArith expr) => Parser expr
parseInt = int . read <$> many1 digit

-- an arithmetic operator has higher precedence for
-- multiplication, and it is the char *
parseMult :: (IntArith expr) => Parser expr
parseMult = do
    n <- parseInt
    f <- mult <$ (spaces *> char '*' <* spaces)
    f n <$> parseInt

-- otherwise parse + or -
parseAdd :: (IntArith expr) => Parser expr
parseAdd = do
    m <- try parseMult <|> parseInt
    f <- add <$ (spaces *> char '+' <* spaces)
    f m <$> (try parseMult <|> parseInt)

parseSub :: (IntArith expr) => Parser expr
parseSub = do
    m <- try parseMult <|> parseInt
    f <- sub <$ (spaces *> char '-' <* spaces)
    f m <$> (try parseMult <|> parseInt)

parseArith :: (IntArith expr) => Parser expr
parseArith = try parseAdd <|> try parseSub <|> parseMult

arithParser :: (IntArith expr) => Text -> Either String expr
arithParser input = case parse parseArith "arith-final" input of
    Left err -> Left $ show err
    Right ast -> Right ast

logicParser :: (Logic expr) => Text -> Either String expr
logicParser input = case parse parseLogic "bool-final" input of
    Left err -> Left $ show err
    Right ast -> Right ast
