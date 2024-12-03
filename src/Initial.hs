module Initial where

import Data.Text.Lazy (Text)
import Text.Parsec (
    ParseError,
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

data BinOp = Add | Sub | Mult | And | Or
    deriving (Show)

data Expr
    = IntE Int
    | BoolE Bool
    | BinE BinOp Expr Expr
    deriving (Show)

-- an integer is simple, one or more digits
parseInt :: Parser Expr
parseInt = IntE . read <$> many1 digit

-- A boolean is either the string "true" or "false"
parseBool :: Parser Expr
parseBool = BoolE <$> (spaces *> (True <$ string "true") <|> (False <$ string "false"))

term :: Parser Expr
term = parseBool <|> parseInt

-- A binary operator is one of the following strings:
-- +, -, *, /\, \/
parseBin :: Parser BinOp
parseBin =
    (Add <$ char '+')
        <|> (Sub <$ char '-')
        <|> (Mult <$ char '*')
        <|> (And <$ string "/\\")
        <|> (Or <$ string "\\/")

-- A binary operator is an expr BINOP expr
parseBinOp :: Parser Expr
parseBinOp = do
    e1 <- spaces *> term
    bin <- spaces *> parseBin
    BinE bin e1 <$> (spaces *> term)

parseExpr :: Parser Expr
parseExpr = try parseBinOp <|> term

parser :: Text -> Either ParseError Expr
parser = parse parseExpr "initial"
