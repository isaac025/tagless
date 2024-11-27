module Initial where

import Control.Monad (when)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.IO (getLine, putStr)
import System.Exit (exitSuccess)
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)
import Prelude hiding (getLine, putStr)

initialRepl :: IO ()
initialRepl = do
    putStr "> "
    l <- getLine
    when (l == "quit") exitSuccess
    print $ parser l
    initialRepl

identity :: Expr
identity = Lam "x" (Var "x")

self :: Expr
self = Lam "s" (App (Var "s") (Var "s"))

data Expr
    = Var Text
    | Lam Text Expr
    | App Expr Expr
    deriving (Show)

-- A var is a letter, example: "x"
parseVar :: Parser Expr
parseVar = Var . pack <$> many1 alphaNum

-- A Lambda is a letter followed by a body
parseLam :: Parser Expr
parseLam = do
    _ <- char '\\'
    boundVar <- pack <$> many1 alphaNum
    _ <- char '.'
    Lam boundVar <$> parseExpr

parseApp :: Parser Expr
parseApp = do
    _ <- char '('
    e1 <- spaces *> parseExpr
    e2 <- spaces *> parseExpr
    _ <- char ')'
    pure $ App e1 e2

parseExpr :: Parser Expr
parseExpr = parseApp <|> parseLam <|> parseVar

parser :: Text -> Either ParseError Expr
parser = parse parseExpr "initial"
