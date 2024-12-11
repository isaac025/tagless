module Initial (initialParser) where

import Control.Monad (void)
import Data.Text.Lazy (Text, pack)
import Text.Parsec (
    char,
    choice,
    letter,
    many1,
    oneOf,
    parse,
    string,
    try,
    (<|>),
 )
import Text.Parsec.Text.Lazy (Parser)

data Expr
    = NameE Text
    | MkPetE Text Text
    deriving (Show)

-- Parser helpers
whitespace :: Parser ()
whitespace = choice [simpleWhitespace *> whitespace, pure ()]
  where
    simpleWhitespace = void $ many1 (oneOf " \t\n")

identifier :: Parser Text
identifier = pack <$> petLexeme (many1 (letter <|> char '_'))

petLexeme :: Parser p -> Parser p
petLexeme p = p <* whitespace

nameK :: Parser ()
nameK = void $ petLexeme (string "name")

actionK :: Parser Text
actionK = pack <$> petLexeme (string "sleep" <|> string "eat" <|> string "bathe")

-- a pet is created by wrinting:
-- new <PET_TYPE> <NAME>
parseNew :: Parser Expr
parseNew = do
    nameK
    NameE <$> identifier

-- make the pet do something by:
-- <PET_NAME> (sleep | eat | sleep)
parseMk :: Parser Expr
parseMk = do
    pn <- identifier
    MkPetE pn <$> actionK

parseExpr :: Parser Expr
parseExpr = try parseNew <|> try parseMk

initialParser :: Text -> Either String Expr
initialParser input =
    case parse parseExpr "initial" input of
        Left err -> Left $ show err
        Right ast -> Right ast
