{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Final where

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

class PetLang expr where
    namePet :: Text -> expr
    petDo :: Text -> Text -> expr

-- Parser helpers
whitespace :: Parser ()
whitespace = choice [simpleWhitespace *> whitespace, pure ()]
  where
    simpleWhitespace = void $ many1 (oneOf " \t\n")

identifier :: Parser Text
identifier = pack <$> petLexeme (many1 (letter <|> char '_'))

petLexeme :: Parser p -> Parser p
petLexeme p = p <* whitespace

newK :: Parser ()
newK = void $ petLexeme (string "name")

actionK :: Parser Text
actionK = pack <$> petLexeme (string "sleep" <|> string "eat" <|> string "bathe")

-- a pet is created by wrinting:
-- new <PET_TYPE> <NAME>
parseName :: (PetLang expr) => Parser expr
parseName = do
    newK
    namePet <$> identifier

-- make the pet do something by:
-- <PET_NAME> (sleep | eat | sleep)
parseMk :: (PetLang expr) => Parser expr
parseMk = do
    pn <- identifier
    petDo pn <$> actionK

parseExpr :: (PetLang expr) => Parser expr
parseExpr = try parseName <|> try parseMk

finalParser :: (PetLang expr) => Text -> Either String expr
finalParser input =
    case parse parseExpr "final" input of
        Left err -> Left $ show err
        Right petst -> Right petst
