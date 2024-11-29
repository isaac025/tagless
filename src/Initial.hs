module Initial where

import Data.Text.Lazy (Text, pack)
import Text.Parsec (
    ParseError,
    alphaNum,
    char,
    many1,
    parse,
    spaces,
    (<|>),
 )
import Text.Parsec.Text.Lazy (Parser)

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

substitute :: Text -> Expr -> Expr -> Expr
substitute x rep expr =
    case expr of
        Var v
            | v == x -> rep
            | otherwise -> Var v
        Lam v body
            | v == x -> Lam v body
            | v `elem` freeVars rep ->
                let v' = freshVar v body rep
                 in Lam v' (substitute x rep (substitute v (Var v') body))
        App e1 e2 -> App (substitute x rep e1) (substitute x rep e2)

freeVars :: Expr -> [Text]
freeVars expr =
    case expr of
        Var v -> [v]
        Lam v body -> filter (/= v) (freeVars body)
        App e1 e2 -> freeVars e1 ++ freeVars e2

freshVar :: Text -> Expr -> Expr -> Text
freshVar v e1 e2 = head [v' | v' <- map (\i -> v <> pack (show i)) [1 :: Int ..], v' `notElem` (freeVars e1 ++ freeVars e2)]

betaReduce :: Expr -> Maybe Expr
betaReduce expr =
    case expr of
        App (Lam x body) arg -> Just (substitute x arg body)
        App e1 e2 ->
            case betaReduce e1 of
                Just e1' -> Just (App e1' e2)
                Nothing -> App e1 <$> betaReduce e2
        Lam x body -> Lam x <$> betaReduce body
        _ -> Nothing
