{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( pDecls
    , expr
    , Decl(..)
    , Expr(..)
    , Value(..)
    , BinOp(..)
    , Identifier
    , Type(..)
    , Parameter(..)
    , DoStatement(..)
    , fnType
    ) where

import           Control.Monad                  ( guard
                                                , void
                                                )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

type Identifier = Text

data Type = Raw Identifier | FnTy [Type] Type
    deriving (Show, Eq, Ord)

type ReturnType = Type

data Parameter = Parameter Identifier Type
    deriving (Show, Eq)

data Decl
    = Let Identifier Type Expr
    | Function Identifier [Parameter] ReturnType Expr
    | Extern Identifier [Parameter] ReturnType
    deriving (Show, Eq)

data Value
    = IInt Int
    | IBool Bool
    | IString Text
    | IFloat Float
    | IUnit
    | IFunc [Identifier] Expr
    | IExtern Identifier
    deriving (Show, Eq)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | LessThan
    | LeEqTo
    | GreaterThan
    | GrEqTo
    | EqualTo
    | And
    | Or
    deriving (Show, Eq, Ord)

data DoStatement
    = DoExpr Expr
    | DoLet Identifier Type Expr
    deriving (Show, Eq)

data Expr
    = Lambda [Parameter] ReturnType Expr
    | Val Value
    | StringLiteral Text
    | Operator BinOp Expr Expr
    | Identifier Text
    | Call Expr [Expr]
    | IntLit Int
    | FloatLit Float
    | BoolLit Bool
    | UnitLit
    | Neg Expr
    | If Expr Expr Expr
    | Do [DoStatement]
    deriving (Show, Eq)

-- util

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

surround :: Parser a -> Parser b -> Parser b
surround a = between a a

----- actual lang

reserved :: [Text]
reserved = ["true", "false", "call", "fun", "and", "do", "if"]

identifier :: Parser Text
identifier = do
    name <- T.cons
        <$> (satisfy ((||) <$> isAlpha <*> ('_' ==)) <?> "first character of identifier must be alpha or _")
        <*> takeWhileP Nothing restrict

    guard $ name `notElem` reserved

    lexeme $ pure name
    where
    restrict = or . (<$> [isAlphaNum, (== '_'), (== '\'')]) . flip ($)

ty :: Parser Type
ty = (try fnType <|> Raw <$> identifier) <?> "type"

fnType :: Parser Type
fnType = symbol "fn" *> (FnTy <$> try (many ty) <*> (symbol "->" *> ty))

pDecls :: Parser [Decl]
pDecls = decls eof

decls :: Parser () -> Parser [Decl]
decls = manyTill decl

decl :: Parser Decl
decl = choice [try letP, try funP, try extern]

letP :: Parser Decl
letP = do
    void $ lexeme "let"

    Let <$> identifier <*> (symbol ":" *> ty) <*> (symbol "=" *> expr' <* symbol ";")

funP :: Parser Decl
funP = do
    void $ lexeme "fn"

    Function <$> identifier <*> many param <*> (symbol "->" *> ty) <*> (symbol "=" *> expr' <* symbol ";")

ops :: [[Operator Parser Expr]]
ops = [ [ Prefix $ Neg <$ symbol "-" ]
      , [ infixL Mul "*" , infixL Div "/" ]
      , [ infixL Add "+" , infixL Sub "-" ]
      , [ infixL LeEqTo "<="
        , infixL GrEqTo ">="
        , infixL GreaterThan ">"
        , infixL LessThan "<"
        ]
      , [ infixL EqualTo "==" ]
      , [ infixL And "and" ]
      , [ infixL Or "or" ]
      ]

infixL :: BinOp -> Text -> Operator Parser Expr
infixL op sym = InfixL $ Operator op <$ symbol sym

expr :: Parser Expr
expr = choice
    [ try $ parens expr'
    -- , parens expr'
    , func
    , try unit
    , stringLiteral
    , try call
    , Identifier <$> try identifier
    , try float
    , doP
    , ifP
    , int
    , bool
    ]

expr'' :: Parser Expr
expr'' = sub -- makeExprParser sub ops 
    where
    sub = choice
        [ try $ parens expr'
        , func
        , try unit
        , stringLiteral
        , Identifier <$> try identifier
        , try float
        , doP
        , ifP
        , int
        , bool
        ]

expr' :: Parser Expr
expr' = makeExprParser expr ops

unit :: Parser Expr
unit = symbol "()" $> UnitLit

bool :: Parser Expr
bool = BoolLit <$> (true <|> false)
  where
    true  = True <$ lexeme (symbol "true")
    false = False <$ lexeme (symbol "false")

extern :: Parser Decl
extern = symbol "extern" *> (Extern <$> identifier <*> many param <*> ty <* symbol ";")

int :: Parser Expr
int = lexeme (IntLit <$> L.decimal) <?> "integer literal"

float :: Parser Expr
float = lexeme (FloatLit <$> L.float) <?> "floating point literal"

doP :: Parser Expr
doP = do
    void $ symbol "do" *> symbol "{"

    let doLet = DoLet <$>
            (symbol "let" *> identifier) <*>
            (symbol ":" *> ty) <*>
            (symbol "=" *> expr')

    let doExpr = DoExpr <$> expr'
    let doS = (doLet <|> doExpr) <* symbol ";"

    v <- Do <$> many doS

    void $ symbol "}"

    pure v

ifP :: Parser Expr
ifP = symbol "if" *> (If <$> expr' <*> expr <*> expr)

call :: Parser Expr
call = Call <$> f <*> params <?> "function call"
  where
    f      = parens expr <|> (Identifier <$> try identifier) <?> "function expression"
    params = some expr'' 

stringLiteral :: Parser Expr
stringLiteral = do
    str <- lexeme . surround (char '"') $ takeWhileP Nothing (/= '"')

    -- stolen trick: use Haskell's string parsing to deal with escapes
    pure . StringLiteral $ read ('"' : T.unpack str ++ "\"")

func :: Parser Expr
func = symbol "\\" *> (
        Lambda
            <$> many param
            <*> ty
            <*> (symbol "->" *> expr')
    ) <?> "lambda"

param :: Parser Parameter
param = parens (Parameter <$> identifier <*> (symbol ":" *> ty)) <?> "parameter"
