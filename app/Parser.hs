{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( pDecls
    , expr
    , Decl(..)
    , Expr(..)
    , Value(..)
    , Identifier
    , Parameter(..)
    )
where

import           Control.Monad (void, guard)
import           Data.Char (isAlphaNum, isAlpha)
import           Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))

type Parser = Parsec Void Text

type Identifier = Text
type Type       = Text
type ReturnType = Text

data Parameter = Parameter Identifier Type
    deriving (Show, Eq)

data Decl =   Let Identifier Type Expr
            | Function Identifier [Parameter] ReturnType Expr
            | Extern Identifier [Parameter] ReturnType
            deriving (Show, Eq)

data Value =
      IInt Int
    | IBool Bool
    | IString Text
    | IFloat Float
    | IUnit
    | IFunc [Identifier] Expr
    | IExtern Identifier
    deriving (Show, Eq)

data Expr =   Lambda [Parameter] ReturnType Expr
            | Val Value
            | StringLiteral Text
            | Add Expr Expr
            | Mul Expr Expr
            | Sub Expr Expr
            | Div Expr Expr
            | LessThan Expr Expr
            | LeEqTo Expr Expr
            | GreaterThan Expr Expr
            | GrEqTo Expr Expr
            | EqualTo Expr Expr
            | Identifier Text
            -- Call (func, params)
            | Call Expr [Expr]
            | IntLit Int
            | FloatLit Float
            | BoolLit Bool
            | UnitLit
            | Neg Expr
            | And Expr Expr
            | Or Expr Expr
            | If Expr Expr Expr
            | Do [Expr]
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

----- actual lang shit

reserved :: [Text]
reserved =
    [ "true"
    , "false"
    , "call"
    , "fun"
    , "and"
    , "do"
    , "if"
    ]

identifier :: Parser Text
identifier = do
    name <- T.cons
            <$> satisfy isAlpha
            <*> takeWhileP Nothing ((||) <$> isAlphaNum <*> ('_' ==))

    guard $ name `notElem` reserved

    lexeme $ pure name

pDecls :: Parser [Decl]
pDecls = decls eof

decls :: Parser () -> Parser [Decl]
decls = manyTill decl

decl :: Parser Decl
decl = choice [ try letP, try funP, try extern ]

letP :: Parser Decl
letP = do
    void $ lexeme "let"

    Let
        <$> identifier
        <*> (symbol ":" *> identifier)
        <*> (symbol "=" *> expr)

funP :: Parser Decl
funP = do
    void $ lexeme "fn"

    Function
        <$> identifier
        <*> many param
        <*> (symbol "->" *> identifier)
        <*> (symbol "=" *> expr)

expr :: Parser Expr
expr =
    choice [ try add
           , try sub
           , try mul
           , try (binop "<" LessThan)
           , try (binop ">" GreaterThan)
           , try (binop ">=" GrEqTo)
           , try (binop "<=" LeEqTo)
           , try (binop "==" EqualTo)
           , try divP
           , try andP
           , try orP
           , try func
           , stringLiteral
           , Identifier <$> try identifier
           , try call
           , try float
           , try neg
           , try doP
           , try ifP
           , try unit
           , int
           , bool
           , parens expr
           ]

neg :: Parser Expr
neg = Neg <$> (symbol "-" *> expr)

unit :: Parser Expr
unit = symbol "()" $> UnitLit

bool :: Parser Expr
bool = BoolLit <$> (True <$ lexeme (symbol "true") <|> False <$ lexeme (symbol "false"))

extern :: Parser Decl
extern = symbol "extern" *> (Extern <$> identifier <*> many param <*> identifier)

int :: Parser Expr
int = lexeme $ IntLit <$> L.decimal

float :: Parser Expr
float = lexeme $ FloatLit <$> L.float

binop :: Text -> (Expr -> Expr -> Expr) -> Parser Expr
binop sym ctor = parens (symbol sym *> (ctor <$> expr <*> expr))

add :: Parser Expr
add = binop "+" Add

sub :: Parser Expr
sub = binop "-" Sub

mul :: Parser Expr
mul = binop "*" Mul

divP :: Parser Expr
divP = binop "/" Div

andP :: Parser Expr
andP = binop "and" And

orP :: Parser Expr
orP = binop "or" Or

doP :: Parser Expr
doP = symbol "do" *> (Do <$> many expr)

ifP :: Parser Expr
ifP = symbol "if" *> (If <$> expr <*> expr <*> expr)

call :: Parser Expr
call = parens $ do
    void $ symbol "call"

    f <- expr

    params <- many expr

    pure $ Call f params

stringLiteral :: Parser Expr
stringLiteral = do
    str <- lexeme . surround (char '"') $ takeWhileP Nothing (/= '"')

    -- stolen trick: use Haskell's string parsing to deal with escapes
    pure . StringLiteral $ read ('"' : cs str ++ "\"")

func :: Parser Expr
func = parens $ symbol "fun" *> (
        Lambda
            <$> many param
            <*> return_type
            <*> (symbol "->" *> expr)
    )
    where
    return_type = identifier

param :: Parser Parameter
param = parens (Parameter <$> identifier <*> (symbol ":" *> identifier))
