{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Runic.Pretty
    ( Pretty
    , pretty
    , render
    , renderT
    , kw
    , id
    , idType
    , lit
    , op
    ) where

import           Data.Text                      ( Text )
import           Runic.Parser                   ( BinOp(..)
                                                , Decl(..)
                                                , DoStatement(..)
                                                , Expr(..)
                                                , Parameter(Parameter)
                                                , Value(..)
                                                )
import qualified Runic.Parser                  as P
import           Prelude                 hiding ( id )
import           Prettyprinter                  ( (<+>)
                                                , Doc
                                                , LayoutOptions(..)
                                                , PageWidth(AvailablePerLine)
                                                )
import           Prettyprinter.Render.Terminal  ( AnsiStyle )

import qualified Prettyprinter                 as P
import qualified Prettyprinter.Render.Terminal as P.Term
import qualified Prettyprinter.Render.Text     as P.Text

import           Data.String                    ( IsString )
-- import           Runic.Typecheck                ( SemantError(..)
--                                                 , Type(..)
--                                                 )
import Runic.Types (Type(..))

layoutOptions :: Int -> LayoutOptions
layoutOptions columns = LayoutOptions { layoutPageWidth = AvailablePerLine columns 1 }

render :: Pretty a => a -> Text
render = P.Term.renderStrict . P.layoutSmart (layoutOptions 4) . pretty

renderT :: Pretty a => a -> Text
renderT = P.Text.renderStrict . P.layoutSmart (layoutOptions 4) . pretty

class Pretty a where
    pretty :: a -> Doc AnsiStyle

instance Pretty Double where
    pretty = P.pretty

instance Pretty Int where
    pretty = P.pretty

instance Pretty Integer where
    pretty = P.pretty

instance Pretty Bool where
    pretty = P.pretty

instance Pretty Float where
    pretty = P.pretty

instance Pretty Text where
    pretty = P.pretty

instance Pretty () where
    pretty = P.pretty

instance Pretty String where
    pretty = P.pretty

instance Pretty (Doc AnsiStyle) where
    pretty = id

annotateColor :: P.Term.Color -> Doc AnsiStyle -> Doc AnsiStyle
annotateColor c = P.annotate (P.Term.bold <> P.Term.color c)

kw :: Doc AnsiStyle -> Doc AnsiStyle
kw = annotateColor P.Term.Red

op :: Doc AnsiStyle -> Doc AnsiStyle
op = annotateColor P.Term.Green

lit :: Doc AnsiStyle -> Doc AnsiStyle
lit = annotateColor P.Term.Yellow

id :: Doc AnsiStyle -> Doc AnsiStyle
id = annotateColor P.Term.Blue

idType :: Doc AnsiStyle -> Doc AnsiStyle
idType = annotateColor P.Term.Magenta

pargs :: Pretty a => [a] -> Doc AnsiStyle
pargs l | null l    = mempty
        | otherwise = (<> " ") . (" " <>) . P.hsep . P.punctuate " " . map pretty $ l

blk :: Doc a -> Doc a
blk b = P.cat ["(", P.indent 4 b, ")"]

pty :: Pretty a => a -> Doc AnsiStyle
pty = idType . pretty

pid :: Pretty a => a -> Doc AnsiStyle
pid = id . pretty

parens :: (Semigroup a, IsString a) => a -> a
parens x = "(" <> x <> ")"

instance Pretty DoStatement where
    pretty = \case
        DoExpr e -> pretty e <> ";"
        DoLet var ty expr ->
            kw "let" <+> pid var <> ":" <+> pty ty <+> "= " <> blk (pretty expr) <> ";"

instance Pretty Type where
    pretty = \case
        I32               -> idType "i32"
        Char              -> idType "char"
        Unit              -> idType "unit"
        Bool              -> idType "bool"
        String            -> idType "str"
        F32               -> idType "f32"
        Func    tys ty    -> kw "fn" <+> foldMap it tys <+> "->" <+> it ty
        Generic ty  inner -> it ty <> "(" <> it inner <> ")"
        Pointer ty        -> "*" <> it ty
        where it = idType . pretty

instance Pretty SemantError where
    pretty = \case
        NotEnoughArguments { callExpr, expectedCt, gotCt } ->
            kw "error:"
                <+> "Expected to get"
                <+> pretty expectedCt
                <+> "arguments in call expression"
                <+> pretty callExpr
                <+> "but got"
                <+> pretty gotCt
        IdentifierNotInScope { var, varExpr } ->
            kw "error:"
                <+> "identifier"
                <+> id (pretty var)
                <+> "not in scope in"
                <+> pretty varExpr
        TypeNotInScope { tVar, tBind } ->
            kw "error:"
                <+> "type "
                <>  id (pretty tVar)
                <>  maybe mempty ((" in binding " <+>) . pretty) tBind
                <+> "not in scope!"
        Internal e -> "Internal error! " <> pretty e
        NoMain     -> "Program is missing a main!"
        DuplicateDeclarationError { decl } ->
            kw "error:" <+> "several declarations of" <+> id (pretty decl) <> "!"
        TypeError { expected, got, errorExpr, containingExpr } ->
            kw "error:"
                <+> "Expected type"
                <+> idType (pretty expected)
                <+> "but got"
                <+> idType (pretty got)
                <>  P.group
                        (P.vcat
                            [ mempty
                            , "in expression"
                            <+> pretty errorExpr
                            <>  maybe mempty
                                      ((<+>) (P.line <> "contained in expression") . pretty)
                                      containingExpr
                            ]
                        )
        MismatchedArms { tArm1, tArm2, arm1E, arm2E } ->
            kw "error:"
                <+> "if arms have mismatched types, first arm has type"
                <+> pretty tArm1
                <+> "but second arm has type"
                <+> pretty tArm2
                <>  P.line
                <>  "first arm:"
                <+> pretty arm1E
                <>  P.line
                <>  "second arm:"
                <+> pretty arm2E
        NotAFunction { fnExpr, callExpr } ->
            kw "error:"
                <+> "tried calling non-function"
                <+> pretty fnExpr
                <+> "in expression"
                <+> pretty callExpr

instance Pretty P.Type where
    pretty = \case
        P.Generic t inner -> id (pretty t) <> "(" <> id (pretty inner) <> ")"
        P.Pointer t       -> id "*" <> id (pretty t)
        P.Raw     t       -> id $ pretty t
        P.FnTy ps ret ->
            "fn" <+> foldr ((<+>) . id . pretty) mempty ps <+> "->" <+> id (pretty ret)

instance Pretty BinOp where
    pretty = \case
        Add         -> op "+"
        Sub         -> op "-"
        Mul         -> op "*"
        Div         -> op "/"
        LessThan    -> op "<"
        LeEqTo      -> op "<="
        GreaterThan -> op ">"
        GrEqTo      -> op ">="
        EqualTo     -> op "=="
        And         -> op "and"
        Or          -> op "or"
        Idx         -> op "!"

instance Pretty [Decl] where
    pretty = P.vsep . map pretty

instance Pretty Parameter where
    pretty (Parameter name ty) = parens $ id (pretty name) <> ":" <+> idType (pretty ty)

instance Pretty Decl where
    pretty = \case
        Let var ty expr ->
            kw "let" <+> pid var <> ":" <+> pty ty <+> "= " <> blk (pretty expr) <> ";"
        Function fn args ret expr ->
            kw "fn"
                <+> pid fn
                <>  pargs args
                <+> "->"
                <+> pty ret
                <+> "="
                <+> blk (pretty expr)
                <>  ";"
        Extern e args ret -> kw "extern" <+> pid e <> pargs args <+> pty ret <> ";"

instance Pretty Expr where
    pretty = \case
        Val v               -> pretty v
        Lambda params ret e -> "(\\" <> pargs params <+> pretty ret <+> "->" <+> pretty e <> ")"
        StringLiteral s     -> lp s
        IntLit        i     -> lp i
        FloatLit      f     -> lp f
        BoolLit       b     -> lp ((if b then "true" else "false") :: Text)
        UnitLit             -> lit "()"
        Neg e               -> op "-" <> parens (pretty e)
        Operator o lhs rhs  -> parens $ pretty lhs <+> op (pretty o) <+> pretty rhs
        Identifier i        -> ip i
        Call f ps           -> ip f <+> P.hcat (P.punctuate " " (map (parens . pretty) ps))

        If cond arm1 arm2   -> P.vcat
            [ "(" <> kw "if"
            , P.indent 4 $ P.vcat [parens $ pretty cond, parens $ pretty arm1, parens $ pretty arm2]
            , ")"
            ]

        Do exps ->
            P.cat [kw "do" <+> "{", P.indent 4 . P.vcat . map ((<> ";") . pretty) $ exps, "}"]

      where
        lp = lit . pretty
        ip = id . pretty

instance Pretty Value where
    pretty v = case v of
        IInt    i      -> vp i
        IBool   b      -> wh (if b then "true" else "false")
        IFloat  f      -> vp f
        IString s      -> vp s
        IPtr t ptr     -> wh "*" <> vp t <> wh ": " <> vp ptr
        IUnit          -> lit "()"
        IFunc params e -> lit "fn" <+> foldr ((<+>) . vp) mempty params <+> vp e
        IExtern params -> undefined
      where
        vp = annotateColor P.Term.White . pretty
        wh = annotateColor P.Term.White
