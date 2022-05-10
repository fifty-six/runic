{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (Pretty, pretty, render) where

import Prelude hiding (id)
import Parser (BinOp(..), Expr(..), Decl(..), Value(..), Parameter (Parameter))
import Prettyprinter (Doc, LayoutOptions(..), (<+>), PageWidth (AvailablePerLine))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Data.Text (Text)

import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Terminal as P.Term
import Typecheck (SemantError(..), Type(..))

layoutOptions :: Int -> LayoutOptions
layoutOptions columns = LayoutOptions { layoutPageWidth = AvailablePerLine columns 1 }

render :: Pretty a => a -> Text
render = P.Term.renderStrict . P.layoutSmart (layoutOptions 4) . pretty

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

-- Overlaps my list impl and I'm not gonna write out [a] for every a I use
-- at least, for now.
instance Pretty String where
    pretty = P.pretty

instance Pretty (Doc AnsiStyle) where
    pretty = id

instance Pretty Type where
    pretty = \case
        I32 -> idType "i32"
        Unit -> idType "unit"
        Bool -> idType "bool"
        String -> idType "str"
        F32 -> idType "f32"
        Func tys ty -> kw "fn" <+> foldMap it tys <+> "->" <+> it ty
        where it = idType . pretty

instance Pretty SemantError where
    pretty = \case
        IdentifierNotInScope { var, varExpr } -> "Identifier" <+> id (pretty var) <+> "not in scope in" <+> pretty varExpr
        TypeNotInScope { tVar, tBind } -> "Type " <> pretty tVar <> maybe mempty (("in binding " <+>) . pretty) tBind <> "with expression "
        Internal e -> "Internal error! " <> pretty e
        NoMain -> "Program is missing a main!"
        TypeError { expected, got, errorExpr, containingExpr } -> 
            "Expected type" <+> idType (pretty expected) <+> 
            "but got" <+> idType (pretty got) <> P.group (P.vcat
            [ mempty
            , "in expression" <+> "(" <> pretty errorExpr  <> ")" 
            <> maybe mempty (\a -> P.line <> "contained in expression" <+> "(" <> pretty a <> ")") containingExpr
            ])
        MismatchedArms { tArm1, tArm2, arm1E, arm2E } -> 
            "If arms have mismatched types, first arm has type" 
            <+> pretty tArm1 
            <+> "but second arm has type" <+> pretty tArm2
            <> P.line <> "first arm:" <+> pretty arm1E
            <> P.line <> "second arm:" <+> pretty arm2E
        NotAFunction { fnExpr, callExpr } -> "Tried calling non-function" <+> pretty fnExpr <+> "in expression" <+> pretty callExpr

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

-- instance Pretty a => Pretty [a] where
--     pretty l = foldMap (\a -> pretty a <> " ") l

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

instance Pretty [Decl] where
    pretty = P.vsep . map pretty

instance Pretty Parameter where
    pretty (Parameter name ty) = "(" <> id (pretty name) <> ":" <+> idType (pretty ty) <> ")"

instance Pretty Decl where
    pretty = \case
        Let var ty expr ->
            kw "let" <+> pid var <> ":" <+> pty ty <+> "= " <> blk (pretty expr)
        Function fn args ret expr -> kw "fn"     <+> pid fn <> pargs args <+> "->" <+> pty ret <+> "=" <+> blk (pretty expr)
        Extern e args ret         -> kw "extern" <+> pid e  <> pargs args <+> pty ret
        where
        pty = idType . pretty
        pid = id . pretty
        blk b = P.cat [ "(", P.indent 4 b, ")" ]
        pargs l
            | null l = mempty
            | otherwise = (" " <>) . P.hsep . P.punctuate " " . map pretty $ l

instance Pretty Expr where
    pretty = \case
        Val v              -> pretty v
        StringLiteral s    -> lp s
        IntLit i           -> lp i
        FloatLit f         -> lp f
        BoolLit b          -> lp b
        UnitLit            -> lit "()"
        Neg e              -> op "-" <> "(" <> pretty e <> ")"
        Operator o lhs rhs -> "(" <> op (pretty o) <+> pretty lhs <+> pretty rhs <> ")"
        Identifier i       -> ip i
        Call f ps          -> kw "call" <+> ip f <+> P.hcat (map pretty ps)

        If cond arm1 arm2  -> P.vcat [ "(" <> kw "if"
                                     , P.indent 4 $ P.vcat [pretty cond, pretty arm1, pretty arm2]
                                     , ")"]

        Do exps            -> P.cat [ kw "do" <+> "{"
                                , P.indent 4 . P.vcat . map ((<> ";") . pretty) $ exps
                            , "}" ]

        where
        lp = lit . pretty
        ip = id . pretty

instance Pretty Value where
    pretty v = case v of
        IInt i         -> vp i
        IBool b        -> vp b
        IFloat f       -> vp f
        IString s      -> vp s
        IUnit            -> lit "()"
        IFunc params e -> lit "fn" <+> foldr ((<+>) . vp) mempty params <+> vp e
        IExtern params -> undefined

        where vp = annotateColor P.Term.White . pretty
    --     where lp = lit . pretty
    -- pretty (IInt i) = lit $ pretty i
    -- pretty (IBool b) = lit $ pretty b
    -- pretty (IFloat f) = lit $ pretty f
    -- pretty (IString s) = lit $ pretty s
    -- pretty IUnit = lit "()"
    -- pretty (IFunc params e) = lit "fn " <> (lit . pretty) params <> (lit . pretty) e
    -- pretty (IExtern params) = undefined
    --     where lp = lit . pretty
