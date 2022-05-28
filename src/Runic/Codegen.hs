{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Runic.Codegen where

import           LLVM.AST                       ( Operand )
import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP
import qualified LLVM.AST.IntegerPredicate     as IP
import           LLVM.AST.Name
import qualified LLVM.AST.Type                 as AST

import qualified LLVM.IRBuilder.Constant       as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import           LLVM.Prelude                   ( ShortByteString )

import qualified Data.Map                      as M

import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.State.Class      ( MonadState )
import           Control.Monad.State ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )
import           Data.Maybe                     ( isJust )
import           Data.String                    ( fromString )
import           Data.String.Conversions        ( ConvertibleStrings
                                                , convertString
                                                , cs
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Prelude                 hiding ( id )
import           Runic.Parser                   ( BinOp(..) )
import           Runic.Pretty                   ( renderT )
import           Runic.Typecheck
import           Runic.Util                     ( locally )
import           Text.Printf                    ( printf )

import Control.Monad.Trans (lift)

data Env = Env
    { operands :: M.Map Text Operand
    , strings  :: M.Map Text Operand
    }

type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

instance ConvertibleStrings Text ShortByteString where
    convertString = fromString . T.unpack

codegenProg :: [SDecl] -> AST.Module
codegenProg decls =
    flip evalState (Env { operands = M.empty, strings = M.empty }) $ L.buildModuleT "runic" $ do
        let unit = AST.StructureType { AST.isPacked = False, AST.elementTypes = [] }
        _ <- L.typedef (mkName "intrinsic.unit") (Just unit)
        mapM_ genDecls decls

convType :: (MonadState Env m, L.MonadModuleBuilder m) => Type -> m AST.Type
convType I32                    = pure AST.i32
convType Unit                   = pure (AST.NamedTypeReference (mkName "intrinsic.unit"))
convType Bool                   = pure AST.i1
convType Char                   = pure AST.i8
convType String                 = pure $ AST.ptr AST.i8
convType F32                    = pure AST.float
convType (Pointer ty          ) = AST.ptr <$> convType ty
convType (Generic name   param) = error "todo - convType: Generic"
convType (Func    params ret  ) = do
    ret'    <- convType ret
    params' <- mapM convType params
    pure . AST.ptr $ AST.FunctionType ret' params' False

negateC :: Operand -> Codegen Operand
negateC = L.sub (L.int32 0)

mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = L.hasTerminator >>= flip unless instr

-- lookupInternal :: MonadState Env m => Text -> m Operand
lookupInternal :: (MonadState Env m, L.MonadModuleBuilder m) => Type -> Text -> m Operand
lookupInternal t id = mdo
    v <- gets $ M.lookup id . operands

    case v of
        Just a  -> pure a
        Nothing -> error $ printf "Unable to get lvalue %s" id

genFunc :: Name -> [SParameter] -> Type -> SExpr -> LLVM Operand
genFunc id params t expr = do
    (function, strs) <- locally $ const $ mdo
        t'      <- convType t
        params' <- mapM mkParam params

        fun     <- L.function id params' t' genFuncBody

        strs'   <- gets strings
        pure (fun, strs')

    modify $ \env -> env { strings =  strs }

    pure function

    where
    mkParam (SParameter pid pty) = (,) <$> convType pty <*> pure (L.ParameterName $ cs pid)

    genFuncBody :: [Operand] -> Codegen ()
    genFuncBody ops = do
        forM_ (zip ops params) $ \(op, SParameter pid pty) -> mdo
            registerOperand pid op

        expr' <- genExpr expr

        L.ret expr'

genDecls :: SDecl -> LLVM ()
genDecls (SLet id ty expr@(t, e)            ) = error "todo: let"

genDecls (SFunction id params ty expr@(t, e)) = mdo
    _        <- registerOperand id function

    function <- genFunc (AST.mkName . cs $ id) params ty expr

    pure ()

genDecls (SExtern id params ty) = do
    params' <- mapM (\(SParameter pid pty) -> convType pty) params
    ty'     <- convType ty

    extern  <- L.extern (mkName . cs $ id) params' ty'

    registerOperand id extern

genExpr :: SExpr -> Codegen Operand
genExpr (I32   , SIntLit i       ) = pure . L.int32 $ fromIntegral i
genExpr (F32   , SFloatLit f     ) = pure $ L.single f
genExpr (Unit  , SUnitLit        ) = AST.ConstantOperand <$> (C.AggregateZero <$> convType Unit)
genExpr (Bool  , SBoolLit b      ) = pure . L.bit $ if b then 1 else 0

genExpr (String, SStringLiteral s) = do
    strs <- gets strings

    case M.lookup s strs of
        Just o  -> pure o
        Nothing -> do
            let name = mkName (show (M.size strs) <> ".str")

            op <- L.globalStringPtr (T.unpack s) name

            modify $ \e -> e { strings = M.insert s (AST.ConstantOperand op) strs }

            pure $ AST.ConstantOperand op

genExpr (_, SIntLit _       ) = error "Got literal with incorrect type!"
genExpr (_, SFloatLit _     ) = error "Got literal with incorrect type!"
genExpr (_, SBoolLit _      ) = error "Got literal with incorrect type!"
genExpr (_, SUnitLit        ) = error "Got literal with incorrect type!"
genExpr (_, SStringLiteral _) = error "Got literal with incorrect type!"

genExpr (t, SIdentifier id  ) = lookupInternal t id -- AST.LocalReference <$> convType t <*> (pure . AST.mkName $ T.unpack id) -- lookupInternal id

genExpr (t, SCall fn params ) = do
    params' <- mapM (fmap (, []) . genExpr) params
    fn'     <- genExpr fn

    L.call fn' params'

genExpr (t, SNeg i   ) = negateC =<< genExpr i

genExpr (t, SDo exprs) = mdo
    (e', strs) <- locally . const $ mdo
        forM_ (init exprs) genExpr

        (,) <$> genExpr (last exprs) <*> gets strings

    modify $ \e -> e { strings = strs }

    pure e'

genExpr (t, SDoLet id ty expr) = do
    op <- genExpr expr

    registerOperand id op

    pure op

genExpr (t, SIf cond brt@(brType, _) brf) = mdo
    cond' <- genExpr cond
    L.condBr cond' blkT blkF

    blkT <- L.block `L.named` "brT"
    brt' <- genExpr brt
    mkTerminator $ L.br merge

    blkF <- L.block `L.named` "brT"
    brf' <- genExpr brf
    mkTerminator $ L.br merge

    merge <- L.block `L.named` "merge"

    L.phi [(brf', blkF), (brt', blkT)]

genExpr (t, SLambda params ty expr) = do
    name <- L.fresh
    lift $ genFunc name params ty expr

genExpr (t, SOperator op lhs@(tl, _) rhs@(tr, _)) = do
    lhs' <- genExpr lhs
    rhs' <- genExpr rhs

    let internal (a :: Text) =
            error $ printf "Internal: case op of %s -> %s %s" a (show tl) (show tr)

    let cmp a ip fp = case (tl, tr) of
            (I32      , I32      ) -> L.icmp ip lhs' rhs'
            (F32      , F32      ) -> L.fcmp fp lhs' rhs'
            (Pointer _, Pointer _) -> do
                lhs'' <- L.ptrtoint lhs' AST.i64
                rhs'' <- L.ptrtoint rhs' AST.i64
                L.icmp ip lhs'' rhs''
            _ -> internal a

    case op of
        Idx -> case (tl, tr) of
            (Pointer _, I32) -> L.gep lhs' [rhs']
            _                -> internal "Idx"

        Add -> case (tl, tr) of
            (I32       , I32        ) -> L.add lhs' rhs'
            (F32       , F32        ) -> L.fadd lhs' rhs'
            (Pointer _ , I32        ) -> L.gep lhs' [rhs']
            (Pointer ty, Pointer ty') -> do
                unless (ty == ty') $ error $ printf
                    "Adding pointers of incompatible types %s %s"
                    (renderT ty)
                    (renderT ty')

                L.add lhs' rhs'

            _ -> internal "Add"

        Sub -> case (tl, tr) of
            (I32       , I32        ) -> L.sub lhs' rhs'
            (F32       , F32        ) -> L.fsub lhs' rhs'
            (Pointer _ , I32        ) -> L.gep lhs' =<< sequence [negateC rhs']
            (Pointer ty, Pointer ty') -> do
                unless (ty == ty') $ error $ printf
                    "Subtracting pointers of incompatible types %s %s!"
                    (renderT ty)
                    (renderT ty')

                -- TODO maybe, division and whatnot because ptrdiff_t nonsense

                L.sub lhs' rhs'
            _ -> internal "Sub"

        Mul -> case (tl, tr) of
            (I32, I32) -> L.mul lhs' rhs'
            (F32, F32) -> L.fmul lhs' rhs'
            _          -> internal "Mul"

        Div -> case (tl, tr) of
            (I32, I32) -> L.sdiv lhs' rhs'
            (F32, F32) -> L.fdiv lhs' rhs'
            _          -> internal "Div"

        LessThan    -> cmp "LessThan" IP.SLT FP.OLT
        LeEqTo      -> cmp "LeEqTo" IP.SLE FP.OLE
        EqualTo     -> cmp "EqualTo" IP.EQ FP.OEQ
        GreaterThan -> cmp "GreaterThan" IP.SGT FP.OGT
        GrEqTo      -> cmp "GreaterThan" IP.SGE FP.OGE

        And         -> L.and lhs' rhs'
        Or          -> L.or lhs' rhs'

registerOperand :: MonadState Env m => Text -> Operand -> m ()
registerOperand name op = do
    ops <- gets operands

    when (isJust $ M.lookup name ops) $ error $ printf "Duplicate operand %s!" name

    modify $ \e -> e { operands = M.insert name op (operands e) }

