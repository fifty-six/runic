{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (id)
import qualified Data.Map                      as M
import qualified Data.Text                      ( )
import           Debug.Trace                    ( trace
                                                , traceShow
                                                , traceShowId
                                                )
import           Runic.Parser                   ( BinOp(..)
                                                , Decl(Let)
                                                , Expr(BoolLit, IntLit, Operator, Val)
                                                , Type(Raw)
                                                , Value(..)
                                                )
import           Runic.Pretty
import           Runic.Typecheck                ( runSemant
                                                , typecheck, SemantError, SDecl
                                                )
import           Test.QuickCheck
import Data.Either (isRight, isLeft)

prop_Test :: Bool
prop_Test = True

newtype WellTyped = WellTyped Decl
    deriving (Show, Eq, Pretty)

newtype IllTyped = IllTyped Decl
    deriving (Show, Eq, Pretty)

numOps :: [BinOp]
numOps = [Add, Mul, Sub, Div]

comps :: [BinOp]
comps = [LessThan, GreaterThan, EqualTo, LeEqTo, GrEqTo]


equalFreqs :: [a] -> Gen a
equalFreqs = frequency . zip [1 ..] . map pure

intE :: Gen Expr
intE = frequency [(4, IntLit <$> arbitrary), (1, Operator <$> equalFreqs numOps <*> intE <*> intE)]

boolE :: Gen Expr
boolE = frequency
    [ (4, BoolLit <$> arbitrary)
    , (1, Operator And <$> boolE <*> boolE)
    , (1, Operator Or <$> boolE <*> boolE)
    , (1, Operator EqualTo <$> boolE <*> boolE)
    , (1, Operator <$> equalFreqs comps <*> intE <*> intE)
    ]

instance Arbitrary WellTyped where
    arbitrary =
        WellTyped <$> oneof [Let "test" (Raw "i32") <$> intE, Let "test" (Raw "bool") <$> boolE]

instance Arbitrary IllTyped where
    arbitrary = do
        (WellTyped decl) <- arbitrary @WellTyped

        -- Irrefutable as we don't have Fun decls yet
        let ~(Let id t e) = decl
        let ~(Raw t') = t

        ty <- Raw <$> oneof (pure <$> filter (/= t') ["i32", "unit", "bool", "f32"])

        pure $ IllTyped (Let id ty e)

runCheck :: [Decl] -> Either SemantError [SDecl]
runCheck = fst . flip runSemant M.empty . typecheck

prop_WellTyped :: WellTyped -> Bool
prop_WellTyped (WellTyped decl) = isRight $ runCheck [decl]

prop_IllTyped :: IllTyped -> Bool
prop_IllTyped (IllTyped decl) = isLeft $ runCheck [decl]

main :: IO ()
main = do
    quickCheck prop_WellTyped
    quickCheck prop_IllTyped
