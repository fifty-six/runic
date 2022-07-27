{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Runic.Types
    () where
import           Prelude                 hiding ( id )
import qualified Data.Maybe                    as Maybe
import qualified Data.List                     as L
import           Runic.Util                     ( (...) )
import           Control.Monad                  ( zipWithM
                                                , foldM
                                                )
import           Text.Printf                    ( printf )

type Id = String

-- TODO: Use monad fresh
enumId :: Int -> Id
enumId n = "v" <> show n

-- A kind is either a solid type or a kind constructor from a kind to a kind
-- This handles multi-arg like a linked list.
data Kind = Star | KFun Kind Kind
  deriving (Show, Eq)

data Type = TVar TyVar | TCon TyCon | TAp Type Type | TGen Int
  deriving Eq

data TyVar = TyVar Id Kind
    deriving Eq

data TyCon = TyCon Id Kind
    deriving Eq

tUnit :: Type
tUnit = tCon "()" Star

tChar :: Type
tChar = tCon "char" Star

tInt32 :: Type
tInt32 = tCon "i32" Star

tFloat :: Type
tFloat = tCon "float" Star

tArrow :: Type
tArrow = tCon "(->)" $ KFun Star $ KFun Star Star

tTuple2 :: Type
tTuple2 = tCon "(,)" $ KFun Star $ KFun Star Star

tVar = TVar ... TyVar
tCon = TCon ... TyCon

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

pair :: Type -> Type -> Type
pair a = TAp (TAp tTuple2 a)

class HasKind t where
  kind :: t -> Kind

instance HasKind TyVar where
    kind (TyVar v k) = k

instance HasKind TyCon where
    kind (TyCon v k) = k

instance HasKind Type where
    kind = \case
        TVar tv  -> kind tv
        TCon tc  -> kind tc
        TAp ty _ -> case kind ty of
            KFun _ k -> k
            Star     -> error "Applications cannot have star kind"
        -- "Generic/Quantified" type variables don't have a kind
        TGen _ -> error "TGen does not have kind"

type Subst = [(TyVar, Type)]

nullSubst :: Subst
nullSubst = []

-- | Well-formed iff kind u = kind t
(+->) :: TyVar -> Type -> Subst
u +-> t = [(u, t)]

class Types t where
  -- | Apply a substitution
  apply :: Subst -> t -> t
  -- | Return the set of type variables appearing in its argument
  tvs :: t -> [TyVar]

instance Types Type where
    apply s (TVar u ) = Maybe.fromMaybe (TVar u) $ lookup u s
    apply s (TAp l r) = TAp (apply s l) (apply s r)
    -- For TyCon / TyGen, we don't apply anything
    apply s t         = t

    tvs (TVar u ) = [u]
    tvs (TAp l r) = tvs l `L.union` tvs r
    tvs t         = []

instance Types a => Types [a] where
    apply = map . apply
    tvs   = L.nub . L.concatMap tvs

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [ (u, apply s1 t) | (u, t) <- s2 ] ++ s1

merge :: MonadFail m => Subst -> Subst -> m Subst
merge s1 s2 | valid     = pure $ s1 <> s2
            | otherwise = fail "failed merge"
  where
    valid = all equiv $ map fst s1 `L.intersect` map fst s2
    equiv = ((==) <$> apply s1 <*> apply s2) . TVar

class Unify t where
  mgu :: MonadFail m => t -> t -> m Subst

instance Unify Type where
    mgu (TAp l r) (TAp l' r') = do
        s1 <- mgu l l'
        s2 <- mgu (apply s1 r) (apply s1 r')
        return $ s2 <> s1
    mgu (TVar u) t                       = varBind u t
    mgu t        (TVar u)                = varBind u t
    mgu (TCon tc) (TCon tc') | tc == tc' = pure nullSubst
    mgu t1 t2                            = fail "types do not unify"

instance (Unify t,  Types t) => Unify [t] where
    mgu (x : xs) (y : ys) = do
        s1 <- mgu x y
        s2 <- mgu (apply s1 xs) (apply s1 ys)
        return (s2 @@ s1)
    mgu [] [] = pure nullSubst
    mgu _  _  = fail "Unable to unify lists (Do they have different lengths?)"

varBind :: MonadFail m => TyVar -> Type -> m Subst
varBind u t | t == TVar u      = pure nullSubst
            | u `elem` tvs t   = fail "occurs check fails"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = pure $ u +-> t

class Match t where
  match :: MonadFail m => t -> t -> m Subst

instance Match t => Match [t] where
    match ts ts' = do
        ss <- zipWithM match ts ts'
        foldM merge nullSubst ss

instance Match Type where
    match (TAp l r) (TAp l' r') = do
        sl <- match l l'
        sr <- match r r'
        merge sl sr
    match (TVar u) t | kind u == kind t    = return $ u +-> t
    match (TCon tc) (TCon tc') | tc == tc' = pure nullSubst
    match t1 t2                            = fail "types do not match"

data Qual t = [Pred] :=> t
    deriving Eq

data Pred = IsIn Id [Type]
    deriving Eq

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tvs (ps :=> t) = tvs ps `L.union` tvs t

instance Types Pred where
    apply s (IsIn id tys) = IsIn id $ apply s tys
    tvs (IsIn id tys) = tvs tys

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu
matchPred = lift match

-- | Lift a substitution onto predicates
lift :: MonadFail m => ([Type] -> [Type] -> m Subst) -> Pred -> Pred -> m Subst
lift m (IsIn i t) (IsIn i' t') | i == i'   = m t t'
                               | otherwise = fail "Type classes differ"

-- | ([Superclasses], [Inst])
type Class = ([Id], [Inst])
type Inst = Qual Pred

data ClassEnv = ClassEnv
    { classes  :: Id -> Maybe Class
    , defaults :: [Type]
    }

super :: ClassEnv -> Id -> [Id]
super ce i = fst $ Maybe.fromJust (classes ce i)

insts :: ClassEnv -> Id -> [Inst]
insts ce i = snd $ Maybe.fromJust (classes ce i)

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce { classes = \j -> if i == j then Just c else classes ce j }

initialEnv :: ClassEnv
initialEnv = ClassEnv { classes = const (fail "class not defined"), defaults = [tInt32, tFloat] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = g =<< f ce

addClass :: Id -> [Id] -> EnvTransformer
addClass i is ce | Maybe.isJust $ classes ce i = fail $ printf "Class %s already defined" i
                 |
  -- TODO: Make this error not shit
                   any (Maybe.isNothing . classes ce) is = fail "Superclass not is defined"
                 | otherwise = pure $ modify ce i (is, [])

addCore =
    addClass "Eq" []
        <:> addClass "Ord"         ["Eq"]
        <:> addClass "Show"        []
        <:> addClass "Functor"     []
        <:> addClass "Applicative" ["Functor"]
        <:> addClass "Monad"       ["Applicative"]

predHead :: Pred -> Id
predHead (IsIn id tys) = id
