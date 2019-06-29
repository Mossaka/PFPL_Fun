{-# LANGUAGE OverloadedStrings #-}

module Language.SysF where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import           Language.Common


-------------------------------- Syntax ---------------------------------------------
data Type = Tvar TVar
          | TInt
          | Type :=> Type
          | Forall TVar Type
          | TTuple [Type]

instance Eq Type where
  (Tvar a) == (Tvar b) = a == b
  TInt == TInt = True
  (t1 :=> t2) == (t3 :=> t4) = t1 == t3 && t2 == t4
  (Forall a1 t1) == (Forall a2 t2) = if a1 == a2 then t1 == t2 else ((Tvar a2)⇖a1) t1 == t2
  (TTuple ts1) == (TTuple ts2) = ts1 == ts2
  _ == _ = False

instance IsString Type where
  fromString = Tvar

-- data Expr = Expr Expr Type

data Binop
    = Plus
    | Minus
    | Mul

data Expr = EVar Id
          | EInt Int
          | EFix Id Id Type Type Expr -- fix x (x1:t1):t2.e
          | EApp Expr Expr
          | ELambda TVar Expr
          | EInst Expr Type
          | ETuple [Expr]
          | EProj Int Expr
          | EBin Binop Expr Expr
          | EIfZ Expr Expr Expr

infixr 2 :=>

type Delta = [TVar]
type Gamma = [(Id, Type)]
instance HasGamma Type

instance IsString Expr where
    fromString = EVar

instance Show Binop where
  show = binopString

instance Show Expr where
  show = exprString

instance Show Type where
  show = typeString

binopString :: Binop -> String
binopString Plus  = "+"
binopString Minus = "-"
binopString Mul   = "*"

exprString :: Expr -> String
exprString (EVar id          ) = id
exprString (EInt i           ) = printf "%d" i
exprString (EFix x x1 t1 t2 e) = printf "fix %s (%s:%s):%s.%s" x x1 (show t1) (show t2) (show e)
exprString (EApp    e1 e2    ) = printf "(%s) %s" (show e1) (show e2)
exprString (ELambda a  e     ) = printf "Λ%s.%s" (show a) (show e)
exprString (EInst   e  t     ) = printf "%s[%s]" (show e) (show t)
exprString (ETuple es        ) = "<" ++ (flatten es) ++ ">"
exprString (EProj i e        ) = printf "π_%d(%s)" i (show e)
exprString (EBin biop e1 e2  ) = printf "(%s %s %s)" (show e1) (show biop) (show e2)
exprString (EIfZ e1   e2 e3  ) = printf "if0(%s,%s,%s)" (show e1) (show e2) (show e3)

-- ExprString :: Expr -> String
-- ExprString (Expr e t) = show e --printf "%s^(%s)" (show e) (show t)

typeString :: Type -> String
typeString (Tvar v       ) = v
typeString (TInt         ) = "int"
typeString (t1     :=> t2) = printf "(%s) → %s" (show t1) (show t2)
typeString (Forall v   t ) = printf "∀%s.%s" (show v) (show t)
typeString (TTuple typs  ) = printf "<%s>" (flatten typs)

flatten []       = ""
flatten (e : es) = foldl (\a b -> a ++ ", " ++ show b) (show e) es

-------------------------------- Statics ---------------------------------------------
instance TSubtituable Type where
  (t⇖a) (Tvar tvar) = if tvar == a then t else (Tvar tvar)
  (t⇖a) TInt = TInt
  (t⇖a) (t1 :=> t2) = ((t⇖a) t1) :=> ((t⇖a) t2)
  (t⇖a) (Forall tvar t2) = if tvar == a then (Forall tvar t2) else (Forall tvar ((t⇖a) t2)) -- not sure
  (t⇖a) (TTuple es) = TTuple $ map (\x -> (t⇖a) x) es

ftv :: Type -> [TVar]
ftv (Tvar var)       = [var]
ftv TInt             = []
ftv (t1     :=>  t2) = ftv t1 ++ ftv t2
ftv (Forall tvar t ) = tvar : (ftv t)
ftv (TTuple es     ) = concatMap ftv es

typerule :: Delta -> Type -> Bool
typerule delta t = foldl (\a b -> a && elem b delta) True $ ftv t
-- a_static :: Delta -> Gamma -> Expr -> Type
-- a_static delta gamma (Expr e t) = if (static delta gamma e) == t then t else error "static error" -- todo type eq

static :: Delta -> Gamma -> Expr -> Type
static delta gamma (EVar id) = lookupTVar id gamma
static delta gamma (EInt i ) = TInt
static delta gamma (EFix x x1 t1 t2 e) =
  if typerule delta t1 && typerule delta t2 && static delta ((x1, t1) : (x, t1 :=> t2) : gamma) e == t2
    then t1 :=> t2
    else staticErr
static delta gamma (EApp e1 e2) = case static delta gamma e1 of
  (t1 :=> t2) -> if t1 == t1' then t2 else staticErr
  _           -> staticErr
  where t1' = static delta gamma e2
static delta gamma (ELambda a e) = Forall a t where t = static (a : delta) gamma e
static delta gamma (EInst   e t) = if typerule delta t
  then case static delta gamma e of
    (Forall a t') -> (t ⇖ a) t'
    _             -> staticErr
  else staticErr
static delta gamma (ETuple es) = TTuple ts where ts = map (static delta gamma) es
static delta gamma (EProj i e) = case static delta gamma e of
  TTuple ts -> if length ts > i then ts !! i else staticErr
  _         -> staticErr
static delta gamma (EBin binop e1 e2) =
  if static delta gamma e1 == TInt && static delta gamma e2 == TInt then TInt else staticErr
static delta gamma (EIfZ e1 e2 e3) = if static delta gamma e1 == TInt && t2 == static delta gamma e3
  then t2
  else staticErr
  where t2 = static delta gamma e2

-------------------------- Dynamics ------------------------------
data Val = VInt Int | VFix Expr | VΛ Expr
data Thunk = Value Val | Exp Expr

checkVal :: Expr -> Bool
checkVal (EInt i           ) = True
checkVal (EFix x x1 t1 t2 e) = True
checkVal (ELambda a e      ) = True
checkVal _                   = False

-- small_step :: Expr -> Thunk
-- small_step (EInt i) = Value (VInt i)
-- small_step (EFix x x1 t1 t2 e) = Value $ VFix $ EFix x x1 t1 t2 e
-- small_step (ELambda a e) = Value $ VΛ $ ELambda a e
-- small_step (EApp e1 e2) = 
--   case checkVal e1 of
--     True -> case checkVal e2 of
--       True ->  case e1 of
--         (EFix x x1 t1 t2 e) -> Exp e'
--         _                   -> error "dynamic error"
--       False -> Exp $ EApp e1 e2'
--     False -> Exp $ EApp e1' e2
--   where
--     e'      = tsubst e2 x1 e
--     Exp e2' = small_step e2
--     Exp e1' = small_step e1
-- small_step (EInst e t) = case e of
--   (ELambda a e1) -> tsubst t a e1
--   _              -> EInst e' t
--   where
--     (Exp e') = small_step e  
-- small_step (ETuple es) = undefined --if not (any (\ x -> checkVal x == False) es) then Val es else
-- --  ETuple $ foldl (\a b -> if fst a == False then (False,b:(snd a)) else (if checkVal b == False then (False, small_step b : (snd a)) else (True, b:(snd a)))  ) (True, []) es 
-- small_step (EProj i e) = undefined
-- small_step (EBin biop e1 e2) = undefined
-- small_step (EIfZ e1 e2 e3) = undefined
-- small_step (EVar id) = error "dynamic error"
