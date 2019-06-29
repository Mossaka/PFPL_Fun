{-# LANGUAGE OverloadedStrings #-}

module Language.SysE where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import           Language.Common
import           Debug.Trace

-------------------------------- Syntax ---------------------------------------------
data Type = ℕ | Str
data Expr = EVar EVar
          | ENum Int
          | EStr String
          | EPlus Expr Expr
          | ETimes Expr Expr
          | ECat Expr Expr
          | ELen Expr
          | ELet Expr EVar Expr
    deriving (Eq)

instance Eq Type where
    ℕ == ℕ = True
    Str == Str = True
    _   == _   = False

type Gamma = [(Id, Type)]
instance HasGamma Type

instance IsString Expr where
    fromString = EVar

instance Show Expr where
    show = exprString

instance Show Type where
    show = typeString

exprString (EVar x      ) = x
exprString (ENum i      ) = printf "%d" i
exprString (EStr s      ) = s
exprString (EPlus  e1 e2) = printf "%s + %s" (show e1) (show e2)
exprString (ETimes e1 e2) = printf "%s * %s" (show e1) (show e2)
exprString (ECat   e1 e2) = printf "%s ^ %s" (show e1) (show e2)
exprString (ELen e      ) = printf "|%s|" (show e)
exprString (ELet e1 x e2) = printf "let %s be %s in %s" x (show e1) (show e2)

typeString ℕ   = "num"
typeString Str = "str"

-------------------------------- Statics ---------------------------------------------

instance ESubsituable Expr where
    (e↖x) es@(EVar x'      ) = if x' == x then e else es
    (e↖x) es@(ENum i       ) = es
    (e↖x) es@(EStr s       ) = es
    (e↖x) es@(EPlus  e1 e2 ) = EPlus ((e↖x) e1) ((e↖x) e2)
    (e↖x) es@(ETimes e1 e2 ) = ETimes ((e↖x) e1) ((e↖x) e2)
    (e↖x) es@(ECat   e1 e2 ) = ECat ((e↖x) e1) ((e↖x) e2)
    (e↖x) es@(ELen e'      ) = ELen ((e↖x) e')
    (e↖x) es@(ELet e1 x' e2) = if x == x' then ELet e x e2 else ELet e1 x' ((e↖x) e2)

static :: Gamma -> Expr -> Type
static env (EVar x      ) = lookupTVar x env
static env (ENum i      ) = ℕ
static env (EStr s      ) = Str
static env (EPlus  e1 e2) = if static env e1 == ℕ && static env e2 == ℕ then ℕ else staticErr
static env (ETimes e1 e2) = if static env e1 == ℕ && static env e2 == ℕ then ℕ else staticErr
static env (ECat   e1 e2) = if static env e1 == Str && static env e2 == Str then Str else staticErr
static env (ELen e      ) = if static env e == Str then ℕ else staticErr
static env (ELet e1 x e2) = t2
 where
  t1 = static env e1
  t2 = static (extendGamma x t1 env) e2


{-@ weakening :: env:Gamma -> e:Expr -> t':Type -> {x:TVar | inGamma x == False} 
                           -> t:Type -> { static env e == t' => static (extendGamma x t env) e == t'}   @-}

{-@ substitution :: env:Gamma -> x:TVar -> t:Type -> e':Expr -> t':Type -> e:Expr -> {t:Type | static gamma e == t} 
                              -> { static gamma (subst e x e') == t' } @-}

-------------------------------- dynamics ---------------------------------------------
data Thunk = Val Expr | Expr Expr
    deriving (Eq)

instance Show Thunk where
    show (Val e) = "Val: " ++ show e
    show (Expr e) = show e

checkVal :: Expr -> Bool
checkVal (ENum _) = True
checkVal (EStr _) = True
checkVal _        = False

dynamics :: Expr -> Thunk
dynamics (ENum i                   ) = Val (ENum i)
dynamics (EStr s                   ) = Val (EStr s)
dynamics (EPlus (ENum n1) (ENum n2)) = Expr $ ENum $ n1 + n2
dynamics (EPlus e1        e2       ) = case checkVal e1 of
  True  -> Expr $ EPlus e1 e2'
  False -> Expr $ EPlus e1' e2
 where
  Expr e1' = dynamics e1
  Expr e2' = dynamics e2
dynamics (ELet e1 x e2) = case checkVal e1 of
  True -> case checkVal e2' of
    True  -> Val e2'
    False -> Expr e2'
  False -> Expr $ ELet e1' x e2
 where
  Expr e1' = dynamics e1
  e2'      = (e1 ↖ x) e2

dynamic _ = error "not implemented"

-------------------------------- type safety ---------------------------------------------
{-@ preservation :: env:Gamma -> e:Expr -> {t:Type | static env e == t} -> {e':Expr | dynamic e == e'} -> { static env e' == t } @-}

-- {-@ progress :: env:Gamma -> e:Expr -> {t:Type | static env e == t} -> { checkVal e == True || dynamics e } @-}

-------------------------------- demo ---------------------------------------------
ex1 = ELet (EPlus (ENum 1) (ENum 2)) "x" (EPlus (EPlus (EVar "x") (ENum 3)) (ENum 4))

eval :: Expr -> IO ()
eval e = case dynamics e of
  Expr e -> do
    print e
    eval e
  Val v -> print $ "Val: " ++ (show v)


