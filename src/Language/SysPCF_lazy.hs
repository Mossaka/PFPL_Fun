{-# LANGUAGE OverloadedStrings #-}

module Language.SysPCF_lazy where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import           Language.Common
import           Debug.Trace

-------------------------------- Syntax ---------------------------------------------
data Type = ℕ | Type :=> Type

instance Eq Type where
    ℕ == ℕ = True
    (t1 :=> t2) == (t3 :=> t4) = t1 == t3 && t2 == t4
    _ == _ = False

data Expr = EVar EVar
          | E0
          | ES Expr
          | EIfz Expr EVar Expr Expr
          | ELam Type EVar Expr
          | EAp Expr Expr
          | EFix Type EVar Expr

type 𝚪 = [(Id, Type)]
instance HasGamma Type

instance IsString Expr where
    fromString = EVar

instance Show Expr where
    show = exprString

instance Show Type where
    show = typeString

exprString (EVar x)         = x
exprString E0               = "z"
exprString (ES e          ) = printf "s(%s)" (show e)
exprString (EIfz e0 x e1 e) = printf "ifz %s [z ↪ %s | s(%s) ↪ %s]" (show e) (show e0) x (show e1)
exprString (ELam t x e    ) = printf "λ(%s:%s)%s" (show t) x (show e)
exprString (EAp e1 e2     ) = printf "%s (%s)" (show e1) (show e2)
exprString (EFix t x e    ) = printf "fix %s:%s is %s" (show x) (show t) (show e)

typeString ℕ           = "num"
typeString (t1 :=> t2) = printf "%s → %s" (show t1) (show t2)

-------------------------------- Statics ---------------------------------------------
instance ESubsituable Expr where
    (e↖x) es@(EVar x'           ) = if x' == x then e else es
    (e↖x) E0                      = E0
    (e↖x) es@(ES e'             ) = ES ((e↖x) e')
    (e↖x) es@(EIfz e0 x' e1 e'  ) = if x' == x then EIfz ((e↖x) e0) x' e1 ((e↖x) e')
                                    else EIfz ((e↖x) e0) x' ((e↖x) e1) ((e↖x) e')
    (e↖x) es@(ELam t x' e'      ) = if x == x' then es else ELam t x' ((e↖x) e')
    (e↖x) es@(EAp e1 e2         ) = EAp ((e↖x) e1) ((e↖x) e2)
    (e↖x) es@(EFix t x' e'      ) = if x == x' then es else EFix t x' ((e↖x) e')

(⊢) :: 𝚪 -> Expr -> Type
𝛄 ⊢ (EVar x)         = lookupTVar x 𝛄
𝛄 ⊢ E0               = ℕ
𝛄 ⊢ (ES e          ) = if 𝛄 ⊢ e == ℕ then ℕ else staticErr
𝛄 ⊢ (EIfz e0 x e1 e) = if 𝛄 ⊢ e == ℕ && 𝛄' ⊢ e1 == t then t else staticErr
 where
  t  = 𝛄 ⊢ e0
  𝛄' = (x, ℕ) : 𝛄
𝛄 ⊢ (ELam t x e) = t :=> (((x, t) : 𝛄) ⊢ e)
𝛄 ⊢ (EAp e1 e2 ) = case 𝛄 ⊢ e1 of
  t2 :=> t -> if 𝛄 ⊢ e2 == t2 then t else staticErr
  _        -> staticErr
𝛄 ⊢ (EFix t x e) = ((x, t) : 𝛄) ⊢ e

-------------------------------- dynamics ---------------------------------------------
data Thunk = Val Expr | Expr Expr
instance Show Thunk where
    show (Val e) = "Val: " ++ show e
    show (Expr e) = show e

isVal :: Expr -> Bool
isVal E0           = True
isVal (ES e      ) = True
isVal (ELam t x e) = True
isVal _            = False

toThunk :: Expr -> Thunk
toThunk e = if isVal e then Val e else Expr e

fromThunk :: Thunk -> Expr
fromThunk (Val  e) = e
fromThunk (Expr e) = e

transition :: Expr -> Thunk
transition (EIfz e0 x e1 E0    ) = toThunk e0
transition (EIfz e0 x e1 (ES e)) = case isVal e of
  True -> toThunk $ (e ↖ x) e1
  _    -> dynamicErr
transition (EIfz e0 x e1 e     ) = toThunk $ EIfz e0 x e1 (fromThunk $ transition e)
transition (EAp (ELam t x e) e2) = toThunk $ (e2 ↖ x) e
transition (EAp e1           e2) = toThunk $ EAp (fromThunk $ transition e1) e2
transition (EFix t x e         ) = toThunk $ ((EFix t x e) ↖ x) e
transition e                     = if isVal e then toThunk e else dynamicErr

-------------------------------- demo ---------------------------------------------
one = ES E0
two = ES one
three = ES two
ex1 = EIfz three "x" (EVar "x") E0
ex2 = ELam ℕ "y" (EIfz three "x" (EVar "x") (EVar "y"))
ex3 = EFix (ℕ :=> ℕ) "f" (ELam ℕ "y" (EIfz E0 "x" (EAp (EVar "f") (EVar "x")) (EVar "y")))
ex4 = EAp ex3 three
eval :: Expr -> IO ()
eval e = case transition e of
  Expr e' -> do
    print e'
    eval e'
  Val v -> print $ "Val: " ++ (show v)
