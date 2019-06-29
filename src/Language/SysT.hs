{-# LANGUAGE OverloadedStrings #-}

module Language.SysT where

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
          | ERec Expr EVar EVar Expr Expr
          | ELam Type EVar Expr
          | EAp Expr Expr

type 𝚪 = [(Id, Type)]
instance HasGamma Type

instance IsString Expr where
    fromString = EVar

instance Show Expr where
    show = exprString

instance Show Type where
    show = typeString

exprString (EVar x)           = x
exprString E0                 = "z"
exprString (ES e            ) = printf "s(%s)" (show e)
exprString (ERec e0 x y e1 e) = printf "rec %s [z ↪ %s | s(%s) with %s ↪ %s]" (show e) (show e0) x y (show e1)
exprString (ELam t x e      ) = printf "λ(%s:%s)%s" (show t) x (show e)
exprString (EAp e1 e2       ) = printf "%s (%s)" (show e1) (show e2)

typeString ℕ           = "num"
typeString (t1 :=> t2) = printf "%s → %s" (show t1) (show t2)
-------------------------------- Statics ---------------------------------------------
instance ESubsituable Expr where
    (e↖x) es@(EVar x'           ) = if x' == x then e else es
    (e↖x) E0                      = E0
    (e↖x) es@(ES e'             ) = ES ((e↖x) e')
    (e↖x) es@(ERec e0 x' y e1 e') = ERec e0 x' y e1 ((e↖x) e')
    (e↖x) es@(ELam t x' e'      ) = if x == x' then es else ELam t x' ((e↖x) e')
    (e↖x) es@(EAp e1 e2         ) = EAp ((e↖x) e1) ((e↖x) e2)

(⊢) :: 𝚪 -> Expr -> Type
𝛄 ⊢ (EVar x)           = lookupTVar x 𝛄
𝛄 ⊢ E0                 = ℕ
𝛄 ⊢ (ES e            ) = if 𝛄 ⊢ e == ℕ then ℕ else staticErr
𝛄 ⊢ (ERec e0 x y e1 e) = if 𝛄 ⊢ e == ℕ && 𝛄'' ⊢ e1 == t then ℕ else staticErr
 where
  t   = 𝛄 ⊢ e0
  𝛄'  = (x, ℕ) : 𝛄
  𝛄'' = (y, t) : 𝛄
𝛄 ⊢ (ELam t x e) = t :=> (((x, t) : 𝛄) ⊢ e)
𝛄 ⊢ (EAp e1 e2 ) = case 𝛄 ⊢ e1 of
  t2 :=> t -> if 𝛄 ⊢ e2 == t2 then t else staticErr
  _        -> staticErr

{-@ 
admissibility :: 𝛄:𝚪 -> e:Expr -> {t:Type | static 𝛄 e == t} -> 
                x:EVar -> e':Expr -> 
                {t':Type | static ( (x,t) : 𝛄) e' == t'} -> 
                {𝛄 ⊢ ((e↖x) e') == t'} 
@-}


