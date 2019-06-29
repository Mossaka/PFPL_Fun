{-# LANGUAGE OverloadedStrings #-}

module Language.SysPCF_modality where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import           Language.Common
import           Debug.Trace

-------------------------------- Syntax ---------------------------------------------
data Type = ℕ | Type :=> Type | Comp Type

instance Eq Type where
    ℕ == ℕ = True
    (t1 :=> t2) == (t3 :=> t4) = t1 == t3 && t2 == t4
    (Comp t1) == (Comp t2) = t1 == t2
    _ == _ = False

data Expr = EIfz Expr EVar Expr Value
          | EAp Value Value
          | ERet Value
          | EBnd Value EVar Expr

data Value = EVar EVar
           | E0
           | ES Value
           | ELam Type EVar Expr
           | EComp Expr

type 𝚪 = [(Id, Type)]
instance HasGamma Type

instance IsString Value where
    fromString = EVar

instance Show Expr where
    show = exprString

instance Show Value where
    show = valueString

instance Show Type where
    show = typeString

exprString (EIfz e0 x e1 e) = printf "ifz %s [z ↪ %s | s(%s) ↪ %s]" (show e) (show e0) x (show e1)
exprString (EAp e1 e2     ) = printf "%s (%s)" (show e1) (show e2)
exprString (ERet v        ) = printf "ret (%s)" (show v)
exprString (EBnd v x e    ) = printf "let (%s ← %s ; %s)" (show x) (show v) (show e)

valueString (EVar x)     = x
valueString E0           = "0"
valueString (ES e      ) = printf "%d" (v2int e + 1)
valueString (ELam t x e) = printf "λ(%s:%s)%s" x (show t) (show e)
valueString (EComp e   ) = printf "%s comp" (show e)

typeString ℕ           = "num"
typeString (t1 :=> t2) = printf "%s → %s" (show t1) (show t2)
typeString (Comp t) = printf "%s comp" (show t)

v2int :: Value -> Int
v2int E0 = 0
v2int (ES v) = v2int v + 1
v2int _ = error "cannot convert other type to int except ES or E0"

int2v :: Int -> Value
int2v n | n == 0 = E0
        | n > 0 = ES (int2v (n-1))
        | otherwise = error "cannot convert negative int to value"
-------------------------------- Statics ---------------------------------------------
-- instance ESubsituable Expr where
(/↖) :: Value -> EVar -> Expr -> Expr
(e/↖x) es@(EIfz e0 x' e1 v  ) = if x' == x then EIfz ((e/↖x) e0) x' e1 ((e|↖x) v)
                                else EIfz ((e/↖x) e0) x' ((e/↖x) e1) ((e|↖x) v)
(e/↖x) es@(EAp e1 e2         ) = EAp ((e|↖x) e1) ((e|↖x) e2)
(e/↖x) es@(ERet v            ) = ERet $ (e|↖x) v
(e/↖x) es@(EBnd b x' e'      ) = if x == x' then EBnd ((e|↖x) b) x' e' else EBnd ((e|↖x) b) x' ((e/↖x) e')
(|↖) :: Value -> EVar -> Value -> Value
(e|↖x) es@(EVar x'           ) = if x' == x then e else es
(e|↖x) E0                      = E0
(e|↖x) es@(ES e'             ) = ES ((e|↖x) e')
(e|↖x) es@(ELam t x' e'      ) = if x == x' then es else ELam t x' ((e/↖x) e')
(e|↖x) es@(EComp e'          ) = EComp ((e/↖x) e')

(|⊢) :: 𝚪 -> Value -> Maybe Type
𝛄 |⊢ (EVar x)         = return $ lookupTVar x 𝛄
𝛄 |⊢ E0               = return ℕ
𝛄 |⊢ (ES v          ) = do
    ℕ <- 𝛄 |⊢ v
    return ℕ
𝛄 |⊢ (ELam t x e    ) = do 
    t2 <- ((x, t) : 𝛄) ⊢ e
    return $ t :=> t2
𝛄 |⊢ (EComp e'      ) = do 
    t <- 𝛄 ⊢ e'
    return (Comp t)

(⊢) :: 𝚪 -> Expr -> Maybe Type
𝛄 ⊢ (EIfz e0 x e1 v) = do
    ℕ <- 𝛄 |⊢ v
    t <- 𝛄 ⊢ e0
    t'<- ((x,ℕ) : 𝛄) ⊢ e1
    if t /= t' then Nothing else return t
𝛄 ⊢ (EAp e1 e2 ) = do
    (t2 :=> t) <- 𝛄 |⊢ e1 
    t2' <- 𝛄 |⊢ e2 
    if t2' == t2 then return t else Nothing
𝛄 ⊢ (EBnd v x e) = do 
    Comp t <- 𝛄 |⊢ v
    ((x,t) : 𝛄) ⊢ e
𝛄 ⊢ (ERet v) = 𝛄 |⊢ v
    
---------------------------- dynamics ---------------------------------------------
data Thunk = Expr Expr | Err | Done
instance Show Thunk where
    show (Expr e) = show e
    show Err      = "dynamic error"
    show Done     = ">>>>>Done<<<<<"

trane :: Expr -> Thunk
trane (ERet e) = Done
trane (EIfz e0 x e1 E0    ) = Expr e0
trane (EIfz e0 x e1 (ES v)) = Expr $ (v /↖ x) e1
trane (EAp (ELam t x e) v)  = Expr $ (v /↖ x) e
trane (EBnd (EComp (ERet v)) x e) = Expr $ (v /↖ x) e
trane (EBnd (EComp e1) x e2) = case trane e1 of
    Expr e1' -> Expr $ EBnd (EComp e1') x e2
trane _ = Err


-- -------------------------------- demo ---------------------------------------------
one = ES E0
two = ES one
three = ES two


ex1 = EIfz (ERet three) "x" (ERet "x") two

-- λ(y:num)ifz y [z ↪ ret (3) | s(x) ↪ ret (x)]
ex2 = ELam ℕ "y" (EIfz (ERet three) "x" (ERet "x") "y") 

-- let ("x" ← ret (3) comp ; λ(y:num)ifz y [z ↪ ret (3) | s(x) ↪ ret (x)] (x))
ex4 = EBnd (EComp (ERet three)) "x" (EAp ex2 "x") 

 -- λ(y:num comp)let ("a" ← y ; ifz a [z ↪ ret (3) | s(x) ↪ ret (x)])
ex5 = ELam (Comp ℕ) "y" (EBnd "y" "a" (EIfz (ERet three) "x" (ERet "x") "a")) 

-- λ(y:num comp)let ("a" ← y ; ifz a [z ↪ ret (3) | s(x) ↪ ret (x)]) (ret (3) comp)
ex6 = EAp ex5 (EComp (ERet three)) 

-- λ(y:num comp)let ("a" ← y ; ifz a [z ↪ ret (3) | s(x) ↪ ret (x)]) (ifz 3 [z ↪ ret (1) | s(x) ↪ ret (x)] comp)
ex7 = EAp ex5 (EComp (EIfz (ERet one) "x" (ERet "x") three))

-- λ(y:num comp)let ("z" ← ret (y) comp ; ret (z))
ex8 = ELam (Comp ℕ) "y" (EBnd (EComp (ERet "y")) "z" (ERet "z"))

ex9 = EAp ex8 (EComp (ERet three))

-- ret (ret (3) comp)
ex10 = ERet (EComp (ERet (int2v 100)))

eval :: Expr -> IO ()
eval e = case [] ⊢ e of
    Nothing -> print ">>>static type error!<<<"
    t       -> do
        putStrLn $ show e ++ " :: " ++ show ([] ⊢ e)
        go [] e
go env e =  case trane e of
    Expr e' -> do
        putStrLn $ show e' ++ " :: " ++ show (env ⊢ e')
        go env e'
    Err -> print Err
    Done -> print Done

