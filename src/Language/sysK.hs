{-# LANGUAGE OverloadedStrings #-}

module Language.SysK where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import           Language.Common

-------------------------------- Syntax ---------------------------------------------
data Type = Tvar TVar
          | TInt
          | Forall [TVar] [Type]
          | TTuple [Type]

-- instance Eq Type where
--     (Tvar a) == (Tvar b) = a == b
--     TInt == TInt = True
--     (Forall a1 t1) == (Forall a2 t2) = if a1 == a2 then t1 == t2 else tsubst t1 a1 (Tvar a2) == t2
--     (TTuple ts1) == (TTuple ts2) = ts1 == ts2
--     _ == _ = False

-- instance Subtituable Type where
--     tsubst t a (Tvar tvar) = if tvar == a then t else (Tvar tvar)
--     tsubst t a TInt = TInt
--     tsubst t a (Forall tvars ts) = if tvar == a then (Forall tvar t2) else (Forall tvar (tsubst t a t2)) -- not sure
--     tsubst t a (TTuple es) = TTuple $ map (\x -> tsubst t a x) es
