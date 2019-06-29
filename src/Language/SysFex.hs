{-# LANGUAGE OverloadedStrings #-}

module Language.SysFex where

import Language.SysF

fact :: Expr
fact = ((EFix "f" "n" TInt TInt ((EIfZ ((EVar "n")) ((EInt 1)) ((EBin Mul ((EVar "n") ) ((EApp ((EVar "f")) ((EBin Minus ((EVar "n")) ((EInt 1)))))) ))))))

