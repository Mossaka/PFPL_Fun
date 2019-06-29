{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Common where

type TVar = String
type EVar = String
type Id = String

class TSubtituable t where
    (⇖) :: t -> TVar -> t -> t
    infixr 5 ⇖

class ESubsituable e where
    (↖) :: e -> TVar -> e -> e
    infixr 5 ↖

staticErr = error "static error"
dynamicErr = error "dynamic error"


class HasGamma t where
    lookupTVar :: Id -> [(Id,t)] -> t
    lookupTVar x ((y, s) : env) | x == y    = s
                                | otherwise = lookupTVar x env
    lookupTVar x [] = error ("unbound type variable: " ++ x)

    extendGamma :: Id -> t -> [(Id,t)] -> [(Id,t)]
    extendGamma x s env = (x, s) : env

    inGamma :: Id -> [(Id,t)] -> Bool
    inGamma x [] = False
    inGamma x ((y, s) : env) | x == y    = True
                            | otherwise = inGamma x env
