-- Tiene las funciones que hacen funcionar al intérprete y el inferidor de tipos,
-- ambos parcialmente implementados.
module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion lt = conversion' lt [] 

-- auxiliar que mantiene la lista de las variables de ligadura.
conversion' :: LamTerm -> [String] -> Term
conversion' lt xs = case lt of
                        LVar s                -> bound_var s xs 0
                        LApp lt1 lt2          -> conversion' lt1 xs :@: conversion' lt2 xs
                        LAbs s t lt1          -> Lam t (conversion' lt1 (s:xs))  
                        LLet s t1 t2          -> Let (conversion' t1 xs) (conversion' t2 xs)

-- Bound_var lleva la cuenta de la ligadura de la variable.
bound_var :: String -> [String] -> Int -> Term
bound_var   n [] _      = Free (Global n) 
bound_var   n (s:xs) i  | n == s = Bound i
                        | otherwise = bound_var n xs (i+1)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let t1 (sub i t t2) 

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado   --- PREG.
-- La idea es seguir "semánticamente" las reglas de evaluación.
-- No es posible hacer el esquema de reglas literalmente ya que las reglas presentan
-- la rel. de evaluacion de paso chico y acá hacemos las big-step.
-- Entonces seguimos la idea en general de, por ej., en caso de aplicación,
-- evaluar primero t1, luego t2 y sustituir.
eval :: NameEnv Value Type -> Term -> Value
eval l@((n, (v,ty)):xs) te =  case te of
                                Bound i     -> error "eval error - Bound is not a possible entry"
                                Free n1     -> if n == n1 then v else eval xs (Free n1) -- usar lookup 
                                (t1 :@: t2) ->  let v1 = eval l t1 
                                                    v2 = eval l t2
                                                in case v1 of     
                                                    VLam t' term -> let Lam t'' term' = sub 0 (quote v2) (quote v1)
                                                                    in VLam t'' term'
                                                    _            -> error "eval error - v1 not a Lam value"
                                Lam t' term  -> VLam t' term
                                Let t1 t2    -> let v1 = eval l t1 
                                                    Lam t' term = sub 0 (quote v1) t2 
                                                in VLam t' term 
----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t1 t2) = case infer' c e t1 of
                            Left s    -> Left s 
                            Right ty  -> infer' (ty:c) e t2  -- Habría que agg a NameEnv? 
