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
                        LLet s t1 t2          -> Let (conversion' t1 xs) (conversion' t2 (s:xs)) -- Como la variable s desaparece (sin nombres) se la agrego como ligada en t2,
                        LZero                 -> Zero                                            -- - (cont. let) - es por eso que en sub le sumo 1 evaluando t2.
                        LSuc t                -> Suc (conversion' t xs)
                        LRec t1 t2 t3         -> Rec (conversion' t1 xs) (conversion' t2 xs) (conversion' t3 xs)
                        LNil                  -> Nil 
                        LCons t1 t2           -> Cons (conversion' t1 xs) (conversion' t2 xs)
                        LRecL t1 t2 t3        -> RecL (conversion' t1 xs) (conversion' t2 xs) (conversion' t3 xs)

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
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i + 1) t t2) -- let x = 3 in x -> (\x. x) 3 -> entonces me voy metiendo adentro del let.
sub i t Zero                  = Zero 
sub i t (Suc t1)              = Suc (sub i t t1)
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)
sub i t Nil                   = Nil
sub i t (Cons n xs)           = Cons (sub i  t n) (sub i t xs)
sub i t (RecL t1 t2 t3)       = RecL (sub i t t1) (sub i t t2) (sub i t t3) 

-- Cons 1 (Cons .. -> (\x. \y Cons x y) 1 (Cons ...)
-- \x.\y.\z Rec x y z 
--
-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (VNum n)   = case n of 
                    NZero -> Zero
                    NSuc v -> Suc (qn v)
quote (VList n)   = case n of 
                      VNil        -> Nil
                      VCons t1 t2 -> Cons (qn t1) (ql t2)
qn :: NumVal -> Term
qn NZero       = Zero 
qn (NSuc v')   = Suc (qn v')

ql :: ListVal -> Term
ql VNil        = Nil 
ql (VCons a b) = Cons (qn a) (ql b)


-- evalúa un término en un entorno dado   --- PREG.
-- La idea es seguir "semánticamente" las reglas de evaluación.
-- No es posible hacer el esquema de reglas literalmente ya que las reglas presentan
-- la rel. de evaluacion de paso chico y acá hacemos las big-step.
-- Entonces seguimos la idea en general de, por ej., en caso de aplicación,
-- evaluar primero t1, luego t2 y sustituir.
eval :: NameEnv Value Type -> Term -> Value
eval l te =  case te of
                Bound i     -> error "eval error - Bound is not a possible entry"
                Free n1     -> case lookup n1 l of
                                    Nothing -> error "eval - free var not found" 
                                    Just (v,_) -> v
                (t1 :@: t2) ->  let v1 = eval l t1  
                                in case v1 of     
                                    VLam t' term -> let v2 = eval l t2 
                                                    in  let s = sub 0 (quote v2) term 
                                                        in eval l s 
                                    _            -> error "eval error - v1 not a Lam value"
                Lam t' term  -> VLam t' term
                Let t1 t2    -> let v1 = eval l t1 
                                    Lam t' term = sub 0 (quote v1) t2 
                                in VLam t' term
                Zero         -> VNum NZero
                Suc t1       -> case eval l t1 of
                                VNum n -> VNum (NSuc n) 
                                _      -> error "eval error - bad type in value suc"
                Rec t1 t2 t3 -> case t3 of 
                                    Zero    -> eval l t1    -- E-RZero
                                    Suc t  -> eval l ((t2 :@: (Rec t1 t2 t)) :@: t) -- E-RSucc
                                    _       ->  let v3 = eval l t3  -- Resuelvo R en caso que t3 sea Rec
                                                in eval l (Rec t1 t2 (quote v3))
                Nil          -> VList VNil
                Cons t1 t2   -> let v1 = (eval l t1)
                                in case v1 of
                                      VNum s -> let v2 = (eval l (Cons (quote v1) t2))
                                                in case v2 of
                                                      VList s'  -> VList (VCons s s')
                                                      _         -> error "eval error - v2 not Vlist value"
                                      _      -> error "eval error - v1 not Vnum value"
                RecL t1 t2 t3 -> case t3 of 
                                    Nil         -> eval l t1 
                                    (Cons n lv) -> eval l (((t2 :@: n) :@: lv) :@: (Rec t1 t2 lv))
                                    _           ->  let v3 = eval l t3  -- Resuelvo RL en caso que t3 sea RecL
                                                    in eval l (Rec t1 t2 (quote v3))
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
                            Right ty  -> infer' (ty:c) e t2 
infer' c e (Zero)      = Right NatT
infer' c e (Suc t)     = case infer' c e t of
                            Right NatT -> Right NatT
                            Left s    -> Left s
                            _         -> error "infer' - t in Suc t bad typed"
infer' c e (Rec t1 t2 t3) = let ty1 = infer' c e t1
                                ty2 = infer' c e t2 
                                ty3 = infer' c e t3
                            in case ty2 of
                                Right (FunT t' (FunT NatT t)) -> if t == t' && ty1 == Right t && ty3 == Right NatT then Right t
                                                                    else case ty1 of
                                                                            Left s -> Left s
                                                                            _      -> case ty3 of
                                                                                        Left j  -> Left j
                                                                                        _       -> error "infer' - bad type Rec in t1 or t3"
                                Left s -> Left s
                                _      -> error "infer' - bad type Rec in t2"
infer' c e (Nil)  = Right ListT
infer' c e (Cons t1 t2) = case infer' c e t1 of
                            Right NatT -> case infer' c e t2 of
                                            Right (ListT) -> Right ListT
                                            Left s -> Left s
                                            _      -> error "infer' - t2 has bad type  in Cons t1 t2"
                            Left s  -> Left s
                            _       -> error "infer' - t1 has a bad type in Cons t1 t2"
infer' c e (RecL t1 t2 t3) =  let ty1 = infer' c e t1
                                  ty2 = infer' c e t2 
                                  ty3 = infer' c e t3
                              in case ty2 of
                                    Right (FunT NatT (FunT ListT (FunT t t'))) -> if t == t' && ty1 == Right t' && ty3 == Right ListT then Right t
                                                                                  else case ty1 of
                                                                                          Left s -> Left s
                                                                                          _      -> case ty3 of
                                                                                                      Left j  -> Left j
                                                                                                      _       -> error "infer' - bad type Rec in t1 or t3"
                                    Left s -> Left s
                                    _      -> error "infer' - bad type Rec in t2"
