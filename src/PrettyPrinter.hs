-- PrettyPrinter del lenguaje parcialmente implementado.
module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t1 t2) = 
  text "Let " 
    <> pp ii vs t1
    <> text "in "
    <> pp ii vs t2 
pp ii vs (Zero) = text "Zero"
pp ii vs (Suc t) = 
    text "Suc "
        <> pp ii vs t
pp ii vs (Rec t1 t2 t3) = 
    text "R "
        <> pp ii vs t1
        <> pp ii vs t2
        <> pp ii vs t3
pp ii vs (Nil) = text "Nil"
pp ii vs (Cons t1 t2) = 
    text "Cons "
    <> pp ii vs t1
    <> text " "
    <> pp ii vs t2
pp ii vs (RecL t1 t2 t3) =
    text "RL "
    <> pp ii vs t1
    <> pp ii vs t2
    <> pp ii vs t3



isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType NatT = text "Nat"
printType ListT = text "List" 

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         )   = []
fv (Free  (Global n))   = [n]
fv (t   :@: u       )   = fv t ++ fv u
fv (Lam _   u       )   = fv u
fv (Let t1 t2       )   = fv t1 ++ fv t2
fv Zero                 = ["0"]
fv (Suc t1            ) = ["Suc "] ++ fv t1
fv (Rec t1 t2 t3      ) = fv t1 ++ fv t2
fv Nil                  = ["Nil"]
fv (Cons t1 t2        ) = ["Cons "] ++ fv t1 ++ [" "] ++ fv t2
fv (RecL t1 t2 t3     ) = fv t1 ++ fv t2
-- Preg.
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

