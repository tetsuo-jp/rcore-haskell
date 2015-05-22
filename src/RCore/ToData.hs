{-|
Module      : RCore.ToData
-}
module RCore.ToData where

import RCore.Val
import RCore.Ast
import Text.PrettyPrint

import Debug.Trace

nils :: Int -> Val
nils n = foldr Cons Nil $ replicate n Nil

class ToData a where
    todata :: a -> Val
          
instance ToData Ident where
    todata (Ident ('X':i)) = var (nils (read i))
        where var e = Cons (Atom "var") e
    todata _ = error "impossible happened in RCore.ToData.Ident.todata"

instance ToData Program where
    todata (PDefs xi cs xj) = 
        Cons (todata xi) (Cons (todata cs) (todata xj))

instance ToData a => ToData [a] where
    todata [] = todata (SAss (Ident "X0") (AVal Nil))
    todata cs = foldr1 (\x y -> Cons (Atom "seq") (Cons x y)) (map todata cs)

instance ToData Cmd where
    todata (SAss xi e) = Cons (Atom "xoreq") (Cons (todata xi) (todata e))
    todata (SLoop xi cs xj) =
        Cons (Atom "loop") (foldr1 Cons [todata xi, todata cs, todata xj])

instance ToData Exp where
    todata (AVar xi)     = todata xi
    todata (AVal Nil)    = Atom "nil"
    todata (AVal v)      = Cons (Atom "val") v
    todata (ACons xi xj) = Cons (Atom "cons") (foldr1 Cons [todata xi, todata xj])
    todata (AHd xi)      = Cons (Atom "hd") (todata xi)
    todata (ATl xi)      = Cons (Atom "tl") (todata xi)
    todata (AEq xi xj)   = Cons (Atom "eq") (Cons (todata xi) (todata xj))
    -- todata _             = error "impossible happened in RCore.ToData.Exp.todata"
