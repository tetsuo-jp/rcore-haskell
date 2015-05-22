{-|
Module      : RCore.Val
-}
module RCore.Val where

{-|
  DATA in D ::= nil | (D.D) | 'atom
-}
data Val = Nil
         | Cons Val Val
         | Atom String
           deriving (Eq,Ord,Show)

data Ident = Ident String
             deriving (Eq,Ord,Show)

ident :: Ident -> String
ident (Ident s) = s
