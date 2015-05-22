{-|
Module      : RCore.Ast
-}
module RCore.Ast where

import RCore.Val
import Text.PrettyPrint

{-|
  Programs in P ::= read X; C^*; write X
-}
data Program = PDefs Ident [Cmd] Ident
               deriving (Eq,Ord,Show)

{-|
  Commands in C ::= X ^= E
                  | from X loop C until X
-}
data Cmd =
   SAss Ident Exp
 | SLoop Ident [Cmd] Ident
 | SAssert String Exp
  deriving (Eq,Ord,Show)

{-|
  Expressions in E, F ::= X | nil | cons E F | hd E | tl E | =? E F
-}
data Exp =
   AVar Ident
 | AVal Val
 | ACons Ident Ident
 | AHd Ident
 | ATl Ident
 | AEq Ident Ident
  deriving (Eq,Ord,Show)

-- |Environment
data Env = Env [(Ident,Val)]
           deriving (Show,Eq,Ord)

-- |Pretty printer
class Pretty a where
    pretty :: a -> Doc
    pretty' :: a -> Doc
    pretty' = error "in RCore.Ast.pretty'"

instance Pretty Val where
    pretty Nil        = text "nil"
    pretty (Cons e f) = lparen <> pretty e <> text "." <> pretty f <> rparen
    pretty (Atom s)   = text "'" <> text s

    pretty' = error "in RCore.Ast.pretty' 1"

instance Pretty Ident where
    pretty (Ident x) = text x

instance Pretty Exp where
    pretty (AVar x)    = pretty x
    pretty (AVal Nil)  = text "nil"
    pretty (ACons x y) = text "cons" <+> pretty x <+> pretty y
    pretty (AHd x)     = text "hd" <+> pretty x
    pretty (ATl x)     = text "tl" <+> pretty x
    pretty (AEq x y)   = text "=?" <+> pretty x <+> pretty y
    pretty (AVal (Cons _ _)) = error "impossible happens"
    pretty (AVal (Atom _)) = error "impossible happens"

    pretty' (AVar x)   = pretty x
    pretty' (AVal Nil) = text "nil"
    pretty' e          = parens (pretty e)

instance Pretty Cmd where
    pretty (SAss x e)     = pretty x <+> text "^=" <+> pretty e
    pretty (SLoop x cs y) = sep [text "from" <+> pretty x,
                                 if null cs then empty else text "loop" <+> pretty cs,
                                 text "until" <+> pretty y]

instance Pretty a => Pretty [a] where
    pretty es = sep (map pretty es)

instance Pretty Program where
    pretty (PDefs x cs y) = sep $ punctuate semi $ filter (not . isEmpty)
                               [text "read" <+> pretty x,
                                if null cs then empty else pretty cs,
                                text "write" <+> pretty y]

prettyEnv :: Env -> Doc
prettyEnv (Env es) = f es
    where
      f []               = empty
      f [(x,v)]          = pretty x <+> text ":=" <+> pretty v
      f ((x,v):ds@(_:_)) = pretty x <+> text ":=" <+> pretty v <> comma <+> f ds
