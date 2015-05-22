{-|
Module      : RCore.Trans
-}
module RCore.Invert where

import RCore.Ast

-- |Inverter
class Invert a where
    invert :: a -> a

instance Invert Program where
    invert (PDefs x cs y) = PDefs y (invert cs) x

instance Invert a => Invert [a] where
    invert cs = reverse (map invert cs)

instance Invert Cmd where
    invert c@(SAss _ _)   = c
    invert (SLoop e cs f) = SLoop f (invert cs) e
