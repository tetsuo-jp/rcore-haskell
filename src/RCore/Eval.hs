{-|
Module      : RCore.Eval
-}
module RCore.Eval where
import RCore.Ast
import Data.Maybe (fromMaybe)
import Text.PrettyPrint
import RCore.Val

lookupEnv :: Ident -> Env -> Val
lookupEnv x@(Ident s) (Env ev) =
    fromMaybe (error ("in lookupEnv: Variable " ++ s ++ " not found"))
              (lookup x ev)

update :: Ident -> Val -> Env -> Env
update x v (Env ev) = Env (update' x v ev)
  where update' (Ident s) _ [] = error ("in update: Variable " ++ s ++ " is not found")
        update' z v' ((y,w) : ev') =
            if z == y then (y,v')  : ev'
                      else (y,w) : update' z v' ev'

minus :: Env -> Ident -> Env
minus (Env ev) x = Env (minus' ev x)
    where minus' []            _ = []
          minus' ((y,v) : ev') z = if x == y then minus' ev' z
                                  else (y,v) : minus' ev' z

initEnv :: [Ident] -> Env
initEnv = Env . map (\z -> (z,Nil))

allNil :: Env -> Bool
allNil (Env cs) = all (==Nil) (map snd cs)

-- |Evaluator
eval :: Env -> Exp -> Val
eval ev (AVar x) = lookupEnv x ev
eval _  (AVal Nil) = Nil
eval ev (ACons x y) = Cons (lookupEnv x ev) (lookupEnv y ev)
eval ev (AHd x) = case lookupEnv x ev of
                    Cons v _ -> v
                    Nil      -> error ("hd nil: " ++ show (AHd x) ++ "\n" ++ show ev)
eval ev (ATl x) = case lookupEnv x ev of
                    Cons _ v -> v
                    Nil      -> error "tl nil"
eval ev (AEq x y) = if v1 == v2 then Cons Nil Nil else Nil
    where v1 = lookupEnv x ev
          v2 = lookupEnv y ev

-- |Execution of commands
exec :: Env -> Cmd -> Env
exec ev (SAss x@(Ident s) e) =
    let v' = eval (ev `minus` x) e in
    case lookupEnv x ev of
      Nil          -> update x v' ev
      v@(Atom _)   -> if v == v'
                      then update x Nil ev
                      else if v' == Nil
                           then ev
                           else error ("Variable " ++ s ++ " does not match: " ++ show ev ++ "\n(v,v')=" ++ show v ++ ","  ++ show v')
      v@(Cons _ _) -> if v == v'
                      then update x Nil ev
                      else if v' == Nil
                           then ev
                           else error ("Variable " ++ s ++ " does not match: " ++ show ev ++ "\n(v,v')=" ++ show v ++ ","  ++ show v')
exec ev (SLoop x1 cs x2)
  | lookupEnv x1 ev /= Nil = loop ev (x1,cs,x2)
  | otherwise              = error ("Assertion failed on entry of loop: " ++ show (SLoop x1 cs x2))

clear :: Env -> [Ident] -> Env
clear (Env evs) vs = Env (map f evs)
    where f (x,v) | x `elem` vs = (x,Nil)
                  | otherwise   = (x,v)

loop :: Env -> (Ident,[Cmd],Ident) -> Env
loop ev1 (x1,cs,x2) =
    if lookupEnv x2 ev1 /= Nil
    then ev1
    else let ev2 = foldl exec ev1 cs
         in if lookupEnv x1 ev2 /= Nil
            then error ("Assertion failed in loop: " ++ show x1 ++ ", " ++ show ev2)
            else loop ev2 (x1,cs,x2)

execProgram :: Program -> Val -> Val
execProgram prog@(PDefs x cs y) v =
    let ev0 = update x v (initEnv (vars prog))
        ev1 = foldl exec ev0 cs
    in if allNil (ev1 `minus` y)
       then lookupEnv y ev1
       else error $ show ("Some vars are not Nil: " ++ show ev1)

-- |Variables, the increasing order, no duplication
class Vars a where
    vars :: a -> [Ident]

instance Vars Program where
    vars (PDefs x cs y) = insert x (insert y (vars cs))

instance Vars a => Vars [a] where
    vars = foldr merge [] . map vars

instance Vars Cmd where
    vars (SAss x e)       = insert x (vars e)
    vars (SLoop x1 cs x2) = insert x1 $ insert x2 (vars cs)

instance Vars Exp where
    vars (AVar x)    = [x]
    vars (AVal Nil)  = []
    vars (ACons x y) = [x,y]
    vars (AHd x)     = [x]
    vars (ATl x)     = [x]
    vars (AEq x y)   = [x,y]

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) | x == y    = merge xs (y : ys)
                    | x < y     = x : merge xs (y : ys)
                    | otherwise = y : merge (x : xs) ys

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x == y    = insert x ys
                | x < y     = x : y : ys
                | otherwise = y : insert x ys

