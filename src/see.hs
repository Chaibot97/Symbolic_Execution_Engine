module Main (main) where

import Language
import Parser.Parser ( parseProg )

import Data.List ((\\),  intercalate, filter, notElem, foldl, concatMap, intersperse, intersperse, )
import qualified Data.Set as S (intersection,  Set, empty, singleton, union, difference, fromList, toList, filter, map, foldl ) 
import Text.Printf ( printf )
import System.Environment ( getArgs, )
import Debug.Trace ( trace )


-- Convert BExp to Assertion
bexp2assert :: BExp -> Assertion
bexp2assert BTrue = ATrue
bexp2assert BFalse = AFalse
bexp2assert (BCmp c) = ACmp c
bexp2assert (BNot b) = ANot (bexp2assert b)
bexp2assert (BBinOp op b1 b2) = ABinOp op b1' b2' where
  b1' = bexp2assert b1
  b2' = bexp2assert b2


-- Remove while loops
rmWhile :: AST -> AST
rmWhile (While c b) = Skip
rmWhile (If c tb fb) = If c (rmWhile tb) (rmWhile fb)
rmWhile (Seq s s') = Seq (rmWhile s) (rmWhile s')
rmWhile s = s


-- Unroll each while loop once
unrollOnce :: AST -> AST
unrollOnce (If c tb fb) = If c (unrollOnce tb) (unrollOnce fb)
unrollOnce (Seq b1 b2) = Seq (unrollOnce b1) (unrollOnce b2)
unrollOnce s@(While c b) =
    If c (Seq b' (While c b')) Skip where
      b' = unrollOnce b
unrollOnce s = s


-- Unroll each while loop n times
unroll :: Int -> AST -> AST
unroll n s = if n == 0 then rmWhile s else unroll (n-1) (unrollOnce s)


-- A sequence of increasingly unrolled programs from 0 to n
unroll_seq :: Int -> AST -> [AST]
unroll_seq n s = map (\i -> unroll i s) [0..n]


-- Things we need to keep track of during symbolic execution
type Guard = [Assertion]
type SymbolicValue = AExp
type State = (Name, Type) -> SymbolicValue


-- Interpret an AExp using the symbolic values in the current state
-- Basically, replace non-symbolic names with symbolic values
evalAExp :: AExp -> State -> AExp
evalAExp (Num n) _ = Num n
evalAExp (Var (x,t)) s = s (x,t)
evalAExp (BinOp op e1 e2) s = BinOp op (evalAExp e1 s) (evalAExp e1 s)
evalAExp (Read ea ei) s = Read (evalAExp ea s) (evalAExp ei s)
evalAExp (Store ea ei ev) s = Store (evalAExp ea s) (evalAExp ei s) (evalAExp ev s)

-- Interpret a BExp using the symbolic values in the current state
-- Basically, replace non-symbolic names with symbolic values
evalBExp :: BExp -> State -> BExp 
evalBExp (BCmp (Comp ord e1 e2)) s = BCmp (Comp ord (evalAExp e1 s) (evalAExp e2 s))
evalBExp (BNot e) s = BNot (evalBExp e s)
evalBExp (BBinOp op e1 e2) s = BBinOp op (evalBExp e1 s) (evalBExp e2 s)
evalBExp e _ = e

-- Interpret an Assertion using the symbolic values in the current state
-- Basically, replace non-symbolic names with symbolic values
evalAssertion ::  Assertion -> State -> Assertion
evalAssertion (ACmp (Comp ord e1 e2)) s = ACmp (Comp ord (evalAExp e1 s) (evalAExp e2 s))
evalAssertion (ANot a) s = ANot (evalAssertion a s)
evalAssertion (ABinOp op a1 a2) s = ABinOp op (evalAssertion a1 s) (evalAssertion a2 s)
evalAssertion (AQ q xs a) s = AQ q xs (evalAssertion a s) -- TODO: refresh quantified variables
evalAssertion a _ = a


-- The empty state maps everything unconditionally to NaN (of the correct type)
empty :: State
empty (_,t) = Var ("_",t)

-- Update the symbolic value of the variable in the state
update :: (Name, Type, AExp) -> State -> State
update (x,t,e) s =
    \(x',t') ->
      if x == x' && t == t'
      then e
      else s (x',t')

updateMany :: State -> [(Name, Type, AExp)] -> State
updateMany = foldl (\s u -> update u s)


conj :: [Assertion] -> Assertion
conj = foldl (\acc a -> ABinOp And a acc) ATrue


-- Symbolic execution
type Environment = (Guard, State) -- Guard represents the path constraint

execute :: AST -> Environment -> ([Assertion], [State])
execute e env@(g,s) = case e of
  Assign x e ->
      -- only one resulting state
    ([], [update (x, TInt, evalAExp e s) s])
  Write a ei ev ->
    -- only one resulting state
    ([], [update (a, TArr, evalAExp a' s) s]) where
      a' = Store (Var (a, TArr)) ei ev
  Skip ->
    -- state unchanged
    ([], [s])
  If c tb fb ->
      (a1 ++ a2, ss1 ++ ss2) where
        cA = bexp2assert (evalBExp c s)
        not_cA = ANot cA
        -- augment path constraint with T/F conditions
        (a1, ss1) = execute tb (cA:g, s)
        (a2, ss2) = execute fb (not_cA:g, s)
  Seq b1 b2 ->
    (a1 ++ a2, ss2) where
      (a1, ss1) =  execute b1 env
      res = map (execute b2) (map (\s -> (g,s)) ss1)
      a2 = concat $ map fst res
      ss2 = concat $ map snd res
  Assert a ->
    ( [conj (evalAssertion a s : g)], [s] )
  While _ _ ->
    error "symbolic execution does not support loops"
  ParAssign _ _ _ _ ->
    error "Parallel assignment not implemented"

r = execute Skip ([ATrue], empty)

initVar :: [Typed] -> State
initVar = foldl (\st (x,t) -> update (x, t, x_sym x t) st) empty where
  x_sym = \x t -> Var ("_" ++ x, t) -- symbolic initial value for variable x


-- DEBUG
-- st = updateMany empty [
--    ("i", TInt, Var ("_i", TInt)), 
--    ("x", TInt, Var ("_x", TInt)), 
--    ("a", TArr, (Store (Var ("_a", TArr)) (Var ("_i", TInt)) (Var ("_x", TInt)))) ]

-- inc_arr = (Store (Var ("a",TArr)) (Var ("i",TInt)) (BinOp Add (Read (Var ("a",TArr)) (Var ("i",TInt))) (Var ("x",TInt))))
-- st' = update ("a", TArr, evalAExp inc_arr st) st
-- inc_x = (BinOp Add (Var ("x",TInt)) (Var ("x",TInt)))
-- st'' = update ("x", TInt, evalAExp inc_x st') st'

main :: IO ()
main = do
    -- putStrLn "hi"
  as <- getArgs
  prog <- readFile (head as) 
  let n = read (head (tail as)) :: Int
  let p = parseProg prog
  putStrLn (show p)

  
 

