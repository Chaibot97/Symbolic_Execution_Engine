{-# LANGUAGE TupleSections #-}

module Main (main) where

import Language
import Parser.Parser ( parseProg )

import Data.List ((\\),  intercalate, filter, notElem, foldl, concatMap, intersperse, intersperse, )
import qualified Data.Set as S (intersection,  Set, empty, singleton, union, difference, fromList, toList, filter, map, foldl ) 
import Text.Printf ( printf )
import System.Environment ( getArgs, )
import Debug.Trace ( trace )


-- Basic idea:
-- For each 0 <= m <= n, generate an AST that corresponds to the result of
--   unrolling the original AST m times and contains no more while loops
-- Then, symbolically execute each unrolled AST:
--   Assign each parameter with an arbitrary symbolic variable, e.g. x |-> _x
--   Keep track of two things in the symbolic execution machine:
--     the path constraint, analogous the program counter
--     a mapping from variables to _symbolic expressions_, like an interpreter's environment
--   Traverse the AST, collecting assertions along the way


-- Some definitions:
-- An AExp is an _symbolic expression_ if
--   the only variables it contains are symbolic variables
-- A BExp or an Assertion is _purely symbolic_ if
--   it only contains _symbolic expressions_


-- TODO
-- During preprocessing stage, replace every quantified variable with a fresh name
-- Implement parallel assignment


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
unroll :: AST -> Int -> AST
unroll s n = if n == 0 then rmWhile s else unroll (unrollOnce s) (n-1)


-- A sequence of increasingly unrolled programs
unrollSeq ::  AST -> Int -> [AST]
unrollSeq s n = map (unroll s) [0..n]


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
evalAssertion (AQ q xs a) s = AQ q xs (evalAssertion a s) -- TODO: need to refresh quantified variables
evalAssertion a _ = a


-- The empty state maps everything unconditionally to variable "_" (of the correct type)
empty :: State
empty (_,t) = Var ("_",t)

-- Update the symbolic value of the variable in the state
update :: State -> (Name, Type, AExp) -> State
update s (x,t,e) (x',t') =
      if x == x' && t == t'
      then e
      else s (x',t')

updateMany :: State -> [(Name, Type, AExp)] -> State
updateMany = foldl update


-- Convert BExp to Assertion
bexp2assert :: BExp -> Assertion
bexp2assert BTrue = ATrue
bexp2assert BFalse = AFalse
bexp2assert (BCmp c) = ACmp c
bexp2assert (BNot b) = ANot (bexp2assert b)
bexp2assert (BBinOp op b1 b2) = ABinOp op b1' b2' where
  b1' = bexp2assert b1
  b2' = bexp2assert b2


-- Return the conjugation of a list of assertions
conj :: [Assertion] -> Assertion
conj = foldl (ABinOp And) ATrue


-- Main symbolic execution machine
type Environment = (Guard, State) -- Guard represents the path constraint

-- Execute program within an environment, and returns a pair (aa, ss)
-- where
--   aa is the list of (negated) assertions collected during the execution
--   ss is the list of all reachable symbolic states after the execution
-- Invariants:
--   let env = (path constraint, state). Then the path constraint is purely symbolic
--   returned assertions are also purely symbolic
--   input program contains no while loop
execute :: AST -> Environment -> ([Assertion], [State])
execute e env@(g,s) = case e of
  Assign x e ->
    -- only one resulting state
    ([], [update s (x, TInt, evalAExp e s)])
  Write a ei ev ->
    -- only one resulting state
    ([], [update s (a, TArr, evalAExp a' s)]) where
      a' = Store (Var (a, TArr)) ei ev
  Skip ->
    -- state unchanged
    ([], [s])
  If c tb fb ->
    -- execute each branch with updated path constraint, and
    -- union the resulting assertions and reachable states
    (a1 ++ a2, ss1 ++ ss2) where
      cA = bexp2assert (evalBExp c s)
      not_cA = ANot cA
      -- augment path constraint with T/F conditions
      (a1, ss1) = execute tb (cA:g, s)
      (a2, ss2) = execute fb (not_cA:g, s)
  Seq b1 b2 ->
    -- execute the 2nd block from all reachable states once the 1st block
    -- finished execution
    -- union the assertions, and return reachable states once 2nd block is done
    (a1 ++ a2, ss2) where
      (a1, ss1) =  execute b1 env
      res = map (execute b2 . (g,)) ss1
      a2 = concatMap fst res
      ss2 = concatMap snd res
  Assert a ->
    -- return the assertion: not (path constraint => a), i.e. constraint /\ not a
    -- and state remains unchanged
    ( [conj (ANot (evalAssertion a s) : g)], [s] )
  While _ _ ->
    -- assume loops are fully unrolled and eliminated before symbolic execution
    error "symbolic execution does not support loops"
  ParAssign {} ->
    -- TODO: implement parallel assignment
    error "Parallel assignment not implemented"

-- Initialize state by mapping input variables to (arbitrary) symbolic values
initialState :: [Typed] -> State
initialState = foldl (\st (x,t) -> update st (x, t, x_sym x t)) empty where
  x_sym = \x t -> Var ("_" ++ x, t) -- symbolic initial value for variable x


-- DEBUGGING: how-to
-- Note: to debug interactively using GHCi, do the following:
--  1. uncomment the line in this file that imports the Parser
--  2. in the project folder, run `ghci`
--  3. run `:cd src` and then `:load see.hs`
--  4. debug
--  5. reload the file using `:reload`. repeat step 4

-- DEBUGGING: stuff
-- r :: ([Assertion], [State])
-- r = execute Skip ([ATrue], empty)
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
  print p

  
 

