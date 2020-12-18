{-# LANGUAGE TupleSections #-}

module Main (main) where

import Language
import Parser.Parser ( parseProg )

import Data.List ( intercalate, )
import Data.Set (Set)
import qualified Data.Set as S (empty, singleton, difference, unions, fromList, map, toList )  
import Data.Map (Map, (!))
import qualified Data.Map as Map (insert, fromList, mapAccumWithKey, lookup)
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
rmWhile (While _ _) = Skip
rmWhile (If c tb fb) = If c (rmWhile tb) (rmWhile fb)
rmWhile (Seq s s') = Seq (rmWhile s) (rmWhile s')
rmWhile s = s


-- Unroll each while loop once
unrollOnce :: AST -> AST
unrollOnce (If c tb fb) = If c (unrollOnce tb) (unrollOnce fb)
unrollOnce (Seq b1 b2) = Seq (unrollOnce b1) (unrollOnce b2)
unrollOnce (While c b) =
    If c (Seq b' (While c b')) Skip where
      b' = unrollOnce b
unrollOnce s = s


-- Unroll each while loop n times
unroll :: AST -> Int -> AST
unroll s n = if n == 0 then s else unroll (unrollOnce s) (n-1)


-- Things we need to keep track of during symbolic execution
type Guard = [Assertion]
type SymbolicValue = AExp
type State = Map Typed SymbolicValue


-- Update the symbolic value of the variable in the state
update :: State -> (Typed, AExp) -> State
update s (xt, e) = Map.insert xt e' s where
  e' = evalAExp s e -- evaluate e in the current state

updateMany :: State -> [(Typed, AExp)] -> State
updateMany = foldl update


-- Interpret an AExp using the symbolic values in the current state
-- Basically, replace non-symbolic names with symbolic values
evalAExp :: State -> AExp -> AExp
evalAExp _ (Num n) = Num n
evalAExp s (Var (x,t)) = s ! (x,t)
evalAExp s (BinOp op e1 e2) = BinOp op (evalAExp s e1) (evalAExp s e2)
evalAExp s (Read ea ei) = Read (evalAExp s ea) (evalAExp s ei)
evalAExp s (Store ea ei ev) = Store (evalAExp s ea) (evalAExp s ei) (evalAExp s ev)

-- Interpret a BExp using the symbolic values in the current state
-- Basically, replace non-symbolic names with symbolic values
evalBExp :: State -> BExp -> BExp 
evalBExp _ BTrue = BTrue
evalBExp _ BFalse  = BFalse
evalBExp s (BCmp (Comp ord e1 e2)) = BCmp (Comp ord (evalAExp s e1) (evalAExp s e2))
evalBExp s (BNot e) = BNot (evalBExp s e)
evalBExp s (BBinOp op e1 e2) = BBinOp op (evalBExp s e1) (evalBExp s e2)


-- Convert BExp to Assertion
assertBExp :: BExp -> Assertion
assertBExp BTrue = ATrue
assertBExp BFalse = AFalse
assertBExp (BCmp c) = ACmp c
assertBExp (BNot b) = ANot (assertBExp b)
assertBExp (BBinOp op b1 b2) = ABinOp op b1' b2' where
  b1' = assertBExp b1
  b2' = assertBExp b2


-- Convert State to [Assertion]
assertState :: State -> [Assertion]
assertState s = fst (Map.mapAccumWithKey f [] s) where
  f acc xt e = (ACmp (Comp Eq (Var xt) e) : acc, e)


-- Return the conjunction of a list of assertions
conj :: [Assertion] -> Assertion
conj [] = ATrue
conj aa = AMOp And aa


-- Main symbolic execution machine
type Environment = (Guard, State) -- Guard represents the path constraint

-- Execute program within an environment, and returns a pair (aa, ss)
-- where
--   aa is the list of (negated) assertions collected during the execution
--   ss is the list of all reachable symbolic states after the execution
-- Preconditions:
--   input program contains no while loop
-- Invariants:
--   let env = (path constraint, state). Then the path constraint is purely symbolic
execute :: AST -> Environment -> ([Assertion], [State])
execute e env@(g,s) = case e of
  Assign x e' ->
    -- only one resulting state
    ([], [update s ((x, TInt), e')])
  Write a ei ev ->
    -- only one resulting state
    ([], [update s ((a, TArr), a')]) where
      a' = Store (Var (a, TArr)) ei ev
  Skip ->
    -- state unchanged
    ([], [s])
  If c tb fb ->
    -- execute each branch with updated path constraint, and
    -- union the resulting assertions and reachable states
    (a1 ++ a2, ss1 ++ ss2) where
      cA = assertBExp (evalBExp s c)
      not_cA = ANot cA
      -- augment path constraint with T/F conditions
      (a1, ss1) = execute tb (cA:g, s)
      (a2, ss2) = execute fb (not_cA:g, s)
  Seq b1 b2 ->
    -- execute the 2nd block from all reachable states once the 1st block
    -- finished execution
    -- union the assertions, and return reachable states once 2nd block is done
    (a1 ++ concat aa2, concat ss2) where
      (a1, ss1) =  execute b1 env
      res = map (execute b2 . (g,)) ss1
      (aa2, ss2) = unzip res
  Assert a ->
    -- return the assertion: not (path constraint => a), i.e. constraint /\ not a
    -- and state remains unchanged
    ( [conj (ANot a : g ++ assertState s)], [s] )
  While _ _ ->
    -- assume loops are fully unrolled
    ([], [s])
  ParAssign x1 x2 e1 e2 ->
    ([], [ updateMany s [((x1, TInt), e1), ((x2, TInt), e2)] ])


-- Initialize state by mapping input variables to (arbitrary) symbolic values
initialState :: [Typed] -> State
initialState ts = Map.fromList (map x_sym ts) where
  x_sym (x,t) = ((x,t), Var (initialValue (x,t), t)) -- symbolic initial value for variable x

initialValue :: Typed -> String
initialValue (x,t) = "_" ++ x

-- Wrapper for the symbolic execution machine
see :: Int -> Program -> (AST, [String])
see n Program {name=_, param=ps, pre=p, ast=t} = (t', res) where
  t' = unroll t n
  (aa, _) = execute t' (p, initialState ps)
  res = map (\a -> assertionToString a ++ check ++ get_value) aa
  check = "(check-sat)"
  get_value = printf "(get-value (%s))" (unwords (map initialValue ps))


-- Collect variables in an AExp
collectVars :: AExp -> Set Typed
collectVars (Num _) = S.empty 
collectVars (Var xt) = S.singleton xt
collectVars (BinOp _ e1 e2) = S.unions (map collectVars [e1, e2])
collectVars (Read ea ei) = S.unions (map collectVars [ea, ei])
collectVars (Store ea ei ev) = S.unions (map collectVars [ea, ei, ev])


-- Collect variables in an Assertion
collectVarsInAssertion :: Assertion -> Set Typed
collectVarsInAssertion (ACmp (Comp _ e1 e2)) = S.unions (map collectVars [e1,e2])
collectVarsInAssertion (ANot a) = collectVarsInAssertion a
collectVarsInAssertion (ABinOp _ a1 a2) = S.unions (map collectVarsInAssertion [a1,a2])
collectVarsInAssertion (AMOp _ aa) = S.unions (map collectVarsInAssertion aa)
collectVarsInAssertion (AQ _ qVars a) = 
  (S.difference s sq) where
    s = collectVarsInAssertion a
    sq = S.map (, TInt) (S.fromList qVars)
collectVarsInAssertion _ = S.empty


assertionToString :: Assertion -> String
assertionToString a = intercalate "\n" (tt ++ [aStr]) where
  declareTypes xt = printf "(declare-const %s)" (typedToString xt)
  xtSet = collectVarsInAssertion a
  tt = map declareTypes (S.toList xtSet)
  aStr = printf "(assert %s)" (show a)


-- DEBUGGING: how-to
-- Note: to debug interactively using GHCi, do the following:
--  1. uncomment the line in this file that imports the Parser
--  2. in the project folder, run `ghci`
--  3. run `:cd src` and then `:load see.hs`
--  4. debug
--  5. reload the file using `:reload`. repeat step 4

-- DEBUGGING: stuff
-- storeArr :: AExp
-- storeArr = (Store (Var ("_a", TArr)) (Var ("_i", TInt)) (Var ("_x", TInt)))
-- a= (AQ Forall ["_i"] (ACmp (Comp Eq (storeArr) (Var ("b",TArr)))))
-- r :: ([Assertion], [State])
-- r = execute Skip ([ATrue], empty)
-- st = updateMany empty [
--    ("i", TInt, Var ("_i", TInt)), 
--    ("x", TInt, Var ("_x", TInt)), 
--    ("a", TArr, store_a) ]

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
  let (p',ss) = see n p
  -- print p
  -- print (rmWhile p')
  putStrLn (intercalate "\n\n;SEP\n\n" (name p: ss))

  
 

