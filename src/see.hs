{-# LANGUAGE TupleSections #-}

module Main (main) where

import Language
import Parser.Parser ( parseProg )

import Data.List ( intercalate, )
import Data.Set (Set)
import qualified Data.Set as S (empty, singleton, difference, union, unions, fromList, map, toList )  
import Data.Map (Map, (!))
import qualified Data.Map as Map (insert, fromList, mapAccumWithKey, keys, elems, lookup)
import Text.Printf ( printf )
import System.Environment ( getArgs, )
import Debug.Trace ( trace )


-- Basic idea:
-- Generate an AST that corresponds to the result of
--   unrolling the original AST m times
-- Then, symbolically execute the unrolled AST:
--   Assign each parameter with an arbitrary symbolic variable, e.g. x |-> _x
--   Keep track of two things in the symbolic execution machine:
--     the path constraint, analogous the program counter
--     a mapping from variables to _symbolic expressions_, like an interpreter's environment
--   Traverse the AST, collecting assertions along the way


-- Some definitions:
-- An AExp is an _symbolic expression_ if
--   the only variables it contains are symbolic variables.
--   Note that the parameters are treated as normal variables,
--   but the execution machine assigns them with symbolic values
--   before execution begins
-- A BExp or an Assertion is _purely symbolic_ if
--   it only contains _symbolic expressions_


-- Remove while loops
rmWhile :: AST -> AST
rmWhile (While c _) = Assume (ANot (assertBExp c))
rmWhile (If c tb fb) = If c (rmWhile tb) (rmWhile fb)
rmWhile (Seq s s') = Seq (rmWhile s) (rmWhile s')
rmWhile s = s


-- Unroll each while loop once
unrollOnce :: AST -> AST
unrollOnce (If c tb fb) = If c (unrollOnce tb) (unrollOnce fb)
unrollOnce (Seq b1 b2) = Seq (unrollOnce b1) (unrollOnce b2)
unrollOnce (While c b) = unrolled where
  b' = unrollOnce b
  unrolled = If c (Seq b' (While c b')) Skip
unrollOnce s = s


-- Unroll each while loop n times
unroll :: AST -> Int -> AST
unroll s n = 
  if n == 0
  then rmWhile s
  else unroll (unrollOnce s) (n-1)


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
evalAExp s (Var (x,t)) = case Map.lookup (x,t) s of
  Just e -> e
  Nothing -> error ("Not found: " ++ typedToString (x,t))
evalAExp s (BinOp op e1 e2) = BinOp op (evalAExp s e1) (evalAExp s e2)
evalAExp s (Read ea ei) = Read (evalAExp s ea) (evalAExp s ei)
evalAExp s (Store ea ei ev) = Store (evalAExp s ea) (evalAExp s ei) (evalAExp s ev)


-- Interpret a BExp using the symbolic values in the current state
-- Basically, replace non-symbolic names with symbolic values
evalBExp :: State -> BExp -> BExp 
evalBExp _ BTrue = BTrue
evalBExp _ BFalse = BFalse
evalBExp s (BCmp (Comp ord e1 e2)) = BCmp (Comp ord (evalAExp s e1) (evalAExp s e2))
evalBExp s (BNot e) = BNot (evalBExp s e)
evalBExp s (BBinOp op e1 e2) = BBinOp op (evalBExp s e1) (evalBExp s e2)


evalAssertion :: State -> Assertion -> Assertion
evalAssertion _ ATrue = ATrue
evalAssertion _ AFalse = AFalse 
evalAssertion s (ACmp (Comp ord e1 e2)) = ACmp (Comp ord (evalAExp s e1) (evalAExp s e2))
evalAssertion s (ANot e) = ANot (evalAssertion s e)
evalAssertion s (ABinOp op e1 e2) = ABinOp op (evalAssertion s e1) (evalAssertion s e2)
evalAssertion s (AMOp op aa) = AMOp op (map (evalAssertion s) aa)
evalAssertion s (AQ q qVars a) = AQ q qVars a' where
  -- shadow quantified variables with themselves as values in the state
  -- so that evalAExp/evalBExp won't mistake them for state variables if there's a clash
  -- e.g. assume that the state maps the program variable i to some symbolic value v, 
  -- and that the assertion is (forall i ...),
  -- then the quantified i should evaluate to itself, not v
  toInt x = (x, TInt)
  s' = foldl (\sAcc x -> Map.insert (toInt x) (Var (toInt x)) sAcc) s qVars
  a' = evalAssertion s' a



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

-- Execute program within an environment, and returns a pair (aa, envs)
-- where
--   aa is the list of (negated) assertions collected during the execution
--   envs is the list of possible environments after the execution
-- Invariants:
--   The path constraint is purely symbolic
execute :: AST -> Environment -> ([Assertion], [Environment])
execute e env@(g,s) = 
  -- trace ("Guard: " ++ show g ++ ". Program: " ++ (unwords $ showStmt e)) rrr where
  case e of
    Assign x e' ->
      -- only one resulting environment
      ([], [(g, update s ((x, TInt), e'))])
    Write a ei ev ->
      -- only one resulting environment
      ([], [(g, update s ((a, TArr), a'))]) where
        a' = Store (Var (a, TArr)) ei ev
    Skip ->
      -- environment unchanged
      ([], [env])
    If c tb fb ->
      -- execute each branch with updated path constraint, and
      -- union the resulting assertions and environments
      (a1 ++ a2, ss1 ++ ss2) where
        cA = assertBExp (evalBExp s c)
        not_cA = trace (show cA) (ANot cA)
        -- augment path constraint with T/F conditions
        (a1, ss1) = execute tb (cA:g, s)
        (a2, ss2) = execute fb (not_cA:g, s)
    Seq b1 b2 ->
      -- execute the 2nd block from all environments resulted from
      -- executing the 1st block. Then union the assertions, and return the 
      -- environments resulting from executing the 2nd block
      (a1 ++ concat aa2, concat envs2) where
        (a1, envs1) = execute b1 env
        res = map (execute b2) envs1
        (aa2, envs2) = unzip res
    Assert a ->
      -- return the assertion: neg (path constraint => a), i.e. 
      -- constraint /\ not a. The environment remains unchanged
      ( [conj (ANot a : g ++ assertState s)], [env] )
    Assume a ->
      ([], [(evalAssertion s a : g, s)])
    While _ _ ->
      -- ignore while loops as we assume loops have been fully unrolled
      ([], [env])
    ParAssign x1 x2 e1 e2 ->
      ([], [(g, updateMany s [((x1, TInt), e1), ((x2, TInt), e2)])])


-- Initialize state by mapping input variables to (arbitrary) symbolic values
initialState :: [Typed] -> State
initialState ts = Map.fromList (map x_sym ts) where
  x_sym (x,t) = ((x,t), Var (initialValue (x,t))) -- symbolic initial value for variable x

initialValue :: Typed -> Typed
initialValue (x,t) = ("_" ++ x,t)


-- Wrapper for the symbolic execution machine
see :: Int -> Program -> (AST, [String])
see n Program {name=_, param=ps, pre=p, ast=t} = (t', res) where
  t' = unroll t n
  initState = initialState ps
  (aa, _) = execute t' (map (evalAssertion initState) p, initState)
  res = map (intercalate "\n" . wrap) aa
  wrap a = [xtStr, "", aStr, check, get_value] where
    psSet = S.fromList ps
    psValSet = S.map initialValue psSet
    xtSet = S.unions [collectVarsInAssertion a, psSet, psValSet]
    declareTypes xt = printf "(declare-const %s)" (typedToString xt)
    xtStr = intercalate "\n" (map declareTypes (S.toList xtSet))
    aStr = printf "(assert %s)" (show a)
    check = "(check-sat)"
    get_value = printf ";%s" (unwords $ map (fst . initialValue) ps)


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
collectVarsInAssertion (AQ _ qVars a) = S.difference s sq where
    s = collectVarsInAssertion a
    sq = S.map (, TInt) (S.fromList qVars)
collectVarsInAssertion _ = S.empty


main :: IO ()
main = do
  as <- getArgs
  prog <- readFile (head as) 
  let n = read (head (tail as)) :: Int
  let p = parseProg prog
  let (p', ss) = see n p
  let pStr = intercalate "\n" (prefix "; " (showStmt $ rmWhile p'))
  let ss' = map (\s -> pStr ++ "\n" ++ s) ss

  putStrLn (intercalate "\n\n;SEP\n\n" (name p: ss'))

  
 

