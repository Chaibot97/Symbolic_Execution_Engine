module Main (main) where

import Language
import Parser.Parser ( parseProg )

import Data.List ((\\),  intercalate, filter, notElem )
import qualified Data.Set as S (intersection,  Set, empty, singleton, union, difference, fromList, toList, filter, map, foldl ) 
import Text.Printf ( printf )
import System.Environment ( getArgs )
import Debug.Trace ( trace )

bexp2assert :: BExp -> Assertion
bexp2assert (BCmp c) = ACmp c
bexp2assert (BNot b) = ANot (bexp2assert b)
bexp2assert (BBinOp op b1 b2) = ABinOp op b1' b2' where
  b1' = bexp2assert b1
  b2' = bexp2assert b2

-- Execute
uninit :: Name -> AExp
uninit name = Var (name ++ "_" ++ show 0)

initVar :: Param -> [Comparison] -> [Comparison]
initVar (PVar name) l = Comp Eq (Var name) (uninit name) : l
initVar (PArr name) l = l

assignVar :: [Comparison] -> AExp -> AExp -> [Comparison]
assignVar (h@(Comp Eq (Var n1) _ ):t) var@(Var n2) val = 
  if n1 == n2
    then Comp Eq var val : t
    else h : assignVar t var val

evalExp :: [Comparison] -> AExp -> AExp
evalExp l@(h@(Comp Eq (Var n1) val ):t) exp = 
  case exp of 
    Var name -> if n1 == name
      then val
      else evalExp t exp
    BinOp op exp1 exp2 -> BinOp op (evalExp l exp1) (evalExp l exp2)
    Read exp1 exp2 -> Read (evalExp l exp1) (evalExp l exp2)
    -- Arr Name
    -- Store AExp AExp AExp
    exp -> exp --num

type Constrain = ([Assertion], [Comparison])
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

newNode :: Constrain -> Tree Constrain
newNode c = Node c Empty Empty

leaves :: [a] -> Tree a -> [a]
leaves l tree@(Node node Empty Empty) = node:l
leaves l tree@(Node node c1 c2) = (leaves [] c1) ++ (leaves [] c2) ++ l

runStmts :: Block -> Int -> Tree Constrain -> Tree Constrain
runStmts (s:t) n tree@(Node node@(conds,vals) Empty Empty) = 
  case s of
    Assign name val 
      -> runStmts t n $ newNode (conds, assignVar vals (Var name) (evalExp vals val))
    If bexp blk1 blk2 
      -> Node node (runStmts (blk1++t) n $ newNode ((bexp2assert (BNot bexp)):conds, vals)) (runStmts (blk2++t) n $ newNode ((bexp2assert bexp):conds, vals))
    -- nested loop not supported yet
    While bexp blk
      -> case n of 
        0 -> runStmts t n $ newNode ((bexp2assert (BNot bexp)):conds, vals)
        _ -> Node node (runStmts t n $ newNode ((bexp2assert (BNot bexp)):conds, vals)) (runStmts (blk++t) (n-1) $ newNode ((bexp2assert bexp):conds, vals))
    Assert as -> runStmts t n $ newNode ((ANot as):conds, vals)
    -- Write Name AExp AExp
    -- ParAssign Name Name AExp AExp
    _ -> tree     -- Skip
runStmts [] n tree = tree
runStmts l n tree@(Node node@(conds,vals) c1 c2) = 
  Node node (runStmts l n c1) (runStmts l n c2)
runStmts l n Empty = Empty
    
    

main :: IO ()
main = do
  as <- getArgs
  prog <- readFile (head as) 
  let n = read (head (tail as)) :: Int
  let p = parseProg prog
  let vals = foldr initVar [] (param p)
  let conds = pre p
  let res = runStmts (block p) n (newNode (conds, vals))
  printf "%s\n" (show res)
  printf "%s\n" (show $ leaves [] res)
 

