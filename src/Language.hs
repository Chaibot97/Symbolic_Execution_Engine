module Language where

import Data.List ( intercalate )
import Text.Printf ( printf )

type Name = String

data AOp = Add | Sub | Mul | Div | Mod
instance Show AOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "div"
  show Mod = "mod"

data BOp = And | Or | Imply
instance Show BOp where
  show And = "and"
  show Or  = "or"
  show Imply = "=>"

data Order = Eq | Neq | Le | Ge | Lt | Gt
instance Show Order where
  show Eq = "="
  show Neq = "!="
  show Le = "<="
  show Ge = ">="
  show Lt = "<"
  show Gt = ">"

data Type = TInt | TArr deriving (Eq, Ord)
instance (Show Type) where
  show TInt = "Int"
  show TArr = "(Array Int Int)"
type Typed = (Name, Type)

-- Arithmetic and array expressions
data AExp = Num Int
          | Var Typed
          | BinOp AOp AExp AExp
          | Read AExp AExp
          | Store AExp AExp AExp

instance Show AExp where
  show (Num n) = show n
  show (Var (x,_)) = x
  show (BinOp op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)
  show (Read a e) = printf "(select %s %s)" (show a) (show e)
  show (Store ae ei ev) = printf "(store %s %s %s)" (show ae) (show ei) (show ev)

-- Comparisons of expressions
data Comparison = Comp Order AExp AExp
instance Show Comparison where
  show (Comp ord e1 e2) = printf "(%s %s %s)" (show ord) (show e1) (show e2)

-- Boolean expressions 
data BExp = BTrue | BFalse
          | BCmp Comparison
          | BNot BExp
          | BBinOp BOp BExp BExp
instance Show BExp where
  show BTrue = "true"
  show BFalse = "false"
  show (BCmp cmp) = show cmp
  show (BNot b) = "(not " ++ show b ++ ")"
  show (BBinOp op b1 b2) = printf "(%s %s %s)" (show op) (show b1) (show b2)

-- First-order assertions
data QF = Forall | Exists
instance Show QF where
  show Forall = "forall"
  show Exists = "exists"
data Assertion = ATrue | AFalse
               | ACmp Comparison
               | ANot Assertion
               | ABinOp BOp Assertion Assertion
               | AMOp BOp [Assertion]
               | AQ QF [Name] Assertion
instance Show Assertion where
  show ATrue = "true"
  show AFalse = "false"
  show (ACmp cmp) = show cmp
  show (ANot b) = "(not " ++ show b ++ ")"
  show (ABinOp op b1 b2) = printf "(%s %s %s)" (show op) (show b1) (show b2)
  show (AMOp op aa) = printf "(%s %s)" (show op) (unwords (map show aa))
  show (AQ q xs s) = printf "(%s [%s] %s)" (show q) (unwords xs) (show s)

data AST =
    Assign Name AExp
  | Write Name AExp AExp
  | ParAssign Name Name AExp AExp
  | Skip
  | Seq AST AST
  | If BExp AST AST
  | While BExp AST
  | Assert Assertion
instance Show AST where
  show s = intercalate "\n" (showStmt s)

showStmt :: AST -> [String]
showStmt (Assign x e) = [x ++ " := " ++ show e ++ ";"]
showStmt (ParAssign x y ex ey) = [x ++ ", " ++ y ++ " := " ++ show ex ++ ", " ++ show ey ++ ";"]
showStmt (Write a ei ev) = [printf "%s[%s] := %s;" a (show ei) (show ev)]
-- showStmt (If c tb Skip) =
--   [ "if " ++ show c
--   , "then" ] ++
--     indent (showStmt tb) ++
--   [ "end" ]
showStmt (If c tb fb) =
  [ "if " ++ show c
  , "then" ] ++
    indent (showStmt tb) ++
  [ "else" ] ++
    indent (showStmt fb) ++
  [ "end" ]
showStmt (While c b) =
  [ "while " ++ show c ] ++
  [ "do" ] ++
    indent (showStmt b) ++
  [ "end" ]
showStmt Skip = [ "skip;" ]
showStmt (Seq b1 b2) = showStmt b1 ++ showStmt b2
showStmt (Assert assertion) = ["assert " ++ show assertion ++ ";"]

prefix :: String -> [String] -> [String]
prefix pre = map (pre ++)

indent :: [String] -> [String]
indent = prefix "  "


data Program = Program { name  :: Name
                       , param :: [Typed]
                       , pre   :: [Assertion]
                       , ast   :: AST
                       }
instance Show Program where
  show Program {name=name, param=param, pre=pre, ast=ast} =
    intercalate "\n" (
    [ "program " ++ name ++ "(" ++ unwords (map show param) ++ ")"
    , intercalate "\n" (prefix "pre " (map show pre))
    , "is" ] ++
    [ intercalate "\n" (indent (showStmt ast)) ] ++
    [ "end" ])

