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

data Type = Int | IntArr deriving (Eq, Ord)
instance (Show Type) where
  show Int = "Int"
  show IntArr = "(Array Int Int)"
type Typed = (Name, Type)
type Typing = [Typed]

-- Arithmetic and array expressions
data AExp = Num Int
          | Var Name
          | Arr Name
          | BinOp AOp AExp AExp
          | Read AExp AExp
          | Store AExp AExp AExp

instance Show AExp where
  show (Num n) = show n
  show (Var x) = x
  show (Arr x) = x
  show (BinOp op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)
  show (Read a e) = printf "(select %s %s)" (show a) (show e)
  show (Store ae ei ev) = printf "(store %s %s %s)" (show ae) (show ei) (show ev)

-- Comparisons of AExpressions
data Comparison = Comp Order AExp AExp
instance Show Comparison where
  show (Comp ord e1 e2) = printf "(%s %s %s)" (show ord) (show e1) (show e2)

-- Boolean AExpressions 
data BExp = BCmp Comparison
          | BNot BExp
          | BBinOp BOp BExp BExp
instance Show BExp where
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
               | AQ QF [Name] Assertion
instance Show Assertion where
  show ATrue = "true"
  show AFalse = "false"
  show (ACmp cmp) = show cmp
  show (ANot b) = "(not " ++ show b ++ ")"
  show (ABinOp op b1 b2) = printf "(%s %s %s)" (show op) (show b1) (show b2)
  show (AQ q xs s) = printf "(%s [%s] %s)" (show q) (intercalate " " xs) (show s)

-- Program statements
type Block = [Statement]
data Statement = Assign Name AExp
               | Write Name AExp AExp
               | ParAssign Name Name AExp AExp
               | If BExp Block Block
               | While BExp [Assertion] Block
               | Skip
instance Show Statement where
  show s = intercalate "\n" (show_list s)

show_list :: Statement -> [String]
show_list (Assign x e) = [x ++ " := " ++ show e ++ ";"]
show_list (ParAssign x y ex ey) = [x ++ ", " ++ y ++ " := " ++ show ex ++ ", " ++ show ey ++ ";"]
show_list (Write a ei ev) = [printf "%s[%s] := %s" a (show ei) (show ev)]
show_list (If b c1 c2) =
  [ "if " ++ show b
  , "then" ] ++
    indent (showlist_block c1) ++
  [ "else" ] ++
    indent (showlist_block c2) ++
  [ "end" ]
show_list (While b invs c) =
  [ "while " ++ show b ] ++
    indent (prefix "inv " (map show invs)) ++
  [ "do" ] ++
    indent (showlist_block c) ++
  [ "end" ]
show_list (Skip) = [ "skip" ]

prefix :: String -> [String] -> [String]
prefix pre = map (\x -> pre ++ x)

indent :: [String] -> [String]
indent = prefix "  "

showlist_block :: Block -> [String]
showlist_block b = concat (map show_list b)

data Program = Program { name  :: Name
                       , pre   :: [Assertion]
                       , post  :: [Assertion]
                       , block :: Block
                       }
instance Show Program where
  show Program {name=name, pre=pre, post=post, block=block} =
    intercalate "\n" (
    [ "program " ++ name
    , intercalate "\n" (prefix "pre " (map show pre))
    , intercalate "\n" (prefix "post " (map show post))
    , "is" ] ++
    [ intercalate "\n" (indent (showlist_block block)) ] ++
    [ "end" ])

-- Loop-free guarded command language
type GCBlock = [GuardedCommand]
data GuardedCommand = GCAssert Assertion
                    | GCAssume Assertion
                    | GCHavoc Typed
                    | GCChoice GCBlock GCBlock
instance (Show GuardedCommand) where
  show gc = intercalate "\n" (gc_strlist gc)

gc_strlist :: GuardedCommand -> [String]
gc_strlist gc = case gc of
  GCAssert s -> ["assert " ++ show s]
  GCAssume s -> ["assume " ++ show s]
  GCHavoc (v,_) -> ["havoc " ++ v]
  GCChoice c1 c2 -> ["choose"] ++ b1 ++ ["or"] ++ b2 where
    b1 = indent (gcblock_strlist c1)
    b2 = indent (gcblock_strlist c2)

gcblock_strlist :: [GuardedCommand] -> [String]
gcblock_strlist l = concat (map gc_strlist l)