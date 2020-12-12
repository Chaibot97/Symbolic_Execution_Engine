module Main (main) where

import Language
import Parser.Parser ( parseProg )

import Data.List ((\\),  intercalate, filter, notElem )
import qualified Data.Set as S (intersection,  Set, empty, singleton, union, difference, fromList, toList, filter, map, foldl ) 
import Text.Printf ( printf )
import System.Environment ( getArgs )
import Debug.Trace ( trace )

main :: IO ()
main = do
  as <- getArgs
  prog <- readFile (head as) 
  putStrLn (show (parseProg prog))