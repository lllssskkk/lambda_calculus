module Language.LetPoly.Syntax (module Syn, parse, stringify) where

import Language.LetPoly.Syntax.Generated.AbsLetPoly as Syn
import Language.LetPoly.Syntax.Generated.LexLetPoly (tokens)
import Language.LetPoly.Syntax.Generated.ParLetPoly (pProg)
import Language.LetPoly.Syntax.Generated.PrintLetPoly
  ( Print,
    printTree,
  )

parse :: String -> Either String Prog
parse = pProg . tokens

stringify :: (Print a) => a -> String
stringify = printTree
