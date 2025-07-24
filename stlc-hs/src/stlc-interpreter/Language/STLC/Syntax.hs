module Language.STLC.Syntax (module Syn, parse, stringify) where

import Language.STLC.Syntax.Generated.AbsSTLC as Syn
import Language.STLC.Syntax.Generated.LexSTLC (tokens)
import Language.STLC.Syntax.Generated.ParSTLC (pProg)
import Language.STLC.Syntax.Generated.PrintSTLC
  ( Print,
    printTree,
  )

parse :: String -> Either String Prog
parse = pProg . tokens

stringify :: (Print a) => a -> String
stringify = printTree
