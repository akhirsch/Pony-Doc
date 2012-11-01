{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, OverlappingInstances #-}

module Main where

import Calc.Base
import Data.Comp
import Text.ParserCombinators.Parsec
import Calc.Parser

evalEx :: Term Sig
evalEx = eval ((iConst 3 `iSub` iConst 2) `iAdd` iConst 3 :: Term Sig)

main :: IO ()
main = do
  let c = "+ * - 3 2 3 4"
  putStrLn . show $ evalEx
  --putStrLn . show $ eval (evalEx :: Term Sig)
  case parse sig'Parser "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 putStrLn . show $ e
    Right sig -> putStrLn . show $ sig
                 