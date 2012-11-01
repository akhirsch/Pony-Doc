{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, OverlappingInstances #-}

module Calc.Extension where

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()
import Calc.Base

data ExtendOp e = Mul e e 

data ExtendVal e = Ext Float

$(derive [makeFunctor, makeTraversable, makeFoldable, smartConstructors, makeEqF, makeShowF, makeOrdF, smartAConstructors] [''ExtendOp, ''ExtendVal])

type Sig' = ExtendOp :+: Sig 

instance (Value :<: v) => Eval ExtendOp v where
  evalAlg (Mul x y) = iConst $ projC x * projC y

