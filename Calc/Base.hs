{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, OverlappingInstances #-}

module Calc.Base where 

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()
import Data.Comp.Equality ()

data Value e = Const Int

data Op e = Add e e | Sub e e

type Sig = Op :+: Value

$(derive [makeFunctor, makeTraversable, makeFoldable, smartConstructors, makeEqF, makeShowF, makeOrdF, smartAConstructors] [''Value, ''Op])

class Eval f v where 
  evalAlg :: Alg f (Term v)
  
$(derive [liftSum] [''Eval])

eval :: (Functor f, Eval f v) => Term f -> Term v
eval = cata evalAlg

instance (f :<: v) => Eval f v where
  evalAlg = inject
  
instance (Value :<: v) => Eval Op v where
  evalAlg (Add x y) = iConst $ projC x + projC y
  evalAlg (Sub x y) = iConst $ projC x - projC y
  
projC :: (Value :<: v) => Term v -> Int
projC v = case project v of Just (Const n) -> n

