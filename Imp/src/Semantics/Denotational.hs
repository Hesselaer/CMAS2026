{-# OPTIONS -Wall #-}

module Semantics.Denotational where

import Syntax.Abs
import Semantics.Memory
import Semantics.ExpEval
import Semantics.Delay

-- | Domain type that we will be computing in. This is essentially a type
-- of partial functions from memory to a.
type D a = Mem -> Delay a

-- | Implements the denotational semantics of IMP.
-- This maps should be implemented so that every step in a while loop
-- generates one Later step, see the test cases.
evalCom :: Com -> D Mem
evalCom c = case c of
  CSkip            -> pure -- η from lecture
  CAssign x  e     -> \m -> pure (updateMem x (evalAExp e m) m)
  CSeq    c1 c2    -> \m -> evalCom c1 m >>= evalCom c2
  CIte    b  c1 c2 -> \m ->
    if evalBExp b m then evalCom c1 m else evalCom c2 m
  CWhile  b  c1    -> \m ->
    if evalBExp b m
    then Later (evalCom c1 m >>= evalCom (CWhile b c1))
    else pure m
