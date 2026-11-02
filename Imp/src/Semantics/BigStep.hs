{-# OPTIONS -Wall #-}

module Semantics.BigStep where

import Syntax.Abs
import Semantics.Memory
import Semantics.ExpEval

evalCom :: Com -> Mem -> Mem
evalCom x = case x of
  CSkip -> id
  CAssign ident aexp -> \m -> updateMem ident (evalAExp aexp m) m
  CSeq com1 com2 -> evalCom com2 . evalCom com1
  CIte bexp com1 com2 -> \m ->
    if evalBExp bexp m then evalCom com1 m else evalCom com2 m
  CWhile bexp com -> \m ->
    if evalBExp bexp m
    then evalCom (CWhile bexp com) (evalCom com m)
    else m
