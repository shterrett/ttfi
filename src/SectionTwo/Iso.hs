{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SectionTwo.Iso where

import SectionTwo.Initial (Exp(..))
import SectionTwo.Final (ExpSYM(..))
import SectionTwo.FinalMult (MultSYM(..))

iToF :: (ExpSYM a, MultSYM a) => Exp -> a
iToF (Lit x) = lit x
iToF (Neg e) = neg (iToF e)
iToF (Add e1 e2) = add (iToF e1) (iToF e2)
iToF (Mult e1 e2) = mult (iToF e1) (iToF e2)

fToI :: (forall a. (ExpSYM a, MultSYM a) => a) -> Exp
fToI x = x

instance ExpSYM Exp where
    lit = Lit
    neg = Neg
    add e1 e2 = Add e1 e2

instance MultSYM Exp where
    mult e1 e2 = Mult e1 e2
