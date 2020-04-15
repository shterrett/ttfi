{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module SectionTwo.FlatF where

import SectionTwo.Final
import SectionTwo.FinalMult
import SectionTwo.PushNegF (push_neg)
import qualified SectionTwo.PushNegF as PushNeg

-- The left-child-ness refers to the expression _to which the context is passed_
data Ctx e =
    LCA e -- ^ Left Child of Addition
    | LCM e -- ^ Left Child of Multiplication
    | NonLC

-- Needed to modify this instance in order to include multiplication. That
-- seems to break the extensibility of final tagless.
instance (MultSYM repr, ExpSYM repr) => ExpSYM (Ctx repr -> repr) where
    lit n = \case
              NonLC -> lit n
              LCA e -> add (lit n) e
              LCM e -> mult (lit n) e
    -- assuming push_neg already called and e is a literal
    neg e = \case
              NonLC -> neg (e NonLC)
              LCA e2 -> add (neg (e NonLC)) e2
              LCM e2 -> mult (neg (e NonLC)) e2
    add e1 e2 = \ctx -> e1 (LCA (e2 ctx))

instance MultSYM repr => MultSYM (Ctx repr -> repr) where
    mult e1 e2 = \ctx -> e1 (LCM (e2 ctx))

flatten :: (ExpSYM repr, MultSYM repr) => (Ctx repr -> repr) -> repr
flatten e = e NonLC

normalize :: (ExpSYM repr, MultSYM repr) => (PushNeg.Ctx -> Ctx repr -> repr) -> repr
normalize = flatten . push_neg
