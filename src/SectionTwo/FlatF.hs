{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module SectionTwo.FlatF where

import SectionTwo.Final
import SectionTwo.FinalMult
import SectionTwo.PushNegF (push_neg)
import qualified SectionTwo.PushNegF as PushNeg

-- The left-child-ness refers to the expression _to which the context is passed_
data CtxA e =
    LCA e -- ^ Left Child of Addition
    | NonLCA

-- Needed to modify this instance in order to include multiplication. That
-- seems to break the extensibility of final tagless.
instance (ExpSYM repr) => ExpSYM (CtxA repr -> repr) where
    lit n = \case
              NonLCA -> lit n
              LCA e -> add (lit n) e
    -- assuming push_neg already called and e is a literal
    neg e = \case
              NonLCA -> neg (e NonLCA)
              LCA e2 -> add (neg (e NonLCA)) e2
    add e1 e2 = \ctx -> e1 (LCA (e2 ctx))

-- New; don't touch the stuff above

data CtxM e =
    LCM e
    | NonLCM

instance (ExpSYM repr, MultSYM repr) => MultSYM (CtxM repr -> repr) where
    mult e1 e2 = \ctx -> e1 (LCM (e2 ctx))

instance (ExpSYM repr, MultSYM repr) => MultSYM (CtxA repr -> repr) where
    mult e1 e2 = \ctx -> mult (e1 ctx) (e2 ctx)

instance (ExpSYM repr, MultSYM repr) => ExpSYM (CtxM repr -> repr) where
    lit n = \case
              NonLCM -> lit n
              LCM e -> mult (lit n) e
    neg e = \case
              NonLCM -> neg (e NonLCM)
              LCM e2 -> mult (neg (e NonLCM)) e2
    add e1 e2 = \case
                  NonLCM -> add (e1 NonLCM) (e2 NonLCM)
                  -- mult e3 (add e1 e2)
                  LCM e3 -> mult e3 (add (e1 NonLCM) (e2 NonLCM))

flattenA :: (ExpSYM repr) => (CtxA repr -> repr) -> repr
flattenA e = e NonLCA

flattenM :: (MultSYM repr) => (CtxM repr -> repr) -> repr
flattenM e = e NonLCM

normalize ::
  (ExpSYM repr, MultSYM repr)
  => (PushNeg.Ctx -> CtxA (CtxM repr -> repr) -> CtxM repr -> repr)
  -> repr
normalize = flattenM . flattenA . push_neg

example1 :: (ExpSYM repr, MultSYM repr) => repr
example1 = add (mult (add (lit 1) (neg (lit 2))) (lit 4)) (lit 3)

example2 :: (ExpSYM repr, MultSYM repr) => repr
example2 = neg (add (mult (lit 2) (lit 3)) (add (lit 4) (lit 5)))

example3 :: (ExpSYM repr, MultSYM repr) => repr
example3 = add (add (lit 3) (lit 4)) (mult (lit 5) (lit 6))

exampleA :: (ExpSYM repr, MultSYM repr) => repr
exampleA = add (add (lit 1) (neg (lit 2))) (lit 4)

exampleM :: (ExpSYM repr, MultSYM repr) => repr
exampleM = mult (mult (lit 3) (lit 4)) (lit 5)
