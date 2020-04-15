{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module SectionTwo.PushNegF where

import SectionTwo.Final
import SectionTwo.FinalMult

data Ctx = Pos | Neg

instance (ExpSYM repr) => ExpSYM (Ctx -> repr) where
    lit n = \case
              Pos -> lit n
              Neg -> neg $ lit n
    neg e = \case
              Pos -> e Neg
              Neg -> e Pos
    add e1 e2 = \ctx -> add (e1 ctx) (e2 ctx)

instance (MultSYM repr) => MultSYM (Ctx -> repr) where
    -- Arbitrarily push negation down left hand argument
    -- -1 * (x * y) == (-1 * x) * y == x * (-1 * y)
    mult e1 e2 = \case
                  Pos -> mult (e1 Pos) (e2 Pos)
                  Neg -> mult (e1 Neg) (e2 Pos)

push_neg :: ( ExpSYM repr , MultSYM repr) => (Ctx -> repr) -> repr
push_neg e = e Pos
