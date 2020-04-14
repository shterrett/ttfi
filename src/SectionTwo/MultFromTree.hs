{-# OPTIONS_GHC -fno-warn-orphans #-}

module SectionTwo.MultFromTree where

import Data.Function (fix)
import qualified SectionTwo.Final as F
import SectionTwo.FinalMult (MultSYM(..))
import qualified SectionTwo.FinalMult as M
import qualified SectionTwo.FromTree as T

instance MultSYM T.Tree where
    mult e1 e2 = T.Node "Mul" [e1, e2]

instance (MultSYM repr, MultSYM repr') => MultSYM (repr, repr') where
    mult (e11, e12) (e21, e22) = (mult e11 e21, mult e12 e22)

fromTreeExt :: (F.ExpSYM repr, M.MultSYM repr) => (T.Tree -> Either T.Err repr) -> T.Tree -> Either T.Err repr
fromTreeExt self (T.Node "Mul" [e1, e2]) = mult <$> self e1 <*> self e2
fromTreeExt self t = T.fromTreeExt self t

fromTree :: (F.ExpSYM repr, M.MultSYM repr) => T.Tree -> Either T.Err repr
fromTree = fix fromTreeExt

threeThings :: IO ()
threeThings = T.check_consume T.thrice . fromTree $ M.example
