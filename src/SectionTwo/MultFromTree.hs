{-# OPTIONS_GHC -fno-warn-orphans #-}

module SectionTwo.MultFromTree where

import Prelude hiding (div)

import Control.Applicative((<|>), Alternative(..))
import Data.Function (fix)
import qualified SectionTwo.Final as F
import SectionTwo.FinalMult (MultSYM(..))
import qualified SectionTwo.FinalMult as M
import qualified SectionTwo.FromTree as T

instance MultSYM T.Tree where
    mult e1 e2 = T.Node "Mul" [e1, e2]

instance (MultSYM repr, MultSYM repr') => MultSYM (repr, repr') where
    mult (e11, e12) (e21, e22) = (mult e11 e21, mult e12 e22)

fromTreeExt ::
  (F.ExpSYM repr, M.MultSYM repr)
  => (T.Tree -> Either T.Err repr)
  -> T.Tree
  -> Either T.Err repr
fromTreeExt self (T.Node "Mul" [e1, e2]) = mult <$> self e1 <*> self e2
fromTreeExt self t = T.fromTreeExt self t

fromTree :: (F.ExpSYM repr, M.MultSYM repr) => T.Tree -> Either T.Err repr
fromTree = fix fromTreeExt

threeThings :: IO ()
threeThings = T.check_consume T.thrice . fromTree $ M.example

class DivSYM repr where
    div :: repr -> repr -> repr

instance DivSYM T.Tree where
    div e1 e2 = T.Node "Div" [e1, e2]

fromTreeDiv :: (F.ExpSYM repr, DivSYM repr) => (T.Tree -> Either T.Err repr) -> T.Tree -> Either T.Err repr
fromTreeDiv self (T.Node "Div" [e1, e2]) = div <$> self e1 <*> self e2
fromTreeDiv self t = T.fromTreeExt self t

fromTreeEverything ::
  (F.ExpSYM repr, M.MultSYM repr, DivSYM repr)
  => (T.Tree -> Either T.Err repr)
  -> T.Tree
  -> Either T.Err repr
fromTreeEverything self t =
    T.fromTreeExt self t
    <|> fromTreeDiv self t
    <|> fromTreeExt self t

instance (Monoid e) => Alternative (Either e) where
    empty = Left mempty
    (Left _) <|> r = r
    r@(Right _) <|> _ = r
