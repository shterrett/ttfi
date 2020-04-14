{-# OPTIONS_GHC -fno-warn-orphans #-}

module SectionTwo.FromTree where

import Data.Function (fix)
import SectionTwo.Final (ExpSYM(..), example, eval, view)

data Tree = Leaf String
          | Node String [Tree]
          deriving (Eq, Read, Show)

instance ExpSYM Tree where
    lit n = Node "Lit" [Leaf $ show n]
    neg e = Node "Neg" [e]
    add e1 e2 = Node "Add" [e1, e2]

toTree :: Tree -> Tree
toTree = id

tf1_tree :: Tree
tf1_tree = toTree example

instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr, repr') where
    lit x = (lit x, lit x)
    neg (e1, e2) = (neg e1, neg e2)
    add (e11, e12) (e21, e22) = (add e11 e21, add e12 e22)

duplicate :: (ExpSYM repr, ExpSYM repr') => (repr, repr') -> (repr, repr')
duplicate = id

type Err = String

safeRead :: Read a => String -> Either Err a
safeRead s = case reads s of
               [(x, "")] -> Right x
               _ -> Left $ "Read error " <> s

check_consume :: (ExpSYM a) => (a -> IO ()) -> Either Err a -> IO ()
check_consume _ (Left e) = putStrLn $ "Error " <> e
check_consume f (Right a) = f a

dup_consume ::  (ExpSYM a, ExpSYM a') => (a -> String) -> (a, a') -> IO a'
dup_consume f a = print (f a1) >> pure a2
  where (a1, a2) = duplicate a

thrice :: (Int, (String, Tree)) -> IO ()
thrice a = dup_consume (show . eval) a >>= dup_consume view >>= print . toTree

fromTreeExt :: (ExpSYM repr) => (Tree -> Either Err repr) -> Tree -> Either Err repr
fromTreeExt _ (Node "Lit" [Leaf n]) = lit <$> safeRead n
fromTreeExt self (Node "Neg" [e]) = neg <$> self e
fromTreeExt self (Node "Add" [e1, e2]) = add <$> self e1 <*> self e2
fromTreeExt _ t = Left $ "Error parsing tree " <> show t

fromTree :: (ExpSYM repr) => Tree -> Either Err repr
fromTree = fix fromTreeExt

threeThings :: IO ()
threeThings = check_consume thrice . fromTree $ tf1_tree
