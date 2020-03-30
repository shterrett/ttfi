{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SectionTwo.FinalMult where

import SectionTwo.Final (ExpSYM(..))

class MultSYM repr where
    mult :: repr -> repr -> repr

example :: (ExpSYM repr, MultSYM repr) => repr
example = mult (lit 2) (add (lit 3) (neg (lit 7)))

instance MultSYM Int where
    mult x y = x * y

instance MultSYM String where
    mult x y = "(" ++ x ++ " * " ++ y ++ ")"

eval :: Int -> Int
eval = id

view :: String -> String
view = id
