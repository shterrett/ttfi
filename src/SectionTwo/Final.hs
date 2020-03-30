{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SectionTwo.Final where

class ExpSYM repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

example :: (ExpSYM repr) => repr
example = add (lit 8) (neg (add (lit 1) (lit 2)))

instance ExpSYM Int where
    lit n = n
    neg e = -e
    add e1 e2 = e1 + e2

eval :: Int -> Int
eval = id

instance ExpSYM String where
    lit = show
    neg e = "(-" ++ e ++ ")"
    add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"

view :: String -> String
view = id
