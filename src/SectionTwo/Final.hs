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

instance ExpSYM String where
    lit n = show n
    neg e = "-" <> e
    add e1 e2 = "(" <> e1 <> " + " <> e2 <> ")"

eval :: Int -> Int
eval = id

view :: String -> String
view = id

instance ExpSYM RoundBrackets where
    lit = RoundBrackets . show
    neg (RoundBrackets e) = RoundBrackets $ "(-" ++ e ++ ")"
    add (RoundBrackets e1) (RoundBrackets e2) = RoundBrackets $ "(" ++ e1 ++ " + " ++ e2 ++ ")"

newtype RoundBrackets = RoundBrackets { unRound :: String }
newtype SquareBrackets = SquareBrackets { unSquare :: String }

viewRound :: RoundBrackets -> String
viewRound = unRound
