{-# LANGUAGE KindSignatures
           , MultiParamTypeClasses
           , NoMonomorphismRestriction
           , FlexibleInstances
           , FlexibleContexts
           , TupleSections
           , ScopedTypeVariables
#-}
module Linear where

newtype F a = F a
  deriving (Show, Eq)

data U = Used
  deriving (Show, Eq)

class LSym (repr :: * -> * -> * -> *) where
    int :: Int -> repr hi hi Int
    add :: repr hi h Int -> repr h ho Int -> repr hi ho Int

    -- variable substitution
    z :: repr (F a, h) (U, h) a
    s :: repr hi ho a -> repr (any, hi) (any, ho) a

    -- lambda application
    app :: repr hi h (a -> b) -> repr h ho a -> repr hi ho b

-- Separate lambda abstraction to expose hi and ho in the instance head
-- so that instances can place constraints on them
class LLam repr hi ho where
    lam :: repr (F a, hi) (U, ho) b -> repr hi ho (a -> b)
    lamU :: repr hi ho b -> repr (F a, hi) (U, ho) b -- "affine"/"relaxation"/"don't use the arg"

ex1 :: LSym repr => repr hi hi Int
ex1 = add (int 1) (int 2)

-- Not linear because `z` is used twice
-- • Couldn't match type ‘F Int’ with ‘U’
--  Expected type: repr (U, ho) (U, ho) Int
--    Actual type: repr (F Int, ho) (U, ho) Int
-- • In the second argument of ‘add’, namely ‘z’
--   In the first argument of ‘lam’, namely ‘(add z z)’
--   In the expression: lam (add z z)
-- ex2 = lam (add (z :: _) (z :: _))

-- Not linear because `z` is not used at all
-- • Couldn't match type ‘F a’ with ‘U’
--   Expected type: repr (F a, ho) (U, ho) Int
--     Actual type: repr (F a, ho) (F a, ho) Int
-- • In the first argument of ‘lam’, namely ‘(int 1)’
--   In the expression: lam (int 1)
--   In an equation for ‘ex3’: ex3 = lam (int 1)
-- • Relevant bindings include
--     ex3 :: repr ho ho (a -> Int)
--       (bound at /Users/stuart/coding/ttfi/src/Linear.hs:42:1)
ex3 = lamU (int 1)

-- Linear because each lambda's argument, `z` and `s z`, is used exactly
-- once
ex4 ::
  ( LLam repr ho ho
  , LLam repr (F a, ho) (U, ho)
  , LSym repr
  )
  => repr ho ho (a -> (a -> b) -> b)
ex4 = lam $ lam $ app z (s z)

ex5 ::
  ( LLam repr ho ho
  , LSym repr
  )
  => repr ho ho ((Int -> Int) -> Int)
ex5 = lam (add (app z (int 1)) (int 2))

ex6 = lam (lam (add z (s z)))

ex7 = lam (app (lam z) z)

ex8 = lam (z `app` lam z)

newtype R hi ho a = R { unR :: hi -> (a, ho) }

instance LSym R where
    int i = R $ \hi -> (i, hi)
    add x y = R $ \hi ->
      let
        (x', h) = unR x hi
        (y', ho) = unR y h
      in
        (x' + y', ho)
    z = R $ \hi -> unR z hi
    s var = R $ \(thing, hi) -> (thing, ) <$> unR var hi
    app f x = R $ \hi ->
      let
        (f', h) = unR f hi
        (x', ho) = unR x h
      in
        (f' x', ho)

-- To interpret the lambda, we need `ho` when the lambda is created
-- However, it's only evaluated when the lambda is _applied_
-- Luckily, we only need _type_, which we _can_ know when the lambda is
-- created
class HiHo hi ho where
    hiho :: hi -> ho

instance HiHo () () where
    hiho = id

instance HiHo hi ho => HiHo (F a, hi) (F a, ho) where
    hiho = fmap hiho

instance HiHo hi ho => HiHo (U, hi) (U, ho) where
    hiho = fmap hiho

instance HiHo hi ho => HiHo (F a, hi) (U, ho) where
    hiho (_, hi) = (Used, hiho hi)

instance HiHo hi ho => LLam R hi ho where
    lam e = R $ \hi -> (f hi, hiho hi)
     where f hi x = fst $ unR e (F x, hi)
    lamU e = R $ \(_, hi) -> (Used,) <$> unR e hi

eval :: R () ho a -> a
eval e = fst $ unR e ()
