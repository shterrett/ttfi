\documentclass[pdf]{beamer}
%include polycode.fmt
\mode<presentation>{}

\usepackage{xcolor}
\definecolor{light-gray}{gray}{0.95}
\newcommand{\code}[1]{\colorbox{light-gray}{\texttt{#1}}}

\title{Typed Tagless Final Interpreters}
\author{Stuart Terrett}

\begin{document}
\begin{frame}
  \titlepage
\end{frame}

\begin{frame}
\frametitle{Preamble}

\begin{code}
module TTFI where
\end{code}

\end{frame}

\begin{frame}
\frametitle{Typed}

The object language is \emph{typed}
\pause
\begin{spec}
add (int 2) (bool 3)
\end{spec}
\pause
will not compile

\end{frame}

\begin{frame}
\frametitle{tagless}
The object language terms are not wrapped in a sum type with type tags

\pause

\begin{spec}
data Term =
  TInt Int
  | TBool Bool
  | TStr String
\end{spec}

\end{frame}

\begin{frame}
\frametitle{final}

Terms of the object language are represented as expressions in the metalanguage

\pause
\begin{code}
twoPlusTwo :: (AddSym repr) => repr Int
twoPlusTwo = add (int 2) (int 2)
\end{code}

\pause

Not abstract syntax

\end{frame}

\begin{frame}
\frametitle{interpreter}

The syntax is a typeclass

\begin{code}
class AddSym repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
\end{code}
\end{frame}

\begin{frame}
\frametitle{interpreter}

The semantics are an instance of the typeclass

\begin{code}
newtype R a = R { unR :: a }

instance AddSym R where
  int = R
  add (R x) (R y) = R $ x + y
\end{code}

\end{frame}

\begin{frame}
\frametitle{extensible}
Adding interpretations

\begin{code}
newtype S a = S { unS :: Int -> String }
instance AddSym S where
  int = S . const . show
  add (S x) (S y) = S $ \c ->
    "(" <> x c <> " + " <> y c <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{extensible}
Adding operations

\begin{code}
class MultSym repr where
  mul :: repr Int -> repr Int -> repr Int

instance MultSym R where
  mul (R x) (R y) = R $ x * y

instance MultSym S where
  mul (S x) (S y) = S $ \c ->
    "(" <> x c <> " * " <> y c <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding booleans}
\begin{code}
class BoolSym repr where
  bool :: Bool -> repr Bool
  lte :: repr Int -> repr Int -> repr Bool
  when :: repr Bool -> repr a -> repr a -> repr a
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding booleans}
\begin{code}
instance BoolSym R where
  bool = R
  lte (R x) (R y) = R $ x <= y
  when (R b) (R t) (R f) = R $ if b then t else f
\end{code}
\pause
\begin{code}
instance BoolSym S where
  bool = S . const . show
  lte (S x) (S y) = S $ \c ->
    "(" <> x c <> " <= " <> y c <> ")"
  when (S b) (S t) (S f) = S $ \c ->
    "if " <> b c
    <> "\nthen " <> t c
    <> "\n else" <> f c
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding lambda abstraction}
\begin{code}
class LamSym repr where
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding lambda abstraction}
\begin{code}
instance LamSym R where
  lam f = R $ unR . f . R
  app (R f) (R a) = R $ f a
\end{code}
\pause
\begin{code}
instance LamSym S where
  lam f = S $ \c ->
    let
      x = "x" <> show c
    in
      "\\" <> x <>".\n"
      <> (unS . f . S $ const x) (c + 1)
  app (S f) (S a) = S $ \c ->
    "(" <> f c <> " " <> a c <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding recursion!!}
\begin{code}
class FixSym repr where
  fix :: (repr a -> repr a) -> repr a
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding recursion!!}
\begin{code}
instance FixSym R where
  fix f = R $ fx (unR . f . R)
    where fx g = g (fx g)
\end{code}
\pause
\begin{code}
instance FixSym S where
  fix f = S $ \c ->
    let
      self = "self" <> show c
    in
      "(fix " <> self <> ".\n"
      <> (unS . f . S $ const self) (c + 1) <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{simply typed lambda calculus with integer and boolean literals}
\begin{code}
class ( AddSym repr
      , MultSym repr
      , BoolSym repr
      , LamSym repr
      , FixSym repr
      ) => Symantics repr

instance Symantics R
instance Symantics S

eval :: R a -> a
eval = unR

pprint :: S a -> String
pprint e = unS e 0
\end{code}
\end{frame}

\begin{frame}
\frametitle{factorial}
\begin{code}
factorial :: (Symantics repr) => repr (Int -> Int)
factorial = fix (\self -> lam (\n ->
              when (lte n (int 0))
                   (int 1)
                   (mul n (self `app` (add n (int (-1)))))))
\end{code}
(hint: now's a good time to \code{stack ghci src/TTFI.hs})
\end{frame}
\end{document}
