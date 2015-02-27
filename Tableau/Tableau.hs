-- Simple theorem prover for PROP
module Tableau where

import Data.List     (nub)
import Control.Monad (msum)

data Formula = Atomic      Char
             | Negation    Formula
             | Conjunction Formula Formula
             | Disjunction Formula Formula
             | Implication Formula Formula
             | Equivalence Formula Formula
             deriving Eq

instance Show Formula where
    show (Atomic      c    ) = [c]
    show (Negation    f1   ) = "~" ++ (show f1)
    show (Conjunction f1 f2) = '(':(show f1) ++ "^"  ++ (show f2) ++ ")"
    show (Disjunction f1 f2) = '(':(show f1) ++ "v"  ++ (show f2) ++ ")"
    show (Implication f1 f2) = '(':(show f1) ++ "->" ++ (show f2) ++ ")"
    show (Equivalence f1 f2) = '(':(show f1) ++ "="  ++ (show f2) ++ ")"

data Tree a  = Leaf         a
             | SingleBranch a (Tree a)
             | MultiBranch  a (Tree a) (Tree a)
             deriving (Eq)

instance Show a => Show (Tree a) where
    show t = schau 0 t 
      where
        schau :: Show a => Int -> Tree a -> String
        schau n (Leaf a)               = "L: " ++ (show a)
        schau n (SingleBranch f t)     = (show f) ++ " * " ++ (schau (n+3+(length (show f))) t) 
        schau n (MultiBranch  f t0 t1) = (show f) ++ " # " ++ (schau (n+3+(length (show f))) t0) 
                                       ++ "\n" ++ (replicate (n+(length (show f))) ' ') ++ " # " ++ (schau (n+3+(length (show f))) t1)
type Path = []
type CounterExample = Path Formula

-- corresponds to (the negation of) the following: (A->B), (B->C) |- (A->C) 
-- Since this is a tautology, the code should find a counterexample (and thereby prove the original)
example1 :: Formula
example1 = Conjunction (Conjunction (Implication (Atomic 'A') (Atomic 'B')) (Implication (Atomic 'B') (Atomic 'C'))) (Negation (Implication (Atomic 'A') (Atomic 'C')))

-- corresponds to (the negation of) the following: (P = Q) & (P -> (Not Q))
-- This is not a tautology, the code should be unable to find a counterexample (and hence fail to prove the original)
example2 :: Formula
example2 = Conjunction (Equivalence (Atomic 'P') (Atomic 'Q')) (Implication (Atomic 'P') (Negation (Atomic 'Q')))

-- corresponds to (the negation of) the following: ((P & Q) v (Not P & Not Q)) |- (P = Q)
example3 :: Formula
example3 = Conjunction (Disjunction (Conjunction (Atomic 'P') (Atomic 'Q')) (Conjunction (Negation (Atomic 'P')) (Negation (Atomic 'Q')))) (Negation (Equivalence (Atomic 'P') (Atomic 'Q')))



trp :: Tree a -> Tree a -> Tree a
trp (Leaf f)               tn = SingleBranch f tn
trp (SingleBranch f t0)    tn = SingleBranch f (t0 `trp` tn)
trp (MultiBranch  f t0 t1) tn = MultiBranch  f (t0 `trp` tn) (t1 `trp` tn)

treeify :: Formula -> Tree Formula 
treeify f@(Atomic c)        = Leaf f
treeify f@(Conjunction p q) = (treeify p) `trp` (treeify q)
treeify f@(Disjunction p q) = MultiBranch f (treeify p) (treeify q)
treeify f@(Implication p q) = MultiBranch f (treeify (Negation p)) (treeify q) 
treeify f@(Equivalence p q) = MultiBranch f (treeify p `trp` treeify q) (treeify (Negation p) `trp` treeify (Negation q))
treeify f@(Negation n)      = case n of
  (Atomic      c  ) -> Leaf (Negation (Atomic c))
  (Negation    p  ) -> treeify p
  (Conjunction p q) -> MultiBranch f (treeify (Negation p)) (treeify (Negation q))
  (Disjunction p q) -> (treeify (Negation p)) `trp` (treeify (Negation q))
  (Implication p q) -> (treeify p) `trp` (treeify q)
  (Equivalence p q) -> MultiBranch f ((treeify (Negation p)) `trp` (treeify q)) ((treeify p) `trp` (treeify (Negation q)))

pathify :: Tree Formula -> [Path Formula]
pathify (Leaf (Atomic c))                          = [[(Atomic c)]]
pathify (Leaf (Negation (Atomic c)))               = [[(Negation (Atomic c))]]
pathify (Leaf _)                                   = error "Non-atomic Leaf"
pathify (SingleBranch (Atomic c) t)                = map ((Atomic c):) (pathify t)  
pathify (SingleBranch (Negation (Atomic c)) t)     = map ((Negation (Atomic c)):) (pathify t) 
pathify (SingleBranch _          t)                = pathify t
pathify (MultiBranch  (Atomic c) t0 t1)            = (map ((Atomic c):) (pathify t0)) ++ (map ((Atomic c):) (pathify t1))
pathify (MultiBranch  (Negation (Atomic c)) t0 t1) = (map ((Negation (Atomic c)):) (pathify t0)) ++ (map ((Negation (Atomic c)):) (pathify t1))
pathify (MultiBranch  _          t0 t1)            = (pathify t0) ++ (pathify t1)

checkPath :: Path Formula -> Maybe (Path Formula)
checkPath p = if all (isnotnegatedin p) p then Just p else Nothing
  where
    isnotnegatedin :: [Formula] -> Formula -> Bool
    isnotnegatedin xs (Negation x) = notElem x            xs
    isnotnegatedin xs f@(Atomic c) = notElem (Negation f) xs

prove :: Formula -> String
prove f = case (msum . (fmap checkPath) . (fmap nub) . pathify . treeify . neg) f of Nothing  -> "Tautology!"
                                                                                     (Just x) -> ("Not a tautology: " ++ show x)
 
neg :: Formula -> Formula
neg f@(Atomic c)        = Negation f
neg f@(Negation n)      = Negation f
neg   (Conjunction p q) = Disjunction (Negation p) (Negation q)
neg   (Disjunction p q) = Conjunction (Negation p) (Negation q)
neg   (Implication p q) = Disjunction (Negation p) q
neg   (Equivalence p q) = Disjunction (Conjunction p q) (Conjunction (Negation p) (Negation q))
