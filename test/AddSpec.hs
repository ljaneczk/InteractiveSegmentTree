module AddSpec
    (
        isAddFunctionCorrect
    ) where

import Test.QuickCheck
import SegmentTree

type Val = Int

isAddFunctionCorrect :: [Val] -> Int -> Int -> Val -> Bool
isAddFunctionCorrect x l r v | (x == []) = True
isAddFunctionCorrect x l r v = let
    n1 = length x
    l1 = max (min (min l r) (n1 - 1)) 0
    r1 = min (max (max l r) (l1 + 1)) n1
    n2 = findSegmentTreeSize n1
    t0 = initialize 0 n2
    t1 = updateAddValueOnInterval l1 r1 0 (insertInitialNumbers 0 n2 x t0)
    t2 = updateAddValueOnInterval l1 r1 v t1
    in ( ((queryMinimumOnInterval l1 r1 t2) == (queryMinimumOnInterval l1 r1 t1) + v) &&
         ((queryMaximumOnInterval l1 r1 t2) == (queryMaximumOnInterval l1 r1 t1) + v) &&
         ((querySumOnInterval l1 r1 t2) == (querySumOnInterval l1 r1 t1) + (r1 - l1) * v) )
