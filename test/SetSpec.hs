module SetSpec
    (
        isSetFunctionCorrect
    ) where

import Test.QuickCheck
import SegmentTree

type Val = Int

isSetFunctionCorrect :: Int -> Int -> Int -> Val -> Bool
isSetFunctionCorrect n l r v = let
    l1 = max (min l r) 0
    r1 = max (max l r + 1) 1
    n1 = max r1 n
    n2 = findSegmentTreeSize n1
    t1 = initialize 0 n2
    t2 = updateSetValueOnInterval l1 r1 v t1
    in ( ((queryMinimumOnInterval l1 r1 (updateAddValueOnInterval l1 r1 0 t2)) == v) &&
         ((queryMaximumOnInterval l1 r1 (updateAddValueOnInterval l1 r1 0 t2)) == v) &&
         ((querySumOnInterval l1 r1 (updateAddValueOnInterval l1 r1 0 t2)) == (r1 - l1) * v) )
