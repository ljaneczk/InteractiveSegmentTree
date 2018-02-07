module GetSpec
    (
        isGetFunctionCorrect
    ) where

import Test.QuickCheck
import SegmentTree

type Val = Int

isGetFunctionCorrect :: [Val] -> Bool
isGetFunctionCorrect x = let
    n1 = length x
    n2 = findSegmentTreeSize n1
    t1 = initialize 0 n2
    t2 = insertInitialNumbers 0 n2 x t1
    in ((getPartOfSegmentTree 0 n1 t2) == x)
