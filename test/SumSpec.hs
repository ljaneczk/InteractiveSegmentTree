module SumSpec
    (
        testsSumSpec
    ) where

import Test.HUnit
import SegmentTree

t = initialize 0 20

test1 = TestCase (assertEqual "sum on empty tree 1" 0 (querySumOnInterval 0 20 t))
test2 = TestCase (assertEqual "sum on empty tree 2" 0 (querySumOnInterval 0 1 t))
test3 = TestCase (assertEqual "sum on empty tree 3" 0 (querySumOnInterval 0 8 t))
test4 = TestCase (assertEqual "sum on empty tree 4" 0 (querySumOnInterval 10 11 t))
test5 = TestCase (assertEqual "sum on empty tree 5" 0 (querySumOnInterval 4 7 t))
test6 = TestCase (assertEqual "sum on empty tree 6" 0 (querySumOnInterval 8 16 t))
test7 = TestCase (assertEqual "sum on empty tree 7" 0 (querySumOnInterval 5 11 t))
test8 = TestCase (assertEqual "sum on empty tree 8" 0 (querySumOnInterval 19 20 t))
test9 = TestCase (assertEqual "sum on empty tree 9" 0 (querySumOnInterval 17 20 t))
test10 = TestCase (assertEqual "sum on empty tree 10" 0 (querySumOnInterval 5 15 t))

t1 = updateSetValueOnInterval 0 20 3 t

test11 = TestCase (assertEqual "sum on medium tree 11" 60 (querySumOnInterval 0 20 t1))

t2 = initialize 0 4
t3 = updateSetValueOnInterval 0 1 7 t2
t4 = updateSetValueOnInterval 1 2 9 t3
t5 = updateSetValueOnInterval 2 3 13 t4
t6 = updateSetValueOnInterval 3 4 6 t5

test12 = TestCase (assertEqual "sum on small tree 12" 7 (querySumOnInterval 0 1 t6))
test13 = TestCase (assertEqual "sum on small tree 13" 9 (querySumOnInterval 1 2 t6))
test14 = TestCase (assertEqual "sum on small tree 14" 13 (querySumOnInterval 2 3 t6))
test15 = TestCase (assertEqual "sum on small tree 15" 6 (querySumOnInterval 3 4 t6))
test16 = TestCase (assertEqual "sum on small tree 16" 16 (querySumOnInterval 0 2 t6))
test17 = TestCase (assertEqual "sum on small tree 17" 22 (querySumOnInterval 1 3 t6))
test18 = TestCase (assertEqual "sum on small tree 18" 19 (querySumOnInterval 2 4 t6))
test19 = TestCase (assertEqual "sum on small tree 19" 29 (querySumOnInterval 0 3 t6))
test20 = TestCase (assertEqual "sum on small tree 20" 28 (querySumOnInterval 1 4 t6))
test21 = TestCase (assertEqual "sum on small tree 21" 35 (querySumOnInterval 0 4 t6))

t10 = initialize 0 131072
t11 = updateSetValueOnInterval 0 65536 10 t10

test22 = TestCase (assertEqual "sum on big tree 22" 655360 (querySumOnInterval 0 131072 t11))
test23 = TestCase (assertEqual "sum on big tree 23" 655360 (querySumOnInterval 0 65536 t11))
test24 = TestCase (assertEqual "sum on big tree 24" 655360 (querySumOnInterval 0 100000 t11))

t12 = updateSetValueOnInterval 65536 131072 20 t11
test25 = TestCase (assertEqual "sum on big tree 25" (655360 + 1310720) (querySumOnInterval 0 131072 t12))

testsSumSpec = [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25]
