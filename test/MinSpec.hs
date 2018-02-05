module MinSpec
    (
        testsMinSpec
    ) where

import Test.HUnit
import SegmentTree

t = initialize 0 16
t01 = updateSetValueOnInterval 0 16 3 t
t02 = updateSetValueOnInterval 0 1 10 t01
t03 = updateSetValueOnInterval 2 3 11 t02
t04 = updateSetValueOnInterval 4 5 12 t03
t05 = updateSetValueOnInterval 6 7 13 t04
t06 = updateSetValueOnInterval 8 9 14 t05
t07 = updateSetValueOnInterval 10 11 15 t06
t08 = updateSetValueOnInterval 12 13 16 t07
t09 = updateSetValueOnInterval 14 15 17 t08



test1 = TestCase (assertEqual "min on specific tree 1" 3 (queryMinimumOnInterval 0 16 t01))
test2 = TestCase (assertEqual "min on specific tree 2" 10 (queryMinimumOnInterval 0 1 t09))
test3 = TestCase (assertEqual "min on specific tree 3" 3 (queryMinimumOnInterval 0 8 t09))
test4 = TestCase (assertEqual "min on specific tree 4" 15 (queryMinimumOnInterval 10 11 t09))
test5 = TestCase (assertEqual "min on specific tree 5" 3 (queryMinimumOnInterval 4 7 t09))
test6 = TestCase (assertEqual "min on specific tree 6" 3 (queryMinimumOnInterval 8 16 t09))
test7 = TestCase (assertEqual "min on specific tree 7" 3 (queryMinimumOnInterval 5 11 t09))
test8 = TestCase (assertEqual "min on specific tree 8" 3 (queryMinimumOnInterval 13 14 t09))
test9 = TestCase (assertEqual "min on specific tree 9" 17 (queryMinimumOnInterval 14 15 t09))
test10 = TestCase (assertEqual "min on specific tree 10" 14 (queryMinimumOnInterval 8 9 t09))

t1 = updateSetValueOnInterval 0 20 3 t

test11 = TestCase (assertEqual "min on medium tree 11" 3 (queryMinimumOnInterval 0 20 t1))

t2 = initialize 0 4
t3 = updateSetValueOnInterval 0 1 7 t2
t4 = updateSetValueOnInterval 1 2 9 t3
t5 = updateSetValueOnInterval 2 3 13 t4
t6 = updateSetValueOnInterval 3 4 6 t5

test12 = TestCase (assertEqual "min on small tree 12" 7 (queryMinimumOnInterval 0 1 t6))
test13 = TestCase (assertEqual "min on small tree 13" 9 (queryMinimumOnInterval 1 2 t6))
test14 = TestCase (assertEqual "min on small tree 14" 13 (queryMinimumOnInterval 2 3 t6))
test15 = TestCase (assertEqual "min on small tree 15" 6 (queryMinimumOnInterval 3 4 t6))
test16 = TestCase (assertEqual "min on small tree 16" 7 (queryMinimumOnInterval 0 2 t6))
test17 = TestCase (assertEqual "min on small tree 17" 9 (queryMinimumOnInterval 1 3 t6))
test18 = TestCase (assertEqual "min on small tree 18" 6 (queryMinimumOnInterval 2 4 t6))
test19 = TestCase (assertEqual "min on small tree 19" 7 (queryMinimumOnInterval 0 3 t6))
test20 = TestCase (assertEqual "min on small tree 20" 6 (queryMinimumOnInterval 1 4 t6))
test21 = TestCase (assertEqual "min on small tree 21" 6 (queryMinimumOnInterval 0 4 t6))

t10 = initialize 0 131072
t11 = updateSetValueOnInterval 0 65536 10 t10

test22 = TestCase (assertEqual "min on big tree 22" 10 (queryMinimumOnInterval 0 131072 t11))
test23 = TestCase (assertEqual "min on big tree 23" 10 (queryMinimumOnInterval 0 65536 t11))
test24 = TestCase (assertEqual "min on big tree 24" 10 (queryMinimumOnInterval 0 100000 t11))

t12 = updateSetValueOnInterval 65536 131072 5 t11
test25 = TestCase (assertEqual "min on big tree 25" 5 (queryMinimumOnInterval 0 131072 t12))

testsMinSpec = [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25]
