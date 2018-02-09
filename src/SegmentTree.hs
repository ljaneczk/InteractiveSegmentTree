{-|
Module      : SegmentTree
Description : Module delivering complete structure of semgent tree with useful operations.

Module delivers complete structure of semgent tree with listed below useful operations:

set b e v - change values on interval [b,e) into v

add b e v - add value v to all numbers on interval [b,e)

sum b e - return sum of numbers on interval [b,e)

min b e - return minimum number on interval [b,e)

max b e - return maximum number on interval [b,e)

get b e - print numbers on interval [b,e) in list format

Some functions may use type Val = Int to differentiate values stored in SegmentTree from Int used to specify boundaries and size.
-}


module SegmentTree (
	SegmentTree(..),
	findSegmentTreeSize,
	initialize,
	insertInitialNumbers,
	updateSetValueOnInterval,
	updateAddValueOnInterval,
	queryMinimumOnInterval,
	queryMaximumOnInterval,
	querySumOnInterval,
	getPartOfSegmentTree,
	zero,
	minusInf,
	plusInf,
	getLeftChild,
	getRightChild,
	getLeftBound,
	getRightBound,
	getMinimum,
	getMaximum,
	getSume,
	getModifierOfSum,
	getModifierOfEquality
) where

import Data.Int
import Data.Bool


type Val = Int

-- | SegmentTree is either NullLeaf or Node containing two children, own boundaries and values to compute answers for queries effectively
data SegmentTree = Null -- ^ NullLeaf
								  | Node SegmentTree SegmentTree Int Int Val Val Val Val Bool -- ^ Node leftChild rightChild leftBound rightBound minimum maximum sume (modifier of sum) (modifier of equality)




-- | This function returns optimal size of segment tree - optimal is in case of power of two.
findSegmentTreeSize :: Int -> Int
findSegmentTreeSize n = findPowerOfTwo 1 n
		where
			findPowerOfTwo s n | (s >= n) = s
			findPowerOfTwo s n = findPowerOfTwo (2 * s) n




-- Constant functions which return neutral values for operations on SegmentTree.


-- | This function returns 'zero' of operations on SegmentTree
zero :: Val
zero = 0

-- | This function returns minimum value (minus infinity) of operations on SegmentTree
minusInf :: Val
minusInf = (-1) * 2^63

-- | This function returns maximum value (plus infinity) of operations on SegmentTree
plusInf :: Val
plusInf = 2^63 - 1




-- These functions return values stored in SegmentTree (similar to getters).

-- | This function returns LeftChild of specified Node of SegmentTree
getLeftChild :: SegmentTree -> SegmentTree
getLeftChild t @ (Node lef _ _ _ _ _ _ _ _)	= lef

-- | This function returns RightChild of specified Node of SegmentTree
getRightChild :: SegmentTree -> SegmentTree
getRightChild t @ (Node _ rig _ _ _ _ _ _ _) = rig

-- | This function returns LeftBound of specified Node of SegmentTree
getLeftBound :: SegmentTree -> Int
getLeftBound t @ (Node _ _ b _ _ _ _ _ _) = b

-- | This function returns RightBound of specified Node of SegmentTree
getRightBound :: SegmentTree -> Int
getRightBound t @ (Node _ _ _ e _ _ _ _ _) = e

-- | This function returns minimum on interval of specified Node of SegmentTree
getMinimum :: SegmentTree -> Val
getMinimum t @ (Node _ _ _ _ min _ _ _ _) = min

-- | This function returns maximum on interval of specified Node of SegmentTree
getMaximum :: SegmentTree -> Val
getMaximum t @ (Node _ _ _ _ _ max _ _ _) = max

-- | This function returns sume of values on interval of specified Node of SegmentTree
getSume :: SegmentTree -> Val
getSume t @ (Node _ _ _ _ _ _ sum _ _) = sum

-- | This function returns modifier of sume of values on interval of specified Node of SegmentTree
getModifierOfSum :: SegmentTree -> Val
getModifierOfSum t @ (Node _ _ _ _ _ _ _ mod _) = mod

-- | This function returns modifier of equality of values on interval of specified Node of SegmentTree
getModifierOfEquality :: SegmentTree -> Bool
getModifierOfEquality t @ (Node _ _ _ _ _ _ _ _ equ) = equ




-- | This function pushes modifier of sum from node to its children.
pushModifierOfSumToChild :: Val -> SegmentTree -> SegmentTree
pushModifierOfSumToChild _ Null = Null
pushModifierOfSumToChild modifier t | (modifier == zero) = t
pushModifierOfSumToChild modifier (Node lef rig b e min max sum mod equ) = let
	newMin = min + modifier
	newMax = max + modifier
	newSum = sum + (e - b) * modifier
	newMod = mod + modifier
	in (Node lef rig b e newMin newMax newSum newMod equ)


-- | This function pushes modifier of equality from node to its children.
pushModifierOfEqualityToChild :: Bool -> Val -> SegmentTree -> SegmentTree
pushModifierOfEqualityToChild equal _ t | (equal == False) = t
pushModifierOfEqualityToChild _ _ Null = Null
pushModifierOfEqualityToChild _ value (Node lef rig b e min max sum mod equ) = let
	newMin = value
	newMax = value
	newSum = (e - b) * value
	newMod = zero
	newEqu = True
	in (Node lef rig b e newMin newMax newSum newMod newEqu)


-- | This function pushes modifiers from node to its children - modifier of sum and modifier of equality.
pushModifiersToChildren :: SegmentTree -> SegmentTree
pushModifiersToChildren Null = Null
pushModifiersToChildren t @ (Node lef rig b e min max sum mod equ) = let
	lef1 = pushModifierOfSumToChild mod lef
	rig1 = pushModifierOfSumToChild mod rig
	newMod = zero
	lef2 = pushModifierOfEqualityToChild equ min lef1
	rig2 = pushModifierOfEqualityToChild equ min rig1
	newEqu = False
	in (Node lef2 rig2 b e min max sum newMod newEqu)




-- | This function creates SegmentTree of specified size.
initialize :: Int -> Int -> SegmentTree
initialize l r =
	if (r - l <= 1)
		then Node Null Null l r plusInf minusInf zero zero True
	else
		Node left right l r plusInf minusInf zero zero False
		where
			left = initialize l m
			right = initialize m r
			m	= (l + r) `div` 2


-- | This function inserts some values into SegmentTree, values are given from beginning (left).
insertInitialNumbers :: Int -> Int -> [Val] -> SegmentTree -> SegmentTree
insertInitialNumbers _ _ [] t = t
insertInitialNumbers ind n _ t | (ind >= n) = t
insertInitialNumbers ind n (x:xs) t =
		insertInitialNumbers (ind + 1) n xs (updateSetValueOnInterval ind (ind + 1) x t)




-- Operations executed on segment tree - all of them have O(log n) complexity, where n = size of tree.
-- In updates the structure is changed and due to it new structure is returned.


-- | This function updates values (adds given v to all values) on specified inverval in SegmentTree
updateAddValueOnInterval :: Int -> Int -> Val -> SegmentTree -> SegmentTree
updateAddValueOnInterval l r dif = updateAddValueOnInterval'
		where
			updateAddValueOnInterval' Null = Null

			updateAddValueOnInterval' t @ (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = t                   --Intervals [l,r) and [b,e) do not intersect

			updateAddValueOnInterval' (Node lef rig b e min max sum mod equ) | (l <= b && e <= r) = let       --Interval [b,e) is included in interval [l,r)
				newMin = min + dif
				newMax = max + dif
				newMod = mod + dif
				newSum = sum + (e - b) * dif
				in (Node lef rig b e newMin newMax newSum newMod equ)

			updateAddValueOnInterval' t @ (Node lef rig b e min max sum mod equ) = let                        --[l,r) and [b,e) intersect and all values on [b,e) are equal
				t1 = pushModifiersToChildren t
				lef1 = getLeftChild t1
				rig1 = getRightChild t1
				newLef = updateAddValueOnInterval l r dif lef1
				newRig = updateAddValueOnInterval l r dif rig1
				newMin = minimum [getMinimum newLef, getMinimum newRig]
				newMax = maximum [getMaximum newLef, getMaximum newRig]
				newSum = (getSume newLef) + (getSume newRig)
				newMod = zero
				newEqu = False
				in (Node newLef newRig b e newMin newMax newSum newMod newEqu)



-- | This function updates values (changes all values into given v) on specified inverval in SegmentTree
updateSetValueOnInterval :: Int -> Int -> Val -> SegmentTree -> SegmentTree
updateSetValueOnInterval l r value = updateSetValueOnInterval'
		where
			updateSetValueOnInterval' Null = Null

			updateSetValueOnInterval' t @ (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = t                   --Intervals [l,r) and [b,e) do not intersect

			updateSetValueOnInterval' (Node lef rig b e min max sum mod equ) | (l <= b && e <= r) = let       --Interval [b,e) is included in interval [l,r)
				newMin = value
				newMax = value
				newSum = (e - b) * value
				newMod = zero
				newEqu = True
				in (Node lef rig b e newMin newMax newSum newMod newEqu)

			updateSetValueOnInterval' t @ (Node lef rig b e min max sum mod equ) = let                        --[l,r) and [b,e) intersect and all values on [b,e) are equal
				t1 = pushModifiersToChildren t
				lef1 = getLeftChild t1
				rig1 = getRightChild t1
				newLef = updateSetValueOnInterval l r value lef1
				newRig = updateSetValueOnInterval l r value rig1
				newMin = minimum [getMinimum newLef, getMinimum newRig]
				newMax = maximum [getMaximum newLef, getMaximum newRig]
				newSum = (getSume newLef) + (getSume newRig)
				newMod = zero
				newEqu = False
				in (Node newLef newRig b e newMin newMax newSum newMod newEqu)




-- In queries it is assumed that modifiers have been pushed to children before calling the function.


-- | This function returns maximum value on specified inverval in SegmentTree
queryMinimumOnInterval :: Int -> Int -> SegmentTree -> Val
queryMinimumOnInterval l r = queryMinimumOnInterval'
		where
			queryMinimumOnInterval' Null = plusInf

			queryMinimumOnInterval' (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = plusInf                   --Intervals [l,r) and [b,e) do not intersect

			queryMinimumOnInterval' (Node _ _ b e min _ _ _ _) | (l <= b && e <= r) = min                     --Interval [b,e) is included in interval [l,r)

			queryMinimumOnInterval' (Node lef rig _ _ _ _ _ _ _) =                                            --[l,r) and [b,e) intersect and all values on [b,e) are equal
				minimum [(queryMinimumOnInterval l r lef), (queryMinimumOnInterval l r rig)]



-- | This function returns minimum value on specified inverval in SegmentTree
queryMaximumOnInterval :: Int -> Int -> SegmentTree -> Val
queryMaximumOnInterval l r = queryMaximumOnInterval'
		where
			queryMaximumOnInterval' Null = minusInf

			queryMaximumOnInterval' (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = minusInf                  --Intervals [l,r) and [b,e) do not intersect

			queryMaximumOnInterval' (Node _ _ b e _ max _ _ _) | (l <= b && e <= r) = max                     --Interval [b,e) is included in interval [l,r)

			queryMaximumOnInterval' (Node lef rig _ _ _ _ _ _ _) =                                            --[l,r) and [b,e) intersect and all values on [b,e) are equal
				maximum [(queryMaximumOnInterval l r lef), (queryMaximumOnInterval l r rig)]



-- | This function returns sume of values on specified inverval in SegmentTree
querySumOnInterval :: Int -> Int -> SegmentTree -> Val
querySumOnInterval l r = querySumOnInterval'
		where
			querySumOnInterval' Null = zero

			querySumOnInterval' (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = zero                          --Intervals [l,r) and [b,e) do not intersect

			querySumOnInterval' (Node _ _ b e _ _ sum _ _) | (l <= b && e <= r) = sum                         --Interval [b,e) is included in interval [l,r)

			querySumOnInterval' (Node lef rig _ _ _ _ _ _ _) =                                                --[l,r) and [b,e) intersect and all values on [b,e) are equal
				(querySumOnInterval l r lef) + (querySumOnInterval l r rig)





-- | With this function the values stored in SegmentTree or its structure can be printed in sensible way.
getPartOfSegmentTree :: Int -> Int -> SegmentTree -> [Val]
getPartOfSegmentTree b e t = do
	[(queryMaximumOnInterval i (i+1) (updateAddValueOnInterval i (i+1) zero t))  | i <- [b..(e-1)]]



-- | This function enables to convert structure of SegmentTree to String in clear way.
show' :: SegmentTree -> String
show' Null = "Null"
show' (Node lef rig b e min max sum mod equ) =
	"Node " ++ show b ++ " " ++ show e ++ "  " ++ show min ++ " " ++ show max ++ " " ++ show sum ++ "  " ++ show mod
	++ " " ++ show equ	++ "\n" ++ show' lef ++ "\n" ++ show' rig



-- | This instance enables to write out structure of SegmentTree in clear way.
instance Show (SegmentTree) where
	show (t) = show' t
