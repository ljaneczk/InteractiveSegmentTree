module SegmentTree (
	findSegmentTreeSize,
	initialize,
	insertInitialNumbers,
	updateSetValueOnInterval,
	updateAddValueOnInterval,
	queryMinimumOnInterval,
	queryMaximumOnInterval,
	querySumeOnInterval,
	getPartOfSegmentTree,
	show',
	SegmentTree(..)
) where

import Data.Int
import Data.Bool

type Val = Int			

--SegmentTree = nullLeaf | Node leftChild rightChild leftBound rightBound minimum maximum sume modifier
data SegmentTree = Null | Node SegmentTree SegmentTree Int Int Val Val Val Val Bool deriving Show


findPowerOfTwo :: Int -> Int -> Int
findPowerOfTwo s n | s >= n = s
findPowerOfTwo s n = findPowerOfTwo (2 * s) n

findSegmentTreeSize :: Int -> Int
findSegmentTreeSize n = findPowerOfTwo 1 n


--Constant functions returning neutral values for operations on SegmentTree

zero :: Val
zero = 0

minusInf :: Val
minusInf = (-1) * 2^63

plusInf :: Val
plusInf = 2^63 - 1



--These functions return values stored in SegmentTree (similar to getters in Java)

getLeftChild :: SegmentTree -> SegmentTree
getLeftChild t @ (Node lef _ _ _ _ _ _ _ _)	= lef

getRightChild :: SegmentTree -> SegmentTree
getRightChild t @ (Node _ rig _ _ _ _ _ _ _) = rig

getLeftBound :: SegmentTree -> Int
getLeftBound t @ (Node _ _ b _ _ _ _ _ _) = b

getRightBound :: SegmentTree -> Int
getRightBound t @ (Node _ _ _ e _ _ _ _ _) = e

getMinimum :: SegmentTree -> Val
getMinimum t @ (Node _ _ _ _ min _ _ _ _) = min

getMaximum :: SegmentTree -> Val
getMaximum t @ (Node _ _ _ _ _ max _ _ _) = max

getSume :: SegmentTree -> Val
getSume t @ (Node _ _ _ _ _ _ sum _ _) = sum

getModifier :: SegmentTree -> Val
getModifier t @ (Node _ _ _ _ _ _ _ mod _) = mod

getEqualAll :: SegmentTree -> Bool
getEqualAll t @ (Node _ _ _ _ _ _ _ _ equ) = equ



--Operations which pass on modifiers from node to its children - modifier of sum and modifier of equality

pushModifierOfSumToChild :: Val -> SegmentTree -> SegmentTree

pushModifierOfSumToChild _ Null = Null

pushModifierOfSumToChild modifier t | (modifier == zero) = t

pushModifierOfSumToChild modifier (Node lef rig b e min max sum mod equ) = let
	newMin = min + modifier
	newMax = max + modifier
	newSum = sum + (e - b) * modifier
	newMod = mod + modifier
	in (Node lef rig b e newMin newMax newSum newMod equ)



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




--Operations done on segment tree

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



insertInitialNumbers :: Int -> Int -> [Val] -> SegmentTree -> SegmentTree
insertInitialNumbers _ _ [] t = t
insertInitialNumbers ind n _ t | (ind >= n) = t
insertInitialNumbers ind n (x:xs) t =
		insertInitialNumbers (ind + 1) n xs (updateSetValueOnInterval ind (ind + 1) x t)



updateAddValueOnInterval :: Int -> Int -> Val -> SegmentTree -> SegmentTree
updateAddValueOnInterval l r dif = updateAddValueOnInterval'
		where
			updateAddValueOnInterval' Null = Null

			updateAddValueOnInterval' t @ (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = t					--Intervals [l,r) and [b,e) do not intersect

			updateAddValueOnInterval' (Node lef rig b e min max sum mod equ) | (l <= b && e <= r) = let		--Interval [b,e) is included in interval [l,r)
				newMin = min + dif
				newMax = max + dif
				newMod = mod + dif
				newSum = sum + (e - b) * dif
				in (Node lef rig b e newMin newMax newSum newMod equ)

			updateAddValueOnInterval' t @ (Node lef rig b e min max sum mod equ) = let			--[l,r) and [b,e) intersect and all values on [b,e) are equal
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



updateSetValueOnInterval :: Int -> Int -> Val -> SegmentTree -> SegmentTree
updateSetValueOnInterval l r value = updateSetValueOnInterval'
		where
			updateSetValueOnInterval' Null = Null

			updateSetValueOnInterval' t @ (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = t					--Intervals [l,r) and [b,e) do not intersect

			updateSetValueOnInterval' (Node lef rig b e min max sum mod equ) | (l <= b && e <= r) = let		--Interval [b,e) is included in interval [l,r)
				newMin = value
				newMax = value
				newSum = (e - b) * value
				newMod = zero
				newEqu = True
				in (Node lef rig b e newMin newMax newSum newMod newEqu)

			updateSetValueOnInterval' t @ (Node lef rig b e min max sum mod equ) = let			--[l,r) and [b,e) intersect and all values on [b,e) are equal
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


--In queries it is assumed that modifiers have been pushed to children before calling a function.

queryMinimumOnInterval :: Int -> Int -> SegmentTree -> Val
queryMinimumOnInterval l r = queryMinimumOnInterval'
		where
			queryMinimumOnInterval' Null = plusInf

			queryMinimumOnInterval' (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = plusInf				--Intervals [l,r) and [b,e) do not intersect

			queryMinimumOnInterval' (Node _ _ b e min _ _ _ _) | (l <= b && e <= r) = min				--Interval [b,e) is included in interval [l,r)

			queryMinimumOnInterval' (Node lef rig _ _ _ _ _ _ _) = 								--[l,r) and [b,e) intersect and all values on [b,e) are equal
				minimum [(queryMinimumOnInterval l r lef), (queryMinimumOnInterval l r rig)]



queryMaximumOnInterval :: Int -> Int -> SegmentTree -> Val
queryMaximumOnInterval l r = queryMaximumOnInterval'
		where
			queryMaximumOnInterval' Null = minusInf

			queryMaximumOnInterval' (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = minusInf			--Intervals [l,r) and [b,e) do not intersect

			queryMaximumOnInterval' (Node _ _ b e _ max _ _ _) | (l <= b && e <= r) = max				--Interval [b,e) is included in interval [l,r)

			queryMaximumOnInterval' (Node lef rig _ _ _ _ _ _ _) = 								--[l,r) and [b,e) intersect and all values on [b,e) are equal
				maximum [(queryMaximumOnInterval l r lef), (queryMaximumOnInterval l r rig)]



querySumeOnInterval :: Int -> Int -> SegmentTree -> Val
querySumeOnInterval l r = querySumeOnInterval'
		where
			querySumeOnInterval' Null = zero

			querySumeOnInterval' (Node _ _ b e _ _ _ _ _) | (e <= l || r <= b) = zero				--Intervals [l,r) and [b,e) do not intersect

			querySumeOnInterval' (Node _ _ b e _ _ sum _ _) | (l <= b && e <= r) = sum				--Interval [b,e) is included in interval [l,r)

			querySumeOnInterval' (Node lef rig _ _ _ _ _ _ _) = 								--[l,r) and [b,e) intersect and all values on [b,e) are equal
				(querySumeOnInterval l r lef) + (querySumeOnInterval l r rig)






--With these functions the values stored in SegmentTree can be printed in sensible way

getPartOfSegmentTree :: Int -> Int -> SegmentTree -> [Val]
getPartOfSegmentTree b e t = do
	[(queryMaximumOnInterval i (i+1) (updateAddValueOnInterval i (i+1) zero t))  | i <- [b..(e-1)]]


show'' :: SegmentTree -> String
show'' Null = "Null"
show'' (Node lef rig b e min max sum mod equ) =
	"Node " ++ show b ++ " " ++ show e ++ "  " ++ show min ++ " " ++ show max ++ " " ++ show sum ++ "  " ++ show mod ++ " " ++ show equ
	++ "\n" ++ show'' lef ++ "\n" ++ show'' rig


show' :: SegmentTree -> IO()
show' t = putStrLn (show'' t)
