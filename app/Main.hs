module Main(main,mainWithoutMessages) where

import Data.String
import Data.List

import SegmentTree
	--findSegmentTreeSize
	--initialize,
	--insertInitialNumbers,
	--updateSetValueOnInterval,
	--updateAddValueOnInterval,
	--queryMinimumOnInterval,
	--queryMaximumOnInterval,
	--querySumeOnInterval,
	--getPartOfSegmentTree,
	--show',
	--SegmentTree(..)


type Val = Int



isIntervalIncorrect :: Int -> [Int] -> Bool
isIntervalIncorrect _ x | ((length x) < 2) = True
isIntervalIncorrect n (x1:x2:x3) = not (0 <= x1 && x1 < x2 && x2 <= n)



interactWithUser' :: Int -> SegmentTree -> IO ()
interactWithUser' n = interactWithUser
	where interactWithUser t1 = do
		input <- getLine
		if (input == ":q" || input == "q" || input == "quit" || input == "exit")
			then putStrLn "See you next time!"
			else do
				let beg = take 3 input
				let end = drop 3 input
				let args = map read (words end) :: [Int]
				if (isIntervalIncorrect n args)
					then do
						putStrLn "Invalid interval - please ensure that your interval is included in SegmentTree boundaries."
						interactWithUser t1
					else do
						case beg of
							"set" -> do
								if ((length args) < 3)
									then do
										putStrLn "Too few arguments for set update."
										interactWithUser t1
									else do
										let (a1:a2:a3:a4) = args
										let t2 = updateSetValueOnInterval a1 a2 a3 t1
										interactWithUser t2
							"add" -> do
								if ((length args) < 3)
									then do
										putStrLn "Too few arguments for add update."
										interactWithUser t1
									else do
										let (a1:a2:a3:a4) = args
										let t2 = updateAddValueOnInterval a1 a2 a3 t1
										interactWithUser t2
							"min" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for min query."
										interactWithUser t1
									else do
										let (a1:a2:a3) = args
										let t2 = updateAddValueOnInterval a1 a2 0 t1
										print (queryMinimumOnInterval a1 a2 t2)
										interactWithUser t2
							"max" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for max query."
										interactWithUser t1
									else do
										let (a1:a2:a3) = args
										let t2 = updateAddValueOnInterval a1 a2 0 t1
										print (queryMaximumOnInterval a1 a2 t2)
										interactWithUser t2
							"sum" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for sum query."
										interactWithUser t1
									else do
										let (a1:a2:a3) = args
										let t2 = updateAddValueOnInterval a1 a2 0 t1
										print (querySumeOnInterval a1 a2 t2)
										interactWithUser t2
							"tre" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for tre query."
										interactWithUser t1
									else do
										let (a1:a2:a3) = args
										let l1 = getPartOfSegmentTree a1 a2 t1
										print l1
										interactWithUser t1
							otherwise -> do
								putStrLn "Invalid operation - check your operation and arguments."
								interactWithUser t1




main :: IO ()
main = do
	putStrLn "Please give one number: n - number of numbers stored in segment tree."
	x <- getLine
	let (n1:p) = map read (words x) :: [Int]
	let n = findSegmentTreeSize n1
	putStrLn ("Please give <= " ++ show n1 ++ " initial numbers in tree - values in tree given from beginning.")
	input <- getLine
	let initials = map read (words input) :: [Val]
	let t0 = initialize 0 n
	let t1 = insertInitialNumbers 0 n initials t0
	putStrLn ("Please introduce operations on intervals included in [0," ++ show n1 ++ ").")
	putStrLn ("Remember that intervals are in format [beg,end), where beg >= 0 and end <= " ++ show n1 ++ ".")
	interactWithUser' n1 t1





mainWithoutMessages :: IO ()
mainWithoutMessages = do
	x <- getLine
	let (n1:p) = map read (words x) :: [Int]
	let n = findSegmentTreeSize n1
	input <- getLine
	let initials = map read (words input) :: [Val]
	let t0 = initialize 0 n
	let t1 = insertInitialNumbers 0 n initials t0
	interactWithUser' n1 t1
