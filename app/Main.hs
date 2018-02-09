{-|
Module      : Main
Description : Module responsible for interaction with user.

Module contains two versions of communication with user. The version should be estimated before running the project.
The first option (by implication) is mainWithMessages, which enables communication and instrucing user.
The second option is mainWithoutMessages, with this option you can work without neccessary coments.
This option might be helpful when you are going to use piping in console.

Some functions may use type Val = Int to differentiate values stored in SegmentTree from Int used to specify boundaries and size.
-}

module Main(main,mainWithMessages,mainWithoutMessages,interactWithUser) where

import Data.String
import Data.List

import SegmentTree



type Val = Int


-- | This function checks if interval given by user is correct.
isIntervalIncorrect :: Int -> [Int] -> Bool
isIntervalIncorrect _ x | ((length x) < 2) = True
isIntervalIncorrect n (x1:x2:x3) = not (0 <= x1 && x1 <= x2 && x2 <= n)


-- | This function proceeds users' commands.
interactWithUser :: Int -> SegmentTree -> IO ()
interactWithUser n = interactWithUser'
	where interactWithUser' t1 = do
		input <- getLine
		if (input == ":q" || input == "q" || input == "quit" || input == "exit")
			then putStr ""
			else do
				let beg = take 3 input
				let end = drop 3 input
				let args = map read (words end) :: [Int]
				if (isIntervalIncorrect n args)
					then do
						putStrLn "Invalid interval - please ensure that interval is included in SegmentTree boundaries."
						interactWithUser' t1
					else do
						case beg of
							"set" -> do
								if ((length args) < 3)
									then do
										putStrLn "Too few arguments for set update."
										interactWithUser' t1
									else do
										let (a1:a2:a3:a4) = args
										let t2 = updateSetValueOnInterval a1 a2 a3 t1
										interactWithUser' t2
							"add" -> do
								if ((length args) < 3)
									then do
										putStrLn "Too few arguments for add update."
										interactWithUser' t1
									else do
										let (a1:a2:a3:a4) = args
										let t2 = updateAddValueOnInterval a1 a2 a3 t1
										interactWithUser' t2
							"min" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for min query."
										interactWithUser' t1
									else do
										let (a1:a2:a3) = args
										let t2 = updateAddValueOnInterval a1 a2 0 t1
										print (queryMinimumOnInterval a1 a2 t2)
										interactWithUser' t2
							"max" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for max query."
										interactWithUser' t1
									else do
										let (a1:a2:a3) = args
										let t2 = updateAddValueOnInterval a1 a2 0 t1
										print (queryMaximumOnInterval a1 a2 t2)
										interactWithUser' t2
							"sum" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for sum query."
										interactWithUser' t1
									else do
										let (a1:a2:a3) = args
										let t2 = updateAddValueOnInterval a1 a2 0 t1
										print (querySumOnInterval a1 a2 t2)
										interactWithUser' t2
							"get" -> do
								if ((length args) < 2)
									then do
										putStrLn "Too few arguments for tre query."
										interactWithUser' t1
									else do
										let (a1:a2:a3) = args
										let l1 = getPartOfSegmentTree a1 a2 t1
										print l1
										interactWithUser' t1
							otherwise -> do
								putStrLn "Invalid operation - check your operation and arguments."
								interactWithUser' t1



-- | This is main function. In this function the appropriate version of cummication with user is called.
main :: IO ()
main = mainWithMessages


-- | This function communicates with user and instructs them.
mainWithMessages :: IO ()
mainWithMessages = do
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
		putStrLn ("When you want to close program, type one of [q,:q,quit,exit] and press enter.")
		interactWithUser n1 t1
		putStrLn "See you next time!"


-- | This function communicates with user without instruction.
mainWithoutMessages :: IO ()
mainWithoutMessages = do
	x <- getLine
	let (n1:p) = map read (words x) :: [Int]
	let n = findSegmentTreeSize n1
	input <- getLine
	let initials = map read (words input) :: [Val]
	let t0 = initialize 0 n
	let t1 = insertInitialNumbers 0 n initials t0
	interactWithUser n1 t1
