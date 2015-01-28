
module Utility.Rotate
	( rotate
	, shuffle
	, choose
	, swap
	) where

import System.Random

rotate :: Int -> [a] -> [a]
rotate n xs
	| n > ln || n < 0 = rotate (n `mod` ln) xs
	| otherwise = drop n xs ++ take n xs
		where ln = length xs

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle xs g = ((head xrs) : fst shf, snd shf)
	where
		shf = shuffle (tail xrs) (snd ng)
		xrs = rotate (fst ng) xs
		ng = randomR (0, (ln-1)) g
		ln = length xs

choose :: RandomGen g => [a] -> g -> (a, g)
choose xs g = (xs !! fst ng, snd ng)
	where
		ng = randomR (0, (ln-1)) g
		ln = length xs

swap :: [a] -> Int -> Int -> [a]
swap xs i j = zipWith swapper [0..] xs
	where swapper index x
		| index == i = xs !! j
		| index == j = xs !! i
		| otherwise = x

{-
rotate :: [a] -> Int -> [a]
rotate xs n
        | n >= 0 = drop n xs ++ take n xs
        | n <= 0= reverse (drop m (reverse xs) ++ take m (reverse xs))
                where m = abs n

shuffle :: [a] -> [a]
shuffle [] = []
shuffle xs = (head xrs) : (shuffle $ tail xrs)
	where
		xrs = rotate xs $ fst $ randomR (0, (ln-1)) (mkStdGen 0)
		ln = length xs

shuffle' :: RandomGen g => [a] -> g -> ([a], g)
shuffle' [] g = ([], g)
shuffle' xs g = 

head xrs : fst shf, snd shf

[ xr
| xr <- rotate xs $ fst $ ng
]
	where
		shf = shuffle' tail xrs snd ng
		ng = randomR (0, (ln-1)) g

swap xs i j
	| i == j = xs
	| j < i = swap xs j i
	| otherwise = take i xs ++ [xs !! j] ++ (take (j - i - 1) $ drop (i + 1) xs) ++ [xs !! i] ++ drop (j + 1) xs
-}