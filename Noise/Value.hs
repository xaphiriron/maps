module Noise.Value
	( Interpolation(..)
	, value
	, valueGrid
	) where

import GHC.Word
import Data.List
import Utility.Rand
import Data.Bits

-- TODO: add cubic interpolation. so far it seems to take a billion (12, a 300% increase) more noise samples and complicate the flow immensely, b/c `cubic` takes five args and so far we've been assuming `interpolate` will just take three.
data Interpolation = Nearest | Linear | Cosine -- | Cubic | Quantic
	deriving (Eq, Ord, Enum, Bounded)

valueGrid :: Int -> Int -> Int -> Int -> Int -> Interpolation -> Int -> [[Word8]]
valueGrid sizeX sizeY frequency amplitude octaves interpolation seed = rawBits
	where
		--rawBits :: [[Word8]]
		rawBits = (fmap . fmap) (\(x, y) -> value frequency amplitude octaves interpolation seed x y) empty
		empty :: [[(Int, Int)]]
		empty = grid sizeX sizeY

{-
assuming we go until we run out of amplitude, the max value would be
	sum $ take octaves $ iterate (`div` 2) amplitude
(but we might run out of frequency or octaves first, and it's unclear if i'd want to scale things in any case)
-}
{- | any of the following can stop recursion:
 * running out of points to sample (frequency hits zero)
 * running out of visible octaves (amplitude hits zero)
 * running out of user-specified octaves to generate (octaves hits zero)
-}
value :: Int -> Int -> Int -> Interpolation -> Int -> Int -> Int -> Word8
value 0 _ _ _ _ _ _ = 0
value _ 0 _ _ _ _ _ = 0
value _ _ 0 _ _ _ _ = 0
-- | WHERE THE MAGIC HAPPENS
value frequency amplitude octaves interpolation seed x y =
	octave + value (frequency `div` 2) (amplitude `div` 2) (octaves - 1) interpolation seed x y
	where
		interpolate = case interpolation of
			Nearest -> nearest
			Linear -> linear
			Cosine -> cosine
		octave = floor $ fromIntegral amplitude * interpolate
			(interpolate
				bottomLeft topLeft yPercent)
			(interpolate
				bottomRight topRight yPercent)
			xPercent
		-- | that's the quotient & remainder, since e.g., if your sampling size is "every 64 pixels" and you have a point at 213,31, then you want to sample at 3,0 / 3,1 / 4,0 / 4,1, and your percentage values for interpolation are 21/64 x and 31/64 y
		(xCoord, xOffset) = quotRem x frequency
		(yCoord, yOffset) = quotRem y frequency
		xPercent = fromIntegral xOffset / fromIntegral frequency
		yPercent = fromIntegral yOffset / fromIntegral frequency
		topLeft = noise seed xCoord (yCoord + 1) octaves
		topRight = noise seed (xCoord + 1) (yCoord + 1) octaves
		bottomLeft = noise seed xCoord yCoord octaves
		bottomRight = noise seed (xCoord + 1) yCoord octaves

nearest :: Float -> Float -> Float -> Float
nearest a b p = if p < 0.5 then a else b

-- | interpolate linearly between a and b based on the value of p (a at 0, b at 1)
linear :: Float -> Float -> Float -> Float
linear a b p = (a * (1 - p)) + (b * p)

-- | see above, only on a curve
cosine :: Float -> Float -> Float -> Float
cosine a b p =
	let sp = (1 - cos (pi * p)) / 2
	in (a * (1 - sp)) + (b * sp)

cubic :: Float -> Float -> Float -> Float -> Float -> Float
cubic ax bx a b p = let
		p2 = p*p
		a0 = bx - b - ax + a
		a1 = ax - a - a0
		a2 = b - ax
		a3 = a
	in a0*p*p2 + a1*p2 + a2*p + a3

-- | this just exists to generate a 2d array w/ coordinates attached.
grid :: Int -> Int -> [[(Int, Int)]]
grid x y =
	  transpose
	. zipWith (\y' xs -> fmap (\(x') -> (x', y')) xs) [0..(x-1)]
	. repeat
		$ [0..(y-1)]

-- THIS IS SUFFICIENTLY RANDOM
noise :: Int -> Int -> Int -> Int -> Float
noise seed x y octave =
	(/ 4294967295) . fromIntegral . hash
		$ merge
			(fromIntegral seed)
			(fromIntegral $ x + 6711713)
			(fromIntegral $ y + 3041789)
			(fromIntegral $ octave + 29847)

merge :: Word16 -> Word16 -> Word16 -> Word16 -> Word32
merge a b c d = (a' `xor` b') + ((c' `xor` d') `shiftL` 16)
	where
		a' = fromIntegral a
		b' = fromIntegral b
		c' = fromIntegral c
		d' = fromIntegral d

hash :: (Num a, Bits a) => a -> a
hash a =
	foldr ($) a $ reverse
		[ (\x -> (x `xor` 61) `xor` (x `shiftR` 16))
		, (\x -> x + (x `shiftL` 3))
		, (\x -> x `xor` (x `shiftR` 4))
		, (\x -> x * 668265261)
		, (\x -> x `xor` (x `shiftR` 15))
		]
