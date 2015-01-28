module Noise.Perlin
	( perlin
	, point
	, pointNormalized
	, perlin'
	) where

import Control.Applicative
import Data.ByteString hiding (minimum, maximum, transpose, zipWith)
import Data.List (transpose)
import Data.Monoid
import Data.Word

import Linear.V2
import Linear.Metric (dot, normalize)

type Width = Int
type Height = Int

-- | this exists for code that generates images or w/e by use of a @Int -> Int -> Word8@ function
pointNormalized :: Int -> Width -> Height -> Int -> Int -> Int -> Word8
pointNormalized seed x y sizeX sizeY octaves = perlin seed sizeX sizeY octaves !! y !! x

-- | the only thing of note here is that we normalize the bits after calculating them all, so the darkest section of the map is always 0 and the brightest is always 255. i don't actually know if this is 100% accurate.
perlin :: Int -> Int -> Int -> Int -> [[Word8]]
perlin seed sizeX sizeY octaves = normalizedBits
	where
		normalizedBits =
			fmap (fmap (\a -> floor $ fromIntegral (a - smallest) / fromIntegral (largest - smallest) * 255)) rawBits
		smallest = minimum . mconcat $ rawBits
		largest = maximum . mconcat $ rawBits
		rawBits :: [[Word8]]
		rawBits = fmap (fmap (\(x, y) -> point seed x y (if sizeX < sizeY then sizeY else sizeX) 128 octaves)) empty
		empty :: [[(Int, Int)]]
		empty = grid sizeX sizeY

point :: Int -> Width -> Height -> Int -> Int -> Int -> Word8
{- | any of the following can stop recursion:
 * running out of points to sample (size hits 0)
 * running out of visible octaves (frequency hits zero)
 * running out of user-specified octaves to generate (octaves hits 0)
-}
point _ _ _ 0 _ _ = 0
point _ _ _ _ 0 _ = 0
point _ _ _ _ _ 0 = 0
-- | WHERE THE MAGIC HAPPENS
point seed x y size frequency octaves =
	octave + point seed x y (size `div` 2) (frequency `div` 2) (octaves - 1)
	where
		interpolate = cosine
		octave = floor $ fromIntegral frequency * interpolate
			(interpolate bottomLeft topLeft yPercent)
			(interpolate bottomRight topRight yPercent)
			xPercent
		-- | that's the quotient & remainder, since e.g., if your sampling size is "every 64 pixels" and you have a point at 213,31, then you want to sample at 3,0 / 3,1 / 4,0 / 4,1, and your percentage values for interpolation are 21/64 x and 31/64 y
		(xCoord, xOffset) = quotRem x size
		(yCoord, yOffset) = quotRem y size
		xPercent = fromIntegral xOffset / fromIntegral size
		yPercent = fromIntegral yOffset / fromIntegral size
		topLeft = noise seed xCoord (yCoord + 1) size
		topRight = noise seed (xCoord + 1) (yCoord + 1) size
		bottomLeft = noise seed xCoord yCoord size
		bottomRight = noise seed (xCoord + 1) yCoord size

point' :: Int -> Width -> Height -> Int -> Int -> Int -> Word8
{- | any of the following can stop recursion:
 * running out of points to sample (size hits 0)
 * running out of visible octaves (frequency hits zero)
 * running out of user-specified octaves to generate (octaves hits 0)
-}
point' _ _ _ 0 _ _ = 0
point' _ _ _ _ 0 _ = 0
point' _ _ _ _ _ 0 = 0
-- | WHERE THE MAGIC HAPPENS
point' seed x y size frequency octaves =
	octave + point' seed x y (size `div` 2) (frequency `div` 2) (octaves - 1)
	where
		interpolate = cosine
		octave = floor $ fromIntegral frequency * interpolate
			(interpolate bottomLeft topLeft yPercent)
			(interpolate bottomRight topRight yPercent)
			xPercent
		-- | that's the quotient & remainder, since e.g., if your sampling size is "every 64 pixels" and you have a point at 213,31, then you want to sample at 3,0 / 3,1 / 4,0 / 4,1, and your percentage values for interpolation are 21/64 x and 31/64 y
		(xCoord, xOffset) = quotRem x size
		(yCoord, yOffset) = quotRem y size
		displace = normalize $ fromIntegral <$> V2 xOffset yOffset :: V2 Float
		xPercent = fromIntegral xOffset / fromIntegral size
		yPercent = fromIntegral yOffset / fromIntegral size
		topLeft = displace `dot` noise' seed xCoord (yCoord + 1)
		topRight = displace `dot` noise' seed (xCoord + 1) (yCoord + 1)
		bottomLeft = displace `dot` noise' seed xCoord yCoord
		bottomRight = displace `dot` noise' seed (xCoord + 1) yCoord


dotGridGradient :: Int -> Float -> Float -> Float -> Float -> Float
dotGridGradient seed ix iy x y =
	let
		d = V2 (x - ix) (y - iy)
		g = noise' seed (floor ix) (floor iy)
	in d `dot` g

perlin' :: Int -> Int -> Int -> Int -> [[Word8]]
perlin' seed sizeX sizeY octaves = normalizedBits
	where
		normalizedBits =
			fmap (fmap (\a -> floor $ fromIntegral (a - smallest) / fromIntegral (largest - smallest) * 255)) rawBits
		smallest = minimum . mconcat $ rawBits
		largest = maximum . mconcat $ rawBits
		rawBits :: [[Word8]]
		rawBits = fmap (fmap (\(x, y) -> point' seed x y (if sizeX < sizeY then sizeY else sizeX) 128 octaves)) empty
		empty :: [[(Int, Int)]]
		empty = grid sizeX sizeY

-- | noise function. stateless.
noise' :: Int -> Int -> Int -> V2 Float
noise' seed x y =
	normalize $ fmap (fromIntegral) $ V2
		((seed + 271 * x + 29847) `mod` 256)
		((seed + 271 * y + 28273) `mod` 256)

-- | noise function. stateless.
noise :: Int -> Int -> Int -> Int -> Float
noise seed x y octave =
	fromIntegral ((seed + 3) * (x + 271) * (y + 29847) * (octave + 28273) `mod` 65536) / 65536

-- | interpolate linearly between a and b based on the value of p (a at 0, b at 1)
linear :: Float -> Float -> Float -> Float
linear a b p = (a * (1 - p)) + (b * p)

-- | see above, only on a curve
cosine :: Float -> Float -> Float -> Float
cosine a b p =
	let sp = (1 - cos (pi * p)) / 2
	in (a * (1 - sp)) + (b * sp)

-- | this just exists to generate a 2d array w/ coordinates attached.
grid :: Int -> Int -> [[(Int, Int)]]
grid x y =
	  transpose
	. zipWith (\y' xs -> fmap (\(x') -> (x', y')) xs) [0..(x-1)]
	. repeat
		$ [0..(y-1)]
