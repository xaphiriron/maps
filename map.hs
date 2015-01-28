
import Prelude hiding (Either(..))

import Linear.V2
import Linear.Metric
import Linear.Vector ((^*))

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Function (on)
import Data.Graph.Inductive hiding (empty)
import Data.Maybe
import Data.Monoid
import Data.List
import Codec.Picture
import GHC.Word

import Utility.Tuple
import Utility.Rand
import InductiveGraph (randomizeEdges')

import Generation.Pattern (Language(..), Shaped(..), generate)
import Noise.Perlin (perlin, perlin')
import qualified Noise.Value as V
import BSP.BSP
import BSP.Color

import LSystem.Base
import LSystem.Turtle
import LSystem.Systems

import Debug.Trace (trace)

data Map a b = Map
	{ seed :: Int
	, boundary :: b
	, contents :: a
	}
	deriving (Show, Read)

main = do
	world <- generateMap (64, 64)
	writePng "map.png"
		$ generateImage' world 8

generateImage' world scale =
	let
		(x, y) = snd . boundary $ world
		g xi yi = case (xi, yi) of
			(xr, yr)
				-- white border 1px around map frame
				| onBorder (x*scale+3) (y*scale+3) xr yr ->
					PixelRGB8 255 255 255
				-- map info bar
				| yr >= y*scale+3 ->
					PixelRGB8 255 255 255
				-- 1px black map frame around map
				| onBorder (x*scale+1) (y*scale+1) (xr-1) (yr-1) ->
					PixelRGB8 0 0 0
				-- actual map pixels, scaled up
				| inBounds (x*scale) (y*scale) (xr-2) (yr-2) ->
					displayMap world ((xi - 2) `div` scale) ((yi - 2) `div` scale)
			_ -> PixelRGB8 255 0 255
	in
		generateImage g (x * scale + 4) (y * scale + 15)

generateMap :: (Int, Int) -> IO (Map [[Int]] ((Int, Int), (Int, Int)))
generateMap (x, y) = do
	s <- randomIO :: IO Int
	print s
	return . evalRand (mkStdGen s) $ generate (foo s (x, y)) bar

displayMap :: (Map [[Int]] ((Int, Int), (Int, Int))) -> Int -> Int -> PixelRGB8
displayMap (Map s (_, (xb, yb)) cs) x y = if inBounds xb yb x y
	then case cs !! y !! x of
		d | d >= 0 && d <= 255 -> let d' = fromIntegral d in PixelRGB8 d' d' d'
		_ -> PixelRGB8 255 0 255
	else error $ mconcat ["displayMap: out of bound coord (", show x, ", ", show y, ")"]

inBounds :: (Ord a, Num a) => a -> a -> a -> a -> Bool
inBounds xb yb x y = x >= 0 && x < xb && y >= 0 && y < yb

onBorder :: (Ord a, Num a) => a -> a -> a -> a -> Bool
onBorder xb yb x y = x == 0 || x == xb || y == 0 || y == yb

data Pattern = Pattern

foo :: RandomGen g => Int -> (Int, Int) -> Language Pattern (Map [[Int]]) (Rand g) ((Int, Int), (Int, Int))
foo s (x, y) = Language
	{ empty = Map s ((0, 0), (x, y)) $ replicate y $ replicate x 0
	, zero = Pattern
	, pattern = \p m@(Map s l c) -> case p of
		Pattern -> do
			f <- noiseFunction (LocalTransform (\as a ->
				case partition (fromMaybe False . fmap ((== 255))) . concat $ as of
					(_:_, _:_) -> max a 127
					([], _) -> a
					(_, []) -> a)
				$ FlowBasin (0, 0) 4) (snd l)
			return $ (contentsMap (updateCoords (\x y _ -> f x y)) m,
				[])
			{-
			f <- noiseFunction (Fractal $ Value 127 127 8 V.Cosine) (snd l)
			let g = (\(BSPRoom (x, y) w h generation) -> let
					-- sample the center of the room from the noise
					noisePercent = fromIntegral (f (x + (w `div` 2)) (y + (h `div` 2))) / 255
					genPercent = fromIntegral generation / 24
				in genPercent < noisePercent)
			f' <- noiseFunction ((BSP $ BSPControl 4 12 (0.35, 0.65) (Just g))) (snd l)
			return $ (contentsMap (updateCoords (\x y _ -> f' x y)) m,
				[])
			-}
	}

bar :: Shaped (Map a) b
bar = Shaped
	{ extract = \(Map _ l _) -> l
	, set = boundsSet
	}

updateCoords :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
updateCoords f = fmap (fmap (fmap $ uncurry . uncurry $ f))
	$ zipWith (\y xs -> fmap (\(x, d) -> ((x, y), d)) xs) [0..] . fmap (zip [0..])

updateCoordsM :: Monad m => (Int -> Int -> a -> m b) -> [[a]] -> m [[b]]
updateCoordsM f = fmap (sequence . fmap (sequence . (fmap $ uncurry . uncurry $ f)))
	$ zipWith (\y xs -> fmap (\(x, d) -> ((x, y), d)) xs) [0..] . fmap (zip [0..])

boundsMap :: (a -> b) -> Map c a -> Map c b
boundsMap f (Map s a c) = Map s (f a) c

boundsSet :: a -> Map c x -> Map c a
boundsSet a (Map s _ c) = Map s a c

contentsMap :: (a -> b) -> Map a c -> Map b c
contentsMap f (Map s c a) = Map s c (f a)

contentsMapM :: Monad m => (a -> m b) -> Map a c -> m (Map b c)
contentsMapM f (Map s c a) = liftM (Map s c) (f a)




data Gradient = Linear | Radial
	deriving (Eq, Ord, Enum, Bounded)

data Generator
	= Gradient Gradient (V2 Int) (V2 Int)
	| Fractal Fractal
	| TranslateDomain Generator Axis Generator
	| BSP BSPControl
	| FlowBasin (Int, Int) Int
--	| DLA (Int -> Int -> Bool) Integer
	| Turbulence Int Int Generator
	| Select Int Int Int Generator
	| FieldTransform (Int -> Int) Generator
	| LocalTransform ([[Maybe Int]] -> Int -> Int) Generator
	| Contour Int Generator

rescale :: Fractional a => V2 a -> (a, a) -> V2 a
rescale (V2 x y) (xs, ys) = V2 (x / xs) (y / ys)

refit :: (V2 Float -> V2 Float -> V2 Float -> Float) -> V2 Int -> V2 Int -> (Int, Int) -> Int -> Int -> Int -> Int
refit gradient o r size scale x y =
	round . (* fromIntegral scale)
		$ gradient (fit o) (fit r) (fit $ V2 x y)
	where
		bounds = (fromIntegral *** fromIntegral) size
		fit x = rescale (fmap fromIntegral x) bounds

linearGradient :: V2 Float -> V2 Float -> V2 Float -> Float
linearGradient o r p = let
		sqd = (\(V2 x y) -> x * x + y * y) r
	in
		max 0 . min 1
			$ (r `dot` (p - o)) * (1 / sqd)

radialGradient :: V2 Float -> V2 Float -> V2 Float -> Float
radialGradient o r p = max 0 . min 1
	$ 1 - (distance p o / distance 0 (r - o))

(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.) . (.)

noiseFunction :: RandomGen g => Generator -> (Int, Int) -> Rand g (Int -> Int -> Int)
noiseFunction p size@(xSize, ySize) = case p of
	-- FIXME: turbulence is Int -> Int -> Rand g Int so it might take a little more effort to translate (since as opposed to all the other structures, we _don't_ want it to generate randomness once and then use the same value for every coordinate). but Rand g (Int -> Int -> Rand g Int) is such an annoying signature. what we should do is use the size param, so this knows how many random numbers to generate
	Turbulence low high p' -> error "no turbulence" -- (\_ _ f -> turbulence low high f)
	Select low high t p' -> do
		f <- noiseFunction p' size
		return $ select low high t .: f
	FieldTransform g p' -> do
		f <- noiseFunction p' size
		return $ g .: f
	LocalTransform g p' -> do
		f <- noiseFunction p' size
		let pts = uncurry (gridOn f) size
		return $ localMapUpdate' g pts size
	Contour band p' -> do
		f <- noiseFunction p' size
		return $ contour band .: f
	Gradient g o r -> let
			gr = case g of
				Linear -> linearGradient
				Radial -> radialGradient
		in
			return $ refit gr o r size 255
	Fractal frac -> do
		pts <- uncurry (fractal frac) size
		return $ (\x y -> pts !! (max 0 . min ySize $ y) !! (max 0 . min xSize $ x))
	TranslateDomain source axis domain -> do
		sf <- noiseFunction source size
		df <- noiseFunction domain size
		return $ (translateDomain sf (Domain axis df))
	BSP control -> do
		d <- randomDungeon control xSize ySize
		return $ (\x y -> case colorDungeon d x y of
			Nothing -> (-1)
			Just True -> 255
			Just False -> 0)
	FlowBasin outflow scale ->
		flowBasin (max xSize ySize) scale outflow

isSampling :: Generator -> Bool
isSampling p = case p of
	(Gradient _ _ _) -> True
	(Fractal _) -> True
	(BSP _) -> True
	(TranslateDomain _ _ _) -> True
	_ -> False

select :: (Ord a) => a -> a -> a -> a -> a
select low high threshold v = if v < threshold then low else high

turbulence :: (RandomGen g, Num a, Random a) => a -> a -> a -> Rand g a
turbulence low high v = do
	r <- liftRand $ randomR (low, high)
	return $ v + r

contour :: Int -> Int -> Int
contour band v = case min (v `mod` band) ((band - v) `mod` band) of
	0 -> 255
	1 -> 191
	2 -> 127
	3 -> 63
	_ -> 0

data Fractal = Perlin Int | Value Int Int Int V.Interpolation
	deriving (Eq, Ord)

-- "FBM / RidgedMulti / Billow / Multi / HybridMulti"
fractal :: RandomGen g => Fractal -> Int -> Int -> Rand g [[Int]]
fractal f xSize ySize = case f of
	(Perlin octaves) -> do -- this does work. i mean, it doesn't generate perlin noise
		s <- liftRand $ random
		return $ fmap (fmap fromIntegral) $ perlin' s xSize ySize octaves
	(Value freq amp octs int) -> do
		s <- liftRand $ random
		return $ (fmap . fmap) fromIntegral $ (V.valueGrid xSize ySize freq amp octs int s :: [[Word8]])

{-
fractal' :: RandomGen g => Fractal -> Int -> Int -> Rand g [[Int]]
fractal' f xSize ySize = case f of
	(Perlin octaves) -> do
		s <- liftRand $ random
		let u = 1 / s
		replicate ySize (take xSize $ iterate (+ u) 0)
		take ySize $ iterate (+ u) 0
-}

data Axis = X | Y | Z | W
	deriving (Eq, Ord)

data Domain a = Domain Axis (Int -> Int -> a)

-- the idea is we use the values of source, but shimmy their coordinates around in a given dimension by the value returned by whichever domain. hypothetically this could take multiple domains. every function (in the domain or otherwise) needs to take the same number of args
translateDomain :: (Int -> Int -> Int) -> Domain Int -> Int -> Int -> Int
translateDomain source (Domain a domain) x y =
	let d = domain x y
	in source (if a == X then d+x else x) (if a == Y then d+y else y)

diffusionLimitedAggregation :: RandomGen g => (Int -> Int -> Bool) -> Integer -> (Int, Int) -> Rand g [[Integer]]
diffusionLimitedAggregation seed generations (w, h) = do
	undefined
	-- grid w h
{-
data DiffuseAggregate = DiffuseAggregate
	{ size :: (Int, Int)
	, pts :: [(Int, Int)]
	, field :: [Integer]
	}

step :: DiffuseAggregate -> Rand g DiffuseAggregate
step (DiffuseAggregate (w, h) pts field) = do
	-- randomly move every point by -/+ 1
	pts' <- mapM adjust pts
	-- partition points that have moved oob or adjacent to a fixed pt
	let (toRemove, remaining) = partition ((||) <$> oob <*> byGrain) pts'
	-- add new fixed points to the field
	let field' = undefined
	-- add x new points in random positions
	newPts <- replicateM (length toRemove) randomPoint
	return $ DiffuseAggregate (w, h) (remaining <> newPts) field'
	where
		adjust :: (Int, Int) -> Rand g (Int, Int)
		adjust (x, y) = do
			xp <- liftRand $ randomR (-1, 1)
			yp <- liftRand $ randomR (-1, 1)
			return (x+xp, y+yp)
		randomPoint :: Rand g (Int, Int)
		randomPoint =
			(,)
				<$> (liftRand $ randomR (0,w-1))
				<*> liftRand $ randomR (0,h-1)
		oob :: (Int, Int) -> Bool
		oob (x, y) = inBounds w h x y
		byGrain :: (Int, Int) -> Bool
		byGrain = undefined
		insertGrain :: [Integer] -> (Int, Int) -> [Integer]
		insertGrain field (x, y) =
			take (n x y) field <> [???] <> drop ((n x y) + 1) field
			where
				n x y = y * w + x
-}

gridGraph :: DynGraph gr => Int -> Int -> gr (Int, Int) ()
gridGraph w h =
	  uncurry mkGraph
	. (id *** concat)
	. unzip
	. fmap (\(x, y) -> ((n x y, (x, y)), adjs x y))
		$ [(x, y) | x <- [0..(w-1)], y <- [0..(h-1)]]
	where
		n x y = y * w + x
		adjs x y = catMaybes
			[ if x == 0 then Nothing else
				Just (n x y, n (x-1) y, ())
			, if x == (w-1) then Nothing else
				Just (n x y, n (x+1) y, ())
			, if y == 0 then Nothing else
				Just (n x y, n x (y-1), ())
			, if y == (h-1) then Nothing else
				Just (n x y, n x (y+1), ())
			-- can't add diagonals until we can draw them
			, if x == 0 || y == 0 then Nothing else
				Just (n x y, n (x-1) (y-1), ())
			, if x == (w-1) || y == 0 then Nothing else
				Just (n x y, n (x+1) (y-1), ())
			, if x == 0 || y == (h-1) then Nothing else
				Just (n x y, n (x-1) (y+1), ())
			, if x == (w-1) || y == (h-1) then Nothing else
				Just (n x y, n (x+1) (y+1), ())
			]

data GridGraph gr a b = GridGraph
	{ graph :: gr a b
	, size :: Int
	, coord :: (Int, Int) -> Node
	}

flowBasin :: RandomGen g => Int -> Int -> (Int, Int) -> Rand g (Int -> Int -> Int)
flowBasin size scale (bx, by) = do
	field <- randomizeEdges' (8, 32 :: Int) $ nmap (const 1) (gridGraph (size `div` scale) (size `div` scale) :: Gr (Int, Int) ())
	let n = \(x, y) -> y * (size `div` scale) + x
	let field' = setFlowParams field (n (bx `div` scale, by `div` scale))
	return
		$ flowGraph
			(GridGraph field' (size `div` scale) n)
			size

setFlowParams :: (DynGraph gr, Num a, Real b) => gr x b -> Node -> gr (a, Node) b
setFlowParams gr outflow =
	let
		-- lord. msTreeAt seems like it should be the function to use, but that just leads to bizarre winding paths. either i'm not understanding what a minimal spanning tree is or the lib implementation is busted. or both! idk.
		flow = msPath (spTree outflow gr) outflow
		parentNode (n, sp) = case drop 1 . reverse $ sp of
			[] -> (n, n) -- outflow node or we got an incoherent graph
			(p:_) -> (n, p)
		(parents, paths) =
			-- we're sorting by distance-to-outflow, b/c when we fold over this list we want to calculate every far point before any point closer on a path (and foldr runs last-item-first)
			  second (sortBy (compare `on` length . flow . fst) . collate)
			. unzip
			. fmap (parentNode &&& (\(n, p) -> (p, [n])) . parentNode)
				$ ((id &&& flow) . fst) <$> labNodes gr
		-- this won't contain leaves of the flow tree, but those will all be 1 anyway
		flowVals = foldr foo [] paths
		foo :: Num a => (Node, [Node]) -> [(Node, a)] -> [(Node, a)]
		foo (p, ns) fs =
			let x = sum . fmap (fromMaybe 1 . (`lookup` fs)) $ ns
			in (p, x + 1) : fs
	in
		gmap (\(ie, n, _, oe) -> case (lookup n flowVals, lookup n parents) of
			(Just x, Just p) -> (ie, n, (x, p), oe)
			(Nothing, Just p) -> (ie, n, (1, p), oe)
			_ -> error "incoherent flow graph ?!") gr

flowGraph :: (DynGraph gr, Real b, Integral a) => GridGraph gr (a, Node) b -> Int -> Int -> Int -> Int
flowGraph (GridGraph field gsize n) size x y =
	let
		cellSize = fromIntegral size / fromIntegral gsize
		halfCell = cellSize / 2
		xg = (fromIntegral x - halfCell) / cellSize
		yg = (fromIntegral y - halfCell) / cellSize
		xi = floor xg
		yi = floor yg
		c = V2 xg yg
		vs =
			(\xy -> case lab field (n xy) of
				Nothing -> Nothing
				Just (fl, pn) -> Just (n xy, (pn, xy, fl))
			) `mapMaybe` filter (uncurry (inBounds gsize gsize)) ((,) <$> [xi-1, xi, xi+1, xi+2] <*> [yi-1, yi, yi+1, yi+2])
		vs' = (\(n, (pn, xy, fl)) -> case lookup pn vs of
			Nothing -> Nothing
			Just (_, pxy, _) -> Just (xy, pxy, fl)) `mapMaybe` vs
	in
		  floor
		. (* 255)
		. maximum
			$ uncurry3 (\(nx, ny) (px, py) flowVolume ->
				let
					d = distanceToSegment (fromIntegral <$> V2 nx ny) (fromIntegral <$> V2 (px - nx) (py - ny)) c
					flowPercent = sqrt (fromIntegral flowVolume) / fromIntegral gsize
				in if d <= flowPercent * 0.3 then (if flowPercent < 0.15 then 0.5 else 1) else 0)
				<$> vs'

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- merge a list of (value, item) together, adding `item`s together until `value` is unique
collate :: (Ord a, Monoid n) => [(a, n)] -> [(a, n)]
collate =
	  fmap flatten
	. groupBy ((==) `on` fst)
	. sortBy (compare `on` fst)
	where
		flatten :: Monoid n => [(a, n)] -> (a, n)
		flatten ais = case ais of
			(a,_):_ -> (a, mconcat is)
			_ -> error "flatten given empty list"
			where
				(_, is) = unzip ais

-- returns the point on the line o/r that's nearest to p, in units of length r. negative if it's before o; between 0..1 if it's within the span of r
ptOnLine :: (Floating a, Eq a) => V2 a -> V2 a -> V2 a -> a
ptOnLine o r p =
	let
		sqd = (\(V2 x y) -> x * x + y * y) r
	in (r `dot` (p - o)) * (1 / sqd)

-- returns (distance from point to line, distance along line in units of r)
distanceToLine :: (Floating a, Eq a) => V2 a -> V2 a -> V2 a -> (a, a)
distanceToLine o r p =
	let
		sqd = (\(V2 x y) -> x * x + y * y) r
		t = (r `dot` (p - o)) * (1 / sqd)
		d = distance (o + (r ^* t)) p
	in (d , t)

distanceToSegment ::  (Floating a, Ord a) => V2 a -> V2 a -> V2 a -> a
distanceToSegment o r p =
	let (d, t) = distanceToLine o r p
	in case t of
		t'
			| t' > 1 -> distance (o+r) p
			| t' < 0 -> distance o p
			| otherwise -> d

localMapUpdate' :: ([[Maybe a]] -> a -> b) -> [[a]] -> (Int, Int) -> (Int -> Int -> b)
localMapUpdate' f ts bounds x y =
	  (!! x) . (!! y)
	. contents . localMapUpdate f
		$ Map 0 (0, bounds) ts

localMapUpdate :: ([[Maybe a]] -> a -> b) -> Map [[a]] (c, (Int, Int)) -> Map [[b]] (c, (Int, Int))
localMapUpdate f (Map seed bounds@(_, (w, h)) ts) = Map seed bounds $ fmap (fmap g) mis
	where
		g (x, y) = f
			(fmap (around x 1) . around y 1 $ mts)
			(ts !! (y-1) !! (x-1))
		mis :: [[(Int, Int)]] -- of indices centered on a Just value 
		mis = fmap (fmap $ (+1) *** (+1)) $ grid w h
		-- mts :: [[Maybe a]]
		mts =
			  cap (replicate (w + 2) Nothing)
			. fmap (cap Nothing . fmap Just)
				$ ts
		cap :: a -> [a] -> [a]
		cap e = (e :) . (++ [e])

--fmap (around x 1) . around y 1 $ mts
{-
around :: Int -> Int -> [a] -> [a]
around i spread as = take (spread * 2 + 1) . drop (i - (spread + 1)) $ as
-}

around :: Int -> Int -> [a] -> [a]
around i spread as = take (spread * 2 + 1) . drop (i - spread) $ as

-- | this just exists to generate a 2d array w/ coordinates attached.
grid :: Int -> Int -> [[(Int, Int)]]
grid x y =
	  transpose
	. zipWith (\y' xs -> fmap (\(x') -> (x', y')) xs) [0..(x-1)]
	. repeat
		$ [0..(y-1)]

gridOn :: (Int -> Int -> a) -> Int -> Int -> [[a]]
gridOn f x y = (fmap . fmap) (uncurry f) (grid x y)

{-
graphemes =
	[ ('-'
		[ [0, 0, 0, 0, 0, 0, 0]
		, [0, 0, 0, 0, 0, 0, 0]
		, [0, 0, 0, 0, 0, 0, 0]
		, [0, 0, 0, 0, 0, 0, 0]
		, [2, 2, 2, 2, 2, 2, 2]
		, [0, 0, 0, 1, 2, 2, 2]
		, [0, 0, 0, 0, 0, 0, 0]
		, [0, 0, 0, 0, 0, 0, 0]
		, [0, 0, 0, 0, 0, 0, 0]
		])
	, ('0',
		[ [1, 2, 2, 2, 2, 2, 1]
		, [2, 2, 0, 0, 0, 2, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 2, 0, 0, 0, 2, 2]
		, [1, 2, 2, 2, 2, 2, 1]
		])
	, ('1',
		[ [0, 0, 2, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 1, 0, 0]
		, [0, 0, 2, 2, 2, 2, 0]
		, [0, 0, 2, 2, 2, 2, 0]
		])
	, ('2',
		[ [2, 2, 2, 2, 2, 2, 1]
		, [0, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 0, 0, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		, [1, 2, 2, 2, 2, 2, 1]
		, [2, 2, 0, 0, 0, 0, 0]
		, [2, 0, 0, 0, 0, 0, 0]
		, [2, 2, 0, 0, 0, 0, 0]
		, [1, 2, 2, 2, 2, 2, 2]
		])
	, ('3',
		[ [2, 2, 2, 2, 2, 2, 0]
		, [0, 0, 0, 0, 1, 2, 0]
		, [0, 0, 0, 0, 0, 2, 0]
		, [0, 0, 0, 0, 1, 2, 0]
		, [2, 2, 2, 2, 2, 2, 1]
		, [0, 0, 0, 0, 1, 2, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 1, 2, 2]
		, [2, 2, 2, 2, 2, 2, 2]
		])
	, ('4',
		[ [2, 0, 0, 0, 0, 0, 0]
		, [2, 0, 0, 0, 0, 0, 0]
		, [2, 0, 0, 0, 0, 0, 0]
		, [2, 0, 0, 2, 1, 0, 0]
		, [2, 2, 2, 2, 2, 0, 0]
		, [0, 0, 1, 2, 2, 0, 0]
		, [0, 0, 0, 2, 2, 0, 0]
		, [0, 0, 0, 2, 2, 0, 0]
		, [0, 0, 0, 2, 2, 0, 0]
		])
	, ('5',
		[ [1, 2, 2, 2, 2, 2, 2]
		, [2, 2, 0, 0, 0, 0, 0]
		, [2, 0, 0, 0, 0, 0, 0]
		, [2, 2, 0, 0, 0, 0, 0]
		, [1, 2, 2, 2, 2, 2, 1]
		, [0, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 0, 0, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		, [2, 2, 2, 2, 2, 2, 1]
		])
	, ('6',
		[ [2, 2, 2, 2, 0, 0, 0]
		, [2, 1, 0, 2, 0, 0, 0]
		, [2, 0, 0, 0, 0, 0, 0]
		, [2, 0, 0, 0, 0, 0, 0]
		, [2, 2, 2, 2, 2, 2, 1]
		, [2, 0, 0, 0, 0, 2, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 2, 0, 0, 0, 2, 2]
		, [1, 2, 2, 2, 2, 2, 1]
		])
	, ('7',
		[ [2, 2, 2, 2, 2, 2, 1]
		, [2, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 0, 0, 2]
		, [0, 0, 0, 0, 1, 2, 2]
		, [0, 0, 0, 2, 2, 1, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		, [0, 0, 0, 2, 0, 0, 0]
		])
	, ('8',
		[ [0, 2, 2, 2, 2, 2, 0]
		, [0, 2, 0, 0, 0, 2, 0]
		, [0, 2, 0, 0, 0, 2, 0]
		, [0, 2, 0, 0, 0, 2, 0]
		, [1, 2, 2, 2, 2, 2, 1]
		, [2, 2, 0, 0, 0, 2, 2]
		, [2, 2, 0, 0, 0, 2, 2]
		, [2, 2, 0, 0, 0, 2, 2]
		, [2, 2, 2, 2, 2, 2, 2]
		])
	, ('9',
		[ [1, 2, 2, 2, 2, 2, 2]
		, [2, 2, 0, 0, 0, 0, 2]
		, [2, 0, 0, 0, 0, 0, 2]
		, [2, 2, 0, 0, 0, 1, 2]
		, [1, 2, 2, 2, 2, 2, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		, [0, 0, 0, 0, 0, 2, 2]
		])
	]
-}