module BSP.BSP
	( BSPControl(..)
	, BSPDungeon(..)
	, BSPRoom(..)
	, Split(..)
	, randomDungeon
	, defaultControl
	, module X
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid

import Data.Graph.Inductive
import InductiveGraph (randomizeEdges)

import Linear.V2 as X
import BSP.Rect as X
import Data.BTree

import Utility.Rand


data BSPDungeon = BSPDungeon Int Int (BTree Split)
	deriving (Show, Read)

data Split = Vertical Int | Horizontal Int
	deriving (Show, Read)

-- this is an internal thing used only during generation (and passed to the control flow parameter)
data BSPRoom = BSPRoom (Int, Int) Int Int Integer
	deriving (Show, Read)

data BSPControl = BSPControl
	{ minSize :: Int
	, maxGeneration :: Integer
	, splitRange :: (Float, Float)
	, divideControl :: Maybe (BSPRoom -> Bool)
	}

defaultControl :: BSPControl
defaultControl = BSPControl 4 6 (0.35, 0.65) Nothing

{-
-- | generate a random BSP dungeon graph
bspRooms :: (DynGraph gr, RandomGen g) => BSPControl -> Int -> Int -> Rand g (gr Room Int)
bspRooms ctrl w h =
	    liftRand . randomizeEdges (5, 20)
	=<< return . nmap (\r -> Room r []) . dungeonGraph
	=<< liftRand (randomDungeon ctrl w h)

-- | same as above, only with the outer rooms removed to give the map a more irregular outline
bspRoomsRounded :: (DynGraph gr, RandomGen g) => BSPControl -> Int -> Int -> Rand g (gr Room Int)
bspRoomsRounded ctrl w h = do
	gr <- bspRooms ctrl w h
	let edges = fmap fst . filter (\(n, l) -> edgeRoom w h l) . labNodes $ gr
	let gr' = delNodes edges gr
	let (minX, minY) = fromMaybe (0, 0) . ufold (labelIntoContext findMins) Nothing $ gr'
	return . nmap (shiftBack (minX - 1) (minY - 1)) $ gr'
	where
		findMins :: Room -> Maybe (Int, Int) -> Maybe (Int, Int)
		findMins (Room (Rect (V2 x y) _) _) Nothing = Just (x, y)
		findMins (Room (Rect (V2 x y) _) _) (Just (mx, my)) =
			Just (min x mx, min y my)
		shiftBack :: Int -> Int -> Room -> Room
		shiftBack x y (Room (Rect (V2 px py) a) b) =
			Room (Rect (V2 (px - x) (py - x)) a) b

labelIntoContext :: (a -> b) -> Context a c -> b
labelIntoContext f (_, _, a, _) = f a

edgeRoom :: Int -> Int -> Room -> Bool
edgeRoom mw mh (Room (Rect (V2 x y) (V2 w h)) _)
	| x == 0 = True
	| y == 0 = True
	| x + w + 1 == mw = True
	| y + h + 1 == mh = True
	| otherwise = False
-}

-- because of the way rooms are overlapped, a BSP with the technical size of w h actually occupies w+1 * h+1 tiles. so to prevent confusion, we shrink the map by one
randomDungeon :: RandomGen g => BSPControl -> Int -> Int -> Rand g BSPDungeon
randomDungeon control w h =
	let
		w' = w-1
		h' = h-1
	in
		  liftM (BSPDungeon w' h')
		. unfoldTreeM (divider control)
			$ BSPRoom (0,0) w' h' 0

divider :: RandomGen g => BSPControl -> BSPRoom -> Rand g (Split, (Maybe BSPRoom, Maybe BSPRoom))
divider (BSPControl minSize maxGeneration (loR, hiR) mdivControl) (BSPRoom (x,y) w h gs) = do
	split <- case (w <= minSize * 2, h <= minSize * 2) of
		(True, True) -> error "given too-small room to split"
		(True, False) -> rHoriz
		(False, True) -> rVert
		-- prefer splitting against the longer axis
		_ -> if w < h
			then rHoriz
			else rVert
	return (split, (maybeSplit *** maybeSplit) $ halves (x,y) w h gs split)
	where
		usingRange x = liftRand $ randomR
			( ceiling $ loR * fromIntegral x
			, floor $ hiR * fromIntegral x
			)
		rHoriz = liftM Horizontal $ usingRange h
		rVert = liftM Vertical $ usingRange w
		maybeSplit x = if splittable x
			then Just x
			else Nothing
		splittable (room@(BSPRoom (x,y) w h gs))
			| (w <= minSize * 2 && h <= minSize * 2) || gs > maxGeneration = False
			| otherwise = fromMaybe True $ mdivControl <*> pure room

halves :: (Int, Int) -> Int -> Int -> Integer -> Split -> (BSPRoom, BSPRoom)
halves (x, y) w h generations (Horizontal i) =
	( BSPRoom (x,y) w i (generations + 1)
	, BSPRoom (x,y+i) w (h - i) (generations + 1)
	)
halves (x, y) w h generations (Vertical i) =
	( BSPRoom (x,y) i h (generations + 1)
	, BSPRoom (x+i,y) (w - i) h (generations + 1)
	)

dungeonGraph :: Graph gr =>
	   BSPDungeon
	-> gr Rect Int -- ^ Int is path weight (it's always 1)
dungeonGraph d =
	uncurry mkGraph . (concat *** concat) . unzip $ graphData <$> rooms
	where
		rooms = zip (dungeonRooms d) [0..]
		graphData :: (Rect, Int) -> ([LNode Rect], [LEdge Int])
		graphData (r, i) =
			( pure (i, r)
			, map (\e -> (i, e, 1))
			. mapMaybe validEdge
			. filter ((/= r) . fst)
				$ rooms
			)
			where
				{- we ignore intersections smaller than 1x3/3x1. let this diagram illuminate you as to why (relatedly, this is part of why 3 should be the absolute minimum room size, since otherwise it's a room w/ no interior):
				#####      %%%%%
				#...#      %%%%%
				#...#####  %%%%*&&&&
				#####...#  %%%%*&&&&
				    #...#      &&&&&
				    #####      &&&&&
				-}
				validEdge :: (Rect, Int) -> Maybe Int
				validEdge (a, i) = case rectOverlap r a of
					Nothing -> Nothing
					Just (Rect _ (V2 w h))
						| w < 2 && h < 2 -> Nothing
					Just _ -> Just i

-- | every room in the dungeon, inc. shared walls (which we will then use to calculate connections)
dungeonRooms :: BSPDungeon -> [Rect]
dungeonRooms (BSPDungeon w h splits) =
	traverse divide (Rect (V2 0 0) (V2 w h)) splits
	where
		traverse :: (a -> b -> (a,a)) -> a -> BTree b -> [a]
		traverse _ zero (Leaf) = [zero]
		traverse tf zero (BNode b l r) =
			let (lzero, rzero) = tf zero b
			in traverse tf lzero l <> traverse tf rzero r
		divide :: Rect -> Split -> (Rect, Rect)
		divide (Rect (V2 x y) (V2 w h)) (Vertical i) =
			( Rect (V2 x y) (V2 i h)
			, Rect (V2 (x + i) y) (V2 (w - i) h)
			)
		divide (Rect (V2 x y) (V2 w h)) (Horizontal i) =
			( Rect (V2 x y) (V2 w i)
			, Rect (V2 x (y + i)) (V2 w (h - i))
			)
